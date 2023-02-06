module Types.Infer where

import PPrint
import Types.Data
import Result
import Control.Monad (foldM)
import Types.Subst
import Data.Maybe (isJust, catMaybes)
import Data.List (nub, find)

class Instantiate t where
    instantiate :: InferenceContext -> t -> (InferenceContext, t)

instance Instantiate Scheme where
    instantiate = loop []
        where loop us' ic (Scheme [] qt) = (ic, Scheme us' qt)
              loop us' ic (Scheme (u:us) qt) = let (ic', u') = allocVar ic in
                                                   loop (u':us') ic' (Scheme us (apply (u +-> TVar u') qt))

-- TODO(alnyan): supertraits
data TypeContext = TypeContext { functions :: [(String, Scheme)]
                               , impls :: [Constraint] }
    deriving Show

emptyTypeContext :: TypeContext
emptyTypeContext = addIntegerImpls TypeContext { functions = [], impls = [] }


addIntegerImpl :: Type -> TypeContext -> TypeContext
addIntegerImpl t = addImpl (Implements t (TConst "Integer")) .
                   addImpl (Implements t (TConst "PartialEq")) .
                   addImpl (Implements t (TConst "Eq")) .
                   addImpl (Implements t (TConst "PartialOrd")) .
                   addImpl (Implements t (TConst "Ord"))

addIntegerImpls :: TypeContext -> TypeContext
addIntegerImpls = addIntegerImpl tI64 .
                  addIntegerImpl tI32

addPreludeImpls :: TypeContext -> TypeContext
addPreludeImpls = addIntegerImpls

addFunctionScheme :: TypeContext -> String -> Scheme -> Result TypeError TypeContext
addFunctionScheme tc name scheme | isJust (lookup name fs) = undefined
                                 | otherwise = Ok $ tc{ functions = fs ++ [(name, scheme)] }
                                where fs = functions tc

addImpl :: Constraint -> TypeContext -> TypeContext
addImpl p tc = tc { impls = p:impls tc }

getFunctionScheme :: TypeContext -> String -> Result TypeError Scheme
getFunctionScheme tc name = okOr f $ UndefinedFunction name
    where f = lookup name (functions tc)

checkConstraint :: TypeContext -> Constraint -> Result TypeError ()
checkConstraint tc p | p `elem` impls tc = Ok ()
                     | otherwise = error $ "Unsatisfied constraint: " ++ pprint p

data InferenceContext = InferenceContext { tcx :: TypeContext,
                                           vars :: [(String, Type)],
                                           lastVarId :: Int,
                                           cs :: [Constraint],
                                           subst :: Subst }
    deriving Show

emptyInferContext tc = InferenceContext { tcx = tc,
                                          lastVarId = 0,
                                          cs = [],
                                          vars = [],
                                          subst = nullSubst }

addVariable :: InferenceContext -> String -> Type -> Result TypeError InferenceContext
addVariable ic name ty | isJust (lookup name vs) = undefined
                       | otherwise = Ok $ ic{ vars = vs ++ [(name, ty)] }
                       where vs = vars ic

addConstraint :: InferenceContext -> Constraint -> InferenceContext
addConstraint ic q | q `elem` cs ic = ic
                   | otherwise = ic { cs = q:cs ic }

findFunction :: InferenceContext -> String -> Result TypeError Scheme
findFunction ic = getFunctionScheme (tcx ic)

findVariable :: InferenceContext -> String -> Result TypeError Type
findVariable ic name = okOr v $ UndefinedVariable name
    where v = lookup name (vars ic)

allocVar :: InferenceContext -> (InferenceContext, TypeVar)
allocVar ic = (ic { lastVarId = vi }, "v" ++ show vi)
    where vi = lastVarId ic + 1

replaceVar' :: TypeVar -> TypeVar -> Constraint -> Constraint
replaceVar' u u' (Implements (TVar u'') t) | u == u'' = Implements (TVar u') t
replaceVar' _ _ c = c

replaceVar :: InferenceContext -> TypeVar -> TypeVar -> InferenceContext
replaceVar ic u u' = ic { cs = nub (map (replaceVar' u u') (cs ic)), subst = subst ic @@ (u +-> TVar u') }

removeVar :: InferenceContext -> TypeVar -> InferenceContext
removeVar ic u = ic { cs = filter ((== TVar u) . lhs) (cs ic) }

foldCtx :: (c -> t -> Result e (c, u)) -> c -> [t] -> Result e (c, [u])
foldCtx = loop []
    where loop ys _ tc [] = Ok (tc, ys)
          loop ys f tc (x:xs) = do (tc', y) <- f tc x
                                   loop (ys ++ [y]) f tc' xs

foldCtx' :: (c -> t -> c) -> c -> [t] -> c
foldCtx' = loop
    where loop _ tc [] = tc
          loop f tc (x:xs) = loop f (f tc x) xs

constraints :: InferenceContext -> TypeVar -> [Constraint]
constraints ic u = filter ((== TVar u) . lhs) (cs ic)

unconstrain :: InferenceContext -> TypeVar -> Type -> Result TypeError InferenceContext
unconstrain ic u t = do let ic' = ic { subst = subst ic @@ (u +-> t) }
                        let ps = apply (subst ic') (constraints ic' u)
                        case find isErr $ map (checkConstraint (tcx ic')) ps of
                          Just x -> error $ "Constraint check failed: " ++ show x
                          Nothing -> pure $ removeVar ic' u

equateInner :: InferenceContext -> TaggedExpr -> Qualified Type -> Result TypeError (InferenceContext, TaggedExpr)
equateInner ic (TVar u, x) (ps :=> TVar u') = do let ic' = replaceVar ic u u'
                                                 pure (foldCtx' addConstraint ic' ps, (TVar u', x))
equateInner ic (TVar u, x) ([] :=> t) = do ic' <- unconstrain ic u t
                                           pure (ic', (apply (subst ic') t, x))
equateInner ic (TConst tc1, x) ([] :=> TConst tc2) | tc1 == tc2 = Ok (ic, (TConst tc1, x))
equateInner _ _ _ = undefined

equate :: InferenceContext -> TaggedExpr -> Qualified Type -> Result TypeError (InferenceContext, TaggedExpr)
equate ic (xt, x) qt = equateInner ic (apply s xt, x) (apply s qt)
    where s = subst ic

qualify :: [Constraint] -> Type -> Qualified Type
qualify ps (TVar u) = filter ((== TVar u) . lhs) ps :=> TVar u
qualify _ t = [] :=> t

splitFunc :: Type -> Result TypeError ([Type], Type)
splitFunc (TFunction xs t) = Ok (xs, t)
splitFunc s = error $ "Not a function: " ++ pprint s

inferCall :: InferenceContext -> Scheme -> [TaggedExpr] -> Result TypeError (InferenceContext, [TaggedExpr], Type)
inferCall ic fsch xs = do -- Instantiate a fresh call scheme fsch
                          let (ic', Scheme _ (fps :=> fqt)) = instantiate ic fsch
                          (fts, ft) <- splitFunc fqt
                          if length fts /= length xs then
                              error "TODO: args mismatch"
                          else do
                              let qargs = map (qualify fps) fts
                              (ic'', xs') <- foldCtx (uncurry . equate) ic' (zip xs qargs)
                              pure (ic'', xs', apply (subst ic') ft)

-- Inference
inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr ic (EBlock []) = Ok (ic, (tUnit, TEBlock []))
inferExpr ic (EBlock xs) = do let (xs', x) = (init xs, last xs)
                              (ic', xs'') <- foldCtx inferExpr ic xs'
                              (ic'', xs''') <- foldCtx (\cx x' -> equate cx x' ([] :=> tUnit)) ic' xs''
                              (ic''', (t, x')) <- inferExpr ic'' x
                              let txs = TEBlock $ xs''' ++ [(t, x')]
                              pure (ic''', (t, txs))
inferExpr ic (EIntLiteral val) = let (ic', u) = allocVar ic in
                                     pure (addConstraint ic' (TVar u <: TConst "Integer"), (TVar u, TEIntLiteral val))
inferExpr ic (ELet name Nothing val) = do (ic', (t, vt)) <- inferExpr ic val
                                          ic'' <- addVariable ic' name t
                                          pure (ic'', (tUnit, TELet name (t, vt)))
inferExpr ic (EIdent name) = do t <- findVariable ic name
                                pure (ic, (t, TEIdent name))
inferExpr ic (ECall (EIdent name) xs) = do fsch <- findFunction ic name
                                           (ic', xs') <- foldCtx inferExpr ic xs
                                           (ic'', xs'', t) <- inferCall ic' fsch xs'
                                           pure (ic'', (t, TECall (tUnit, TEIdent name) xs''))
inferExpr _ x = error $ "TODO: infer expr: " ++ pprint x

--- Top-level stuff
checkItem :: TypeContext -> Item -> Result TypeError (TypeContext, Maybe TaggedItem)
checkItem tc (IExternFunction _ _) = Ok (tc, Nothing)
-- TODO use function's scheme and arguments to introduce local types/variables
checkItem tc (IFunction _ (Scheme us (ps :=> TFunction xs fty)) body) = do (ic, body') <- inferExpr (emptyInferContext tc) body
                                                                           (ic', (t, res)) <- equate ic body' (qualify ps fty)
                                                                           -- TODO check that res doesn't have any unresolved variables left
                                                                           --       (i.e. variables that still exist in the tree and aren't present in us)
                                                                           -- TODO check ic' remaining constraints against function scheme
                                                                           let res' = applyAll (subst ic') res
                                                                           pure (tc, Just (TIFunction (Scheme us (ps :=> TFunction xs fty)) (t, res')))
checkItem _ _ = undefined

extractItem :: TypeContext -> Item -> Result TypeError TypeContext
extractItem tc (IFunction name sch _) = addFunctionScheme tc name sch
extractItem tc (IExternFunction name sch) = addFunctionScheme tc name sch

checkProgram :: TypeContext -> Program -> Result TypeError (TypeContext, TaggedProgram)
checkProgram tc (Program is) = do tc' <- foldM extractItem tc is
                                  (tc'', is') <- foldCtx checkItem tc' is
                                  pure (tc'', catMaybes is')

-- Test program:
-- extern fn<T: PartialEq> f1(x: T, y: T) -> T;
--
-- fn main() -> i64 {
--   let x = 1234;
--   let y = 4321;
--   let z = f1(x, x);
--   z
-- }
dPartialEq = TConst "PartialEq"
dExternFn1 = IExternFunction "f1" $ Scheme ["T"]
    ([TVar "T" <: dPartialEq] :=> TFunction [TVar "T", TVar "T"] (TVar "T"))

dLetX = ELet "x" Nothing (EIntLiteral 1234)
dLetY = ELet "y" Nothing (EIntLiteral 4321)
dCallF1 = ECall (EIdent "f1") [EIdent "x", EIdent "x"]
dLetZ = ELet "z" Nothing dCallF1
dZ = EIdent "z"
-- dMainBlock = Block [ dLetX, dLetY, dLetZ ] (Just dZ)
dFnMain = IFunction "main" (Scheme [] ([] :=> TFunction [] tI64)) $ EBlock [dLetX, dLetY, dLetZ, dZ]

dProgram1 = Program [ dExternFn1, dFnMain ]
