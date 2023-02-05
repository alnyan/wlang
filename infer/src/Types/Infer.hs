module Types.Infer where

import PPrint
import Types.Data
import Result
import Control.Monad (foldM)
import Types.Subst
import Data.Maybe (isJust, catMaybes)
import Utils (mapPair)

data TaggedExprValue = TEIdent String
                     | TECall TaggedExpr [TaggedExpr]
                     | TEBlock [TaggedExpr]
                     | TELet String TaggedExpr
                     | TEIntLiteral Int
    deriving (Show)

type TaggedExpr = (Type, TaggedExprValue)
data TaggedItem = TIFunction Scheme TaggedExpr
    deriving Show
type TaggedProgram = [TaggedItem]

class Instantiate t where
    instantiate :: InferenceContext -> t -> (InferenceContext, t)

instance Instantiate Scheme where
    instantiate = loop []
        where loop us' ic (Scheme [] qt) = (ic, Scheme us' qt)
              loop us' ic (Scheme (u:us) qt) = let (ic', u') = freshenVar ic u in
                                                   loop (u':us') ic' (Scheme us (apply (u +-> TVar u') qt))

-- TODO traits
data TypeContext = TypeContext { functions :: [(String, Scheme)] }
    deriving Show

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext { functions = [] }

addFunctionScheme :: TypeContext -> String -> Scheme -> Result TypeError TypeContext
addFunctionScheme tc name scheme | isJust (lookup name fs) = undefined
                                 | otherwise = Ok $ tc{ functions = fs ++ [(name, scheme)] }
                                where fs = functions tc

getFunctionScheme :: TypeContext -> String -> Result TypeError Scheme
getFunctionScheme tc name = okOr f $ UndefinedFunction name
    where f = lookup name (functions tc)

data InferenceContext = InferenceContext { tcx :: TypeContext,
                                           vars :: [(String, Type)],
                                           lastVarId :: Int,
                                           cs :: [Constraint] }
    deriving Show

emptyInferContext tc = InferenceContext { tcx = tc,
                                          lastVarId = 0,
                                          cs = [],
                                          vars = [] }

addVariable :: InferenceContext -> String -> Type -> Result TypeError InferenceContext
addVariable ic name ty | isJust (lookup name vs) = undefined
                       | otherwise = Ok $ ic{ vars = vs ++ [(name, ty)] }
                       where vs = vars ic

findFunction :: InferenceContext -> String -> Result TypeError Scheme
findFunction ic = getFunctionScheme (tcx ic)

findVariable :: InferenceContext -> String -> Result TypeError Type
findVariable ic name = okOr v $ UndefinedVariable name
    where v = lookup name (vars ic)

allocVar :: InferenceContext -> (InferenceContext, TypeVar)
allocVar ic = (ic { lastVarId = vi }, "v" ++ show vi)
    where vi = lastVarId ic + 1

freshenVar :: InferenceContext -> TypeVar -> (InferenceContext, TypeVar)
freshenVar = undefined

foldCtx :: (c -> t -> Result e (c, u)) -> c -> [t] -> Result e (c, [u])
foldCtx = loop []
    where loop ys _ tc [] = Ok (tc, ys)
          loop ys f tc (x:xs) = do (tc', y) <- f tc x
                                   loop (y:ys) f tc' xs

-- Inference
inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr ic (EBlock []) = Ok (ic, (tUnit, TEBlock []))
inferExpr ic (EBlock xs) = do let (xs', x) = (init xs, last xs)
                              (ic', xs'') <- foldCtx inferExpr ic xs'
                              -- TODO equate xs'' types to tUnit
                              (ic'', (t, x')) <- inferExpr ic' x
                              let txs = TEBlock $ (t, x'):xs''
                              pure (ic'', (t, txs))
inferExpr ic (EIntLiteral val) = let (ic', u) = allocVar ic in
                                     error "TODO: constrain int literal type vars"
                                     Ok (ic', (TVar u, TEIntLiteral val))
inferExpr ic (ECall (EIdent name) xs) = do fsch <- findFunction ic name
                                           -- Instantiate a fresh call scheme fsch
                                           let (ic', fsch') = instantiate ic fsch
                                           -- Alloc type var for return
                                           let (ic'', t) = mapPair (id, TVar) $ allocVar ic'
                                           -- Infer call types, create a schema for them
                                           (ic''', xs') <- foldCtx inferExpr ic'' xs
                                           -- TODO make a schema from xs' types, equate xsch and fsch'
                                           -- TODO return type is t
                                           undefined
inferExpr _ x = error $ "TODO: infer expr: " ++ pprint x

--- Top-level stuff
checkItem :: TypeContext -> Item -> Result TypeError (TypeContext, Maybe TaggedItem)
checkItem tc (IExternFunction _ _) = Ok (tc, Nothing)
-- TODO use function's scheme and arguments to introduce local types/variables
checkItem tc (IFunction _ (Scheme _ (_ :=> TFunction _ fty)) body) = do (_, _) <- inferExpr (emptyInferContext tc) body
                                                                        -- TODO equate fty qt
                                                                        undefined
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
--   let z = f1(x, y);
--   z
-- }
dPartialEq = TConst "PartialEq"
dExternFn1 = IExternFunction "f1" $ Scheme ["T"]
    ([TVar "T" <: dPartialEq] :=> TFunction [TVar "T", TVar "T"] (TVar "T"))

dLetX = ELet "x" Nothing (EIntLiteral 1234)
dLetY = ELet "y" Nothing (EIntLiteral 4321)
dCallF1 = ECall (EIdent "f1") [EIdent "x", EIdent "y"]
dLetZ = ELet "z" Nothing dCallF1
dZ = EIdent "z"
-- dMainBlock = Block [ dLetX, dLetY, dLetZ ] (Just dZ)
dFnMain = IFunction "main" (Scheme [] ([] :=> TFunction [] tI64)) $ EBlock [dLetX, dLetY, dLetZ, dZ]

dProgram1 = Program [ dExternFn1, dFnMain ]
