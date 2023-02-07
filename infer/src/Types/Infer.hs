module Types.Infer where

import PPrint
import Types.Data
import Types.Context
import Result
import Control.Monad (foldM)
import Types.Subst
import Data.Maybe (catMaybes)
import Data.List (nub, partition)

qUnit = [] :=> tUnit

instantiate :: InferenceContext -> Scheme -> (InferenceContext, Qualified Type)
instantiate = loop
    where loop ic (Scheme [] qt) = (ic, qt)
          loop ic (Scheme (u:us) qt) =
              let (ic', u') = allocVar ic in
                loop ic' (Scheme us (apply (u +-> TVar u') qt))

intTypeNames = ["i64", "i32", "i16", "i8", "u64", "u32", "u16", "u8"]
floatTypeNames = ["f64", "f32"]
intTraits = [TConst "Integer", TConst "PartialEq", TConst "PartialOrd"]

isIntOrFloat :: Id -> Bool
isIntOrFloat t = t `elem` intTypeNames || t `elem` floatTypeNames

isCastable :: Id -> Id -> Bool
isCastable t "char" = t `elem` intTypeNames
isCastable "char" t = t `elem` intTypeNames
isCastable t1 t2
  | t1 == t2 = True
  | isIntOrFloat t1 && isIntOrFloat t2 = True
  | otherwise = False

onlyIntTraits :: [Constraint] -> Bool
onlyIntTraits = all ((`elem` intTraits) . rhs)

checkCast :: Qualified Type -> Qualified Type -> Bool
-- Check int casts from literals
-- TODO this is ugly and doesn't cover all values
checkCast (ps :=> TVar _) ([] :=> TConst tc) = tc `elem` intTypeNames && onlyIntTraits ps
checkCast ([] :=> TConst tc1) ([] :=> TConst tc2) = isCastable tc1 tc2
checkCast t1 t2 = error $ "TODO: check cast from " ++ pprint t1 ++ " to " ++ pprint t2

-- Lifts all constraints directly concerning t, the rest remains
-- e.g. unconstrain ([T1 <: Trait1, T2 <: Trait2] :=> T1<T2>) == [T2 <: Trait2] :=> T1<T2>
--      (assuming T1 <: Trait1 in Γ)
unconstrain :: InferenceContext -> Qualified Type -> Result TypeError (Qualified Type)
unconstrain _ (ps :=> TVar u) = Ok $ ps :=> TVar u
unconstrain ic (ps :=> t) =
    case checkConstraints tc ps' of
        Ok _ -> pure (ps'' :=> t)
        Err e -> Err e
    where tc = tcx ic
          (ps', ps'') = partition ((== t) . lhs) ps

-- Shallow unconstrain
unconstrainExpr' :: InferenceContext -> TaggedExpr -> Result TypeError TaggedExpr
unconstrainExpr' ic (XExpr t v) = (`XExpr` v) <$> unconstrain ic t

-- Deep unconstrain
unconstrainExprDeep' :: InferenceContext -> XExprValue (Qualified Type) -> Result TypeError (XExprValue (Qualified Type))
unconstrainExprDeep' ic (XECall f xs) = do
    f' <- unconstrainExprDeep ic f
    xs' <- mapM (unconstrainExprDeep ic) xs
    pure $ XECall f' xs'
unconstrainExprDeep' ic (XEBlock xs) = XEBlock <$> mapM (unconstrainExprDeep ic) xs
unconstrainExprDeep' ic (XELet name t v) = XELet name t <$> unconstrainExprDeep ic v
unconstrainExprDeep' ic (XEAs x t) = do
    x' <- unconstrainExprDeep ic x
    pure $ XEAs x' t
unconstrainExprDeep' _ (XEIntLiteral val) = Ok $ XEIntLiteral val
unconstrainExprDeep' _ (XEIdent name) = Ok $ XEIdent name
unconstrainExprDeep' _ expr = error $ "TODO: unconstrainExprDeep' " ++ show expr

unconstrainExprDeep :: InferenceContext -> TaggedExpr -> Result TypeError TaggedExpr
unconstrainExprDeep ic (XExpr t v) = do
    t' <- unconstrain ic t
    v' <- unconstrainExprDeep' ic v
    pure $ XExpr t' v'

bindVarConst :: InferenceContext -> [Constraint] -> Id -> Id -> Result TypeError (InferenceContext, Qualified Type)
bindVarConst ic ps u tc = do
    let s = u +-> TConst tc
    let ps' = apply s ps
    let qt' = ps' :=> TConst tc
    qt'' <- unconstrain ic qt'
    pure (addSubst ic s, qt'')

equateInner :: InferenceContext -> Qualified Type -> Qualified Type -> Result TypeError (InferenceContext, Qualified Type)
equateInner ic (ps :=> TVar u) (ps' :=> TVar u') = do
    Ok (addSubst ic s, ps'' :=> TVar u')
    where s = u +-> TVar u'
          ps'' = nub (apply s (ps ++ ps'))
equateInner ic (ps :=> TVar u) ([] :=> TConst tc) = bindVarConst ic ps u tc
equateInner ic ([] :=> TConst tc) (ps :=> TVar u) = bindVarConst ic ps u tc
equateInner ic ([] :=> TConst tc1) ([] :=> TConst tc2) | tc1 == tc2 = Ok (ic, [] :=> TConst tc2)
equateInner _ t1 t2 = error $ "TODO: " ++ pprint t1 ++ ", " ++ pprint t2

equate :: InferenceContext -> TaggedExpr -> Qualified Type -> Result TypeError (InferenceContext, TaggedExpr)
equate ic (XExpr t v) qt = do t' <- unconstrain ic (apply s t)
                              qt' <- unconstrain ic (apply s qt)
                              (ic', t'') <- equateInner ic t' qt'
                              pure (ic', XExpr t'' v)
    where s = subst ic

qualify :: [Constraint] -> Type -> Qualified Type
qualify ps (TVar u) = filter ((== TVar u) . lhs) ps :=> TVar u
qualify _ t = [] :=> t

splitFunc :: Type -> Result TypeError ([Type], Type)
splitFunc (TFunction xs t) = Ok (xs, t)
splitFunc s = error $ "Not a function: " ++ pprint s

inferCall :: InferenceContext -> Qualified Type -> [TaggedExpr] -> Result TypeError (InferenceContext, [TaggedExpr], Qualified Type)
inferCall ic (ps :=> TFunction xs t) ys
    | length xs /= length ys = undefined
    | otherwise = do
        let qargs = map (qualify ps) xs
        let qt = qualify ps t
        (ic', xs') <- foldCtx (uncurry . equate) ic (zip ys qargs)
        xs'' <- mapM (unconstrainExpr' ic') $ apply (subst ic') xs'
        t' <- unconstrain ic' (apply (subst ic') qt)
        pure (ic', xs'', t')
inferCall _ fqt _ = error $ "Not a function: " ++ pprint fqt

-- Inference
inferExpr' :: InferenceContext -> ExprValue -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr' ic (XEBlock []) = Ok (ic, XExpr qUnit (XEBlock []))
inferExpr' ic (XEBlock xs) =
    do let (xs', x) = (init xs, last xs)
       (ic', xs'') <- foldCtx inferExpr ic xs'
       (ic'', xs''') <- foldCtx (\cx x' -> equate cx x' qUnit) ic' xs''
       (ic''', XExpr t x') <- inferExpr ic'' x
       let txs = XEBlock $ xs''' ++ [XExpr t x']
       pure (ic''', XExpr t txs)
inferExpr' ic (XEIntLiteral val) =
    let (ic', u) = allocVar ic in pure (ic', XExpr ([TVar u <: TConst "Integer"] :=> TVar u) (XEIntLiteral val))
inferExpr' ic (XELet name Nothing val) =
    do (ic', XExpr t vt) <- inferExpr ic val
       ic'' <- addVariable ic' name t
       pure (ic'', XExpr qUnit (XELet name Nothing (XExpr t vt)))
inferExpr' ic (XELet name (Just ty) val) =
    do (ic', val') <- inferExpr ic val
       (ic'', XExpr t vt) <- equate ic' val' ([] :=> ty)
       ic''' <- addVariable ic'' name t
       pure (ic''', XExpr ([] :=> tUnit) (XELet name (Just ty) (XExpr t vt)))
inferExpr' ic (XEIdent name) = case findVariable ic name of
    Ok t -> pure (ic, XExpr t (XEIdent name))
    Err e -> case findFunction ic name of
        Ok f ->
            let (ic', fqt) = instantiate ic f in pure (ic', XExpr fqt (XEIdent name))
        Err _ -> Err e
inferExpr' ic (XECall f xs) =
    do (ic', XExpr ft' fv') <- inferExpr ic f
       (ic'', xs') <- foldCtx inferExpr ic' xs
       (ic''', xs'', t) <- inferCall ic'' ft' xs'
       let xf = XExpr ft' fv'
       pure (ic''', XExpr t (XECall xf xs''))
inferExpr' ic (XEAs val ty) =
    do (ic', XExpr t x) <- inferExpr ic val
       let qty = [] :=> ty
       -- TODO: ty should be changed to a qualified type so it can be used
       --       in generic functions: fn<T...> f(x: ...) -> T { x as T }
       if checkCast t qty then
           pure (ic', XExpr qty (XEAs (XExpr t x) ty))
       else
           error $ "TODO: cannot cast from " ++ pprint t ++ " to " ++ pprint qty
inferExpr' _ x = error $ "TODO: infer expr: " ++ pprint x

inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr ic (XExpr _ x) = inferExpr' ic x

--- Top-level stuff
checkItem :: TypeContext -> Item -> Result TypeError (TypeContext, Maybe TaggedItem)
checkItem tc (XIExternFunction _ _) = Ok (tc, Nothing)
-- TODO use function's scheme and arguments to introduce local types/variables
checkItem tc (XIFunction name (Scheme us (ps :=> TFunction xs fty)) body) =
    do (ic, body') <- inferExpr (emptyInferContext tc) body
       (ic', res) <- equate ic body' (qualify ps fty)
       -- TODO check that res doesn't have any unresolved variables left
       --       (i.e. variables that still exist in the tree and aren't present in us)
       -- TODO check ic' remaining constraints against function scheme
       res' <- unconstrainExprDeep ic' $ applyAll (subst ic') res
       pure (tc, Just (XIFunction name (Scheme us (ps :=> TFunction xs fty)) res'))
checkItem _ _ = undefined

extractItem :: TypeContext -> Item -> Result TypeError TypeContext
extractItem tc (XIFunction name sch _) = addFunctionScheme tc name sch
extractItem tc (XIExternFunction name sch) = addFunctionScheme tc name sch

checkProgram :: TypeContext -> Program -> Result TypeError (TypeContext, TaggedProgram)
checkProgram tc (XProgram is) =
    do tc' <- foldM extractItem tc is
       (tc'', is') <- foldCtx checkItem tc' is
       pure (tc'', XProgram (catMaybes is'))

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
dT1 = TVar "T"

dExternFn1 = externFn_ "f1" $ Scheme ["T"] ([dT1 <: dPartialEq] :=> TFunction [dT1, dT1] dT1)
dLetX = let_ "x" $ lint_ 1234
dLetY = let_ "y" $ lint_ 4321

dLetV = let_ "v" $ as_ (id_ "y") tI32

dCallF1 = call_ (id_ "f1") [id_ "x", id_ "y"]
dLetZ = let_ "z" dCallF1
-- dLetZ = let_ "z" (id_ "x")
dZ = id_ "z"

dMainFn = fn_ "main" (Scheme [] ([] :=> TFunction [] tI64)) $ block_ [dLetX, dLetY, dLetV, dLetZ, dZ]

dProgram1 = XProgram [dExternFn1, dMainFn]
