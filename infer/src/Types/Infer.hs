module Types.Infer where

import PPrint
import Types.Data
import Types.Context
import Result
import Control.Monad (foldM)
import Types.Subst
import Data.Maybe (catMaybes)
import Data.List (nub)

class Instantiate t where
    instantiate :: InferenceContext -> t -> (InferenceContext, t)

instance Instantiate Scheme where
    instantiate = loop []
        where loop us' ic (Scheme [] qt) = (ic, Scheme us' qt)
              loop us' ic (Scheme (u:us) qt) = let (ic', u') = allocVar ic in
                                                   loop (u':us') ic' (Scheme us (apply (u +-> TVar u') qt))

intTypeNames = ["i64", "i32", "i16", "i8", "u64", "u32", "u16", "u8"]
floatTypeNames = ["f64", "f32"]

isIntOrFloat :: Id -> Bool
isIntOrFloat t = t `elem` intTypeNames || t `elem` floatTypeNames

isCastable :: Id -> Id -> Bool
isCastable t "char" = t `elem` intTypeNames
isCastable "char" t = t `elem` intTypeNames
isCastable t1 t2
  | t1 == t2 = True
  | isIntOrFloat t1 && isIntOrFloat t2 = True
  | otherwise = False

checkCast :: InferenceContext -> Type -> Type -> Result TypeError ()
checkCast ic (TVar u) t = do
    let ps = constraints ic u
    let ps' = projectConstraints u ps t
    checkConstraints (tcx ic) ps'

checkCast _ (TConst tc1) (TConst tc2)
  | isCastable tc1 tc2 = Ok ()
checkCast _ t1 t2 = error $ "TODO: cast " ++ pprint t1 ++ " to " ++ pprint t2

equateInner :: InferenceContext -> Type -> Qualified Type -> Result TypeError (InferenceContext, Type)
equateInner ic (TVar u) (ps :=> TVar u') =
    do let ic' = replaceVar ic u u'
       pure (foldCtx' addConstraint ic' ps, TVar u')
equateInner ic (TVar u) ([] :=> t) =
    do ic' <- unconstrain ic u t
       pure (ic', apply (subst ic') t)
equateInner ic (TConst tc1) (ps :=> TVar u) =
    do let ps' = nub (ps ++ constraints ic u)
       let ps'' = projectConstraints u ps' (TConst tc1)
       case checkConstraints (tcx ic) ps'' of
         Ok _ -> equateInner ic (TVar u) ([] :=> TConst tc1)
         Err e -> Err e
equateInner ic (TConst tc1) ([] :=> TConst tc2) | tc1 == tc2 = Ok (ic, TConst tc1)
equateInner _ t1 t2 = Err $ EquateError t1 t2

equate :: InferenceContext -> TaggedExpr -> Qualified Type -> Result TypeError (InferenceContext, TaggedExpr)
equate ic (XExpr t v) qt = do (ic', t') <- equateInner ic (apply s t) (apply s qt)
                              pure (ic', XExpr t' (apply (subst ic') v))
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
inferExpr' :: InferenceContext -> ExprValue -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr' ic (XEBlock []) = Ok (ic, XExpr tUnit (XEBlock []))
inferExpr' ic (XEBlock xs) =
    do let (xs', x) = (init xs, last xs)
       (ic', xs'') <- foldCtx inferExpr ic xs'
       (ic'', xs''') <- foldCtx (\cx x' -> equate cx x' ([] :=> tUnit)) ic' xs''
       (ic''', XExpr t x') <- inferExpr ic'' x
       let txs = XEBlock $ xs''' ++ [XExpr t x']
       pure (ic''', XExpr t txs)
inferExpr' ic (XEIntLiteral val) =
    let (ic', u) = allocVar ic in
      pure (addConstraint ic' (TVar u <: TConst "Integer"), XExpr (TVar u) (XEIntLiteral val))
inferExpr' ic (XELet name Nothing val) =
    do (ic', XExpr t vt) <- inferExpr ic val
       ic'' <- addVariable ic' name t
       pure (ic'', XExpr tUnit (XELet name Nothing (XExpr t vt)))
inferExpr' ic (XELet name (Just ty) val) =
    do (ic', val') <- inferExpr ic val
       (ic'', XExpr t vt) <- equate ic' val' ([] :=> ty)
       ic''' <- addVariable ic'' name t
       pure (ic''', XExpr tUnit (XELet name (Just ty) (XExpr t vt)))
inferExpr' ic (XEIdent name) =
    do t <- findVariable ic name
       pure (ic, XExpr t (XEIdent name))
inferExpr' ic (XECall (XExpr _ (XEIdent name)) xs) =
    do fsch <- findFunction ic name
       (ic', xs') <- foldCtx inferExpr ic xs
       (ic'', xs'', t) <- inferCall ic' fsch xs'
       pure (ic'', XExpr t (XECall (XExpr tUnit (XEIdent name)) xs''))
inferExpr' ic (XEAs val ty) =
    do (ic', XExpr t x) <- inferExpr ic val
       case checkCast ic' t ty of
         Ok _ -> pure (ic', XExpr ty (XEAs (XExpr t x) ty))
         Err e -> Err e
inferExpr' _ x = error $ "TODO: infer expr: " ++ pprint x

inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, TaggedExpr)
inferExpr ic (XExpr _ x) = inferExpr' ic x

--- Top-level stuff
checkItem :: TypeContext -> Item -> Result TypeError (TypeContext, Maybe TaggedItem)
checkItem tc (XIExternFunction _ _) = Ok (tc, Nothing)
-- TODO use function's scheme and arguments to introduce local types/variables
checkItem tc (XIFunction name (Scheme us (ps :=> TFunction xs fty)) body) =
    do (ic, body') <- inferExpr (emptyInferContext tc) body
       (ic', XExpr t res) <- equate ic body' (qualify ps fty)
       -- TODO check that res doesn't have any unresolved variables left
       --       (i.e. variables that still exist in the tree and aren't present in us)
       -- TODO check ic' remaining constraints against function scheme
       let res' = applyAll (subst ic') res
       pure (tc, Just (XIFunction name (Scheme us (ps :=> TFunction xs fty)) (XExpr t res')))
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
dLetY = letv_ "y" tI32 $ lint_ 4321
dLetV = let_ "v" $ as_ (lint_ 1) tI32
dCallF1 = call_ (id_ "f1") [id_ "x", as_ (id_ "y") tI64]
dLetZ = let_ "z" dCallF1
-- dLetZ = let_ "z" (id_ "x")
dZ = id_ "z"

dMainFn = fn_ "main" (Scheme [] ([] :=> TFunction [] tI64)) $ block_ [dLetX, dLetY, dLetV, dLetZ, dZ]

dProgram1 = XProgram [dExternFn1, dMainFn]
