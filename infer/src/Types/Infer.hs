module Types.Infer where

import PPrint
import Types.Data
import Result
import Control.Monad (foldM)
import Types.Subst
import Data.Maybe (isJust)

mapPair :: (a -> b, c -> d) -> (a, c) -> (b, d)
mapPair (f, g) (x, y) = (f x, g y)

class Instantiate t where
    instantiate :: InferenceContext -> t -> (InferenceContext, t)

instance Instantiate Type where
    instantiate ic (TVar u) = undefined
    instantiate _ t = error $ show t

instance Instantiate Scheme where
    instantiate = loop []
        where loop us' ic (Scheme [] qt) = (ic, Scheme us' qt)
              loop us' ic (Scheme (u:us) qt) = let (ic', u') = freshenVar ic u in
                                                   loop (u':us') ic' (Scheme us (apply (u +-> TVar u') qt))

-- TODO traits
data TypeContext = TypeContext { functions :: [(String, Scheme)],
                                 constraints :: [Constraint] }
    deriving Show

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext { functions = [], constraints = [] }

addFunctionScheme :: TypeContext -> String -> Scheme -> Result TypeError TypeContext
addFunctionScheme tc name scheme | isJust (lookup name fs) = undefined
                                 | otherwise = Ok $ tc{ functions = fs ++ [(name, scheme)] }
                                where fs = functions tc

getFunctionScheme :: TypeContext -> String -> Result TypeError Scheme
getFunctionScheme tc name = case lookup name (functions tc) of
                                Just s -> Ok s
                                Nothing -> undefined

-- TODO anonymous type variables
data InferenceContext = InferenceContext { tcx :: TypeContext,
                                           vars :: [(String, Type)],
                                           lastVarId :: Int,
                                           lastIntId :: Int,
                                           cs :: [Constraint] }
    deriving Show

emptyInferContext tc = InferenceContext { tcx = tc,
                                          lastVarId = 0,
                                          lastIntId = 0,
                                          cs = [],
                                          vars = [] }

addVariable :: InferenceContext -> String -> Type -> Result TypeError InferenceContext
addVariable ic name ty | isJust (lookup name vs) = undefined
                       | otherwise = Ok $ ic{ vars = vs ++ [(name, ty)] }
                       where vs = vars ic

findFunction :: InferenceContext -> String -> Result TypeError Scheme
findFunction ic = getFunctionScheme (tcx ic)

findVariable :: InferenceContext -> String -> Result TypeError Type
findVariable ic name = case lookup name (vars ic) of
                         Just s -> Ok s
                         Nothing -> undefined

allocIntVar :: InferenceContext -> (InferenceContext, TypeVar)
allocIntVar ic = (ic { lastIntId = vi }, TVInt vi)
    where vi = lastIntId ic + 1

allocVar :: InferenceContext -> (InferenceContext, TypeVar)
allocVar ic = (ic { lastVarId = vi }, TVAny ("v" ++ show vi))
    where vi = lastVarId ic + 1

freshenVar :: InferenceContext -> TypeVar -> (InferenceContext, TypeVar)
freshenVar ic (TVAny _) = allocVar ic
freshenVar ic (TVInt _) = allocIntVar ic

inferList :: InferenceContext -> [Expr] -> Result TypeError (InferenceContext, [Type])
inferList = loop []
    where loop ts ic [] = Ok (ic, ts)
          loop ts ic (x:xs) = do (ic', t) <- inferExpr ic x
                                 loop (ts ++ [t]) ic' xs

inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, Type)
inferExpr ic (EIntLiteral _) = Ok $ mapPair (id, TVar) $ allocIntVar ic
inferExpr ic (ECall (EIdent name) xs) = do fsch <- findFunction ic name
                                           -- Instantiate function scheme with fresh vars
                                           let (ic', fsch') = instantiate ic fsch
                                           (ic'', xs') <- inferList ic' xs
                                           -- Alloc type var for return
                                           let (ic''', t) = mapPair (id, TVar) $ allocVar ic''
                                           -- Convert call into scheme
                                           let asch = Scheme [] $ [] :=> TFunction xs' t
                                           -- TODO relate schemes
                                           error $ pprint asch ++ " `relate` " ++ pprint fsch'
inferExpr ic (EIdent name) = do ty <- findVariable ic name
                                pure (ic, ty)
inferExpr _ _ = undefined

inferStmt :: InferenceContext -> Stmt -> Result TypeError InferenceContext
inferStmt ic (SLet name Nothing val) = do (ic', valTy) <- inferExpr ic val
                                          addVariable ic' name valTy
inferStmt _ _ = undefined

inferBlock :: InferenceContext -> Block -> Result TypeError (InferenceContext, Type)
inferBlock ic (Block ss (Just e)) = do ic' <- foldM inferStmt ic ss
                                       inferExpr ic' e
inferBlock _ _ = undefined

inferItem :: TypeContext -> Item -> Result TypeError TypeContext
inferItem tc (IFunction name scheme body) = do (ic', resTy) <- inferBlock ic body
                                               undefined
    where ic = emptyInferContext tc
inferItem tc (IExternFunction _ _) = Ok tc

extractItem :: TypeContext -> Item -> Result TypeError TypeContext
extractItem tc (IFunction name scheme _) = addFunctionScheme tc name scheme
extractItem tc (IExternFunction name scheme) = addFunctionScheme tc name scheme

inferProgram :: TypeContext -> Program -> Result TypeError TypeContext
inferProgram tc (Program ps) = do tc' <- foldM extractItem tc ps    -- Step 1. Extract stuff like functions
                                  foldM inferItem tc' ps            -- Step 2. Infer everything using that info


-- Test program:
-- extern fn<T: PartialEq> f1(x: T, y: T) -> T;
--
-- fn main() -> i64 {
--   let x = 1234;
--   let y = 4321;
--   let z = f1(x, y);
--   z
-- }
dT1 = TVAny "T"
dPartialEq = TConst "PartialEq"
dExternFn1 = IExternFunction "f1" $ Scheme [dT1]
    ([TVar dT1 <: dPartialEq] :=> TFunction [TVar dT1, TVar dT1] (TVar dT1))

dLetX = SLet "x" Nothing (EIntLiteral 1234)
dLetY = SLet "y" Nothing (EIntLiteral 4321)
dCallF1 = ECall (EIdent "f1") [EIdent "x", EIdent "y"]
dLetZ = SLet "z" Nothing dCallF1
dZ = EIdent "z"
dMainBlock = Block [ dLetX, dLetY, dLetZ ] (Just dZ)
dFnMain = IFunction "main" (Scheme [] ([] :=> TFunction [] tI64)) dMainBlock

dProgram1 = Program [ dExternFn1, dFnMain ]
