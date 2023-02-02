module Types.Infer where

import Types.Data
import Result
import Control.Monad (foldM)
import Data.Maybe (isJust)

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

allocIntVar :: InferenceContext -> (InferenceContext, Type)
allocIntVar ic = (ic { lastVarId = vi }, TVar (TVInt vi))
    where vi = lastVarId ic + 1

inferExpr :: InferenceContext -> Expr -> Result TypeError (InferenceContext, Type)
inferExpr ic (EIntLiteral _) = Ok $ allocIntVar ic
inferExpr ic (ECall (EIdent name) args) = do fscheme <- findFunction ic name
                                             -- Relate argument types
                                             undefined

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
