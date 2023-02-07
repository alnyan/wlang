module Types.Context where

import Types.Data
import Types.Subst
import Result
import PPrint
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.List (nub)

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

checkConstraints :: TypeContext -> [Constraint] -> Result TypeError ()
checkConstraints tc ps = case find isErr (map (checkConstraint tc) ps) of
                           Just e -> e
                           Nothing -> Ok ()

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

projectConstraints :: TypeVar -> [Constraint] -> Type -> [Constraint]
projectConstraints u ps t = apply (u +-> t) ps

constraints :: InferenceContext -> TypeVar -> [Constraint]
constraints ic u = filter ((== TVar u) . lhs) (cs ic)

unconstrain :: InferenceContext -> TypeVar -> Type -> Result TypeError InferenceContext
unconstrain ic u t = do let ic' = ic { subst = subst ic @@ (u +-> t) }
                        let ps = apply (subst ic') (constraints ic' u)
                        case find isErr $ map (checkConstraint (tcx ic')) ps of
                          Just x -> error $ "Constraint check failed: " ++ show x
                          Nothing -> pure $ removeVar ic' u

