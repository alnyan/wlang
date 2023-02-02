module PPrint where

import Data.List (intercalate, concatMap)
import Types.Data

---- Pretty printing helper
class PrettyPrint a where
    pprint :: a -> String

-- Implement pretty-printing for types
instance PrettyPrint TypeVar where
    pprint (TVAny u) = u
    pprint (TVInt n) = "{integer #" ++ show n ++ "}"
    pprint (TVFloat n) = "{float #" ++ show n ++ "}"

instance PrettyPrint a => PrettyPrint [a] where
    pprint = intercalate ", " . map pprint

instance PrettyPrint Type where
    pprint (TFunction ts t) = "fn (" ++ pprint ts ++ ") -> " ++ pprint t
    pprint (TParameterized t ts) = pprint t ++ "<" ++ pprint ts ++ ">"
    pprint (TArray t n) = "[" ++ pprint t ++ "; " ++ show n ++ "]"
    pprint (TPointer t) = pprint t
    pprint (TConst tc) = tc
    pprint (TVar u) = pprint u

instance PrettyPrint Constraint where
    pprint (Implements t u) = pprint t ++ ": " ++ pprint u

instance PrettyPrint Scheme where
    pprint (Scheme [] qt) = pprint qt
    pprint (Scheme us qt) = "for<" ++ pprint us ++ ">" ++ pprint (Scheme [] qt)

instance PrettyPrint t => PrettyPrint (Qualified t) where
    pprint ([] :=> t) = pprint t
    pprint (ps :=> t) = pprint t ++ " where " ++ pprint ps

-- Expression/statement/item pprint
instance PrettyPrint Expr where
    pprint (EIdent name) = name
    pprint (ECall f xs) = pprint f ++ "(" ++ pprint xs ++ ")"
    pprint (EIntLiteral v) = show v
    pprint _ = undefined

instance PrettyPrint Stmt where
    pprint (SLet name (Just ty) val) = "let " ++ name ++ ": " ++ pprint ty ++ " = " ++ pprint val ++ ";"
    pprint (SLet name Nothing val) = "let " ++ name ++ " = " ++ pprint val ++ ";"
    pprint (SExpr e) = pprint e ++ ";"
    pprint _ = undefined

instance PrettyPrint Block where
    pprint (Block ss (Just e)) = "{\n" ++ (intercalate "\n" . map pprint) ss ++ "\n" ++ pprint e ++ "\n}"
    pprint (Block ss Nothing) = "{\n" ++ (intercalate "\n" . map pprint) ss ++ "\n}"

instance PrettyPrint Item where
    pprint (IExternFunction name scheme) = "extern " ++ name ++ ": " ++ pprint scheme ++ ";"
    pprint (IFunction name scheme body) = name ++ ": " ++ pprint scheme ++ "\n" ++ pprint body

instance PrettyPrint Program where
    pprint (Program is) = (intercalate "\n" . map pprint) is ++ "\n"

instance PrettyPrint (TypeVar, Type) where
    pprint (u, t) = pprint u ++ " +-> " ++ pprint t
