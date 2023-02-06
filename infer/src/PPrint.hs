{-# LANGUAGE FlexibleInstances #-}

module PPrint where

import Data.List (intercalate)
import Types.Data
import Result

---- Pretty printing helper
class PrettyPrint a where
    pprint :: a -> String

-- Implement pretty-printing for types
pprintDelimited :: PrettyPrint a => String -> [a] -> String
pprintDelimited s = intercalate s . map pprint
pprintCommad :: PrettyPrint a => [a] -> String
pprintCommad = pprintDelimited ", "

instance PrettyPrint Type where
    pprint (TFunction ts t) = "fn (" ++ pprintCommad ts ++ ") -> " ++ pprint t
    pprint (TParameterized t ts) = pprint t ++ "<" ++ pprintCommad ts ++ ">"
    pprint (TArray t n) = "[" ++ pprint t ++ "; " ++ show n ++ "]"
    pprint (TPointer t) = pprint t
    pprint (TConst tc) = tc
    pprint (TVar u) = u

instance PrettyPrint Constraint where
    pprint (Implements t u) = pprint t ++ ": " ++ pprint u

instance PrettyPrint Scheme where
    pprint (Scheme [] qt) = pprint qt
    pprint (Scheme us qt) = "for<" ++ intercalate ", " us ++ "> " ++ pprint (Scheme [] qt)

instance PrettyPrint t => PrettyPrint (Qualified t) where
    pprint ([] :=> t) = pprint t
    pprint (ps :=> t) = pprint t ++ " where " ++ pprintCommad ps

-- Expression/statement/item pprint
instance PrettyPrint Expr where
    pprint (EIdent name) = name
    pprint (ECall f xs) = pprint f ++ "(" ++ pprintCommad xs ++ ")"
    pprint (EIntLiteral v) = show v
    pprint (ELet name (Just ty) val) = "let " ++ name ++ ": " ++ pprint ty ++ " = " ++ pprint val
    pprint (ELet name Nothing val) = "let " ++ name ++ " = " ++ pprint val
    pprint (EBlock xs) = "{\n" ++ pprintDelimited ";\n" xs ++ "\n}"
    pprint _ = undefined

instance PrettyPrint Item where
    pprint (IExternFunction name scheme) = "extern " ++ name ++ ": " ++ pprint scheme ++ ";"
    pprint (IFunction name scheme body) = name ++ ": " ++ pprint scheme ++ "\n" ++ pprint body

instance PrettyPrint Program where
    pprint (Program is) = pprintDelimited "\n" is ++ "\n"

instance PrettyPrint (TypeVar, Type) where
    pprint (u, t) = u ++ " +-> " ++ pprint t

-- instance (PrettyPrint e, PrettyPrint t) => PrettyPrint (Result e t) where
--     pprint (Ok t) = "Ok (" ++ pprint t ++ ")"
--     pprint (Err e) = "Err (" ++ pprint e ++ ")"

instance (Show e, PrettyPrint t) => PrettyPrint (Result e t) where
    pprint (Ok t) = "Ok (" ++ pprint t ++ ")"
    pprint (Err e) = "Err (" ++ show e ++ ")"

pprintTE :: TaggedExpr -> String
pprintTE (t, x) = pprint x ++ ": " ++ pprint t
parens :: String -> String
parens x = "(" ++ x ++ ")"

instance PrettyPrint TaggedExprValue where
    pprint (TEBlock xs) = "{\n" ++ intercalate ";\n" (map pprintTE xs) ++ "\n}"
    pprint (TELet name (t, val)) = "let " ++ name ++ ": " ++ pprint t ++ " = " ++ pprint val
    pprint (TEIntLiteral i) = show i
    pprint (TEIdent name) = name
    pprint (TECall (_, callee) args) = pprint callee ++ "(" ++ intercalate ", " (map pprintTE args) ++ ")"

instance PrettyPrint TaggedItem where
    pprint (TIFunction s (_, x)) = pprint s ++ " " ++ pprint x
