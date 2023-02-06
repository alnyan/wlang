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
instance (PrettyPrint t) => PrettyPrint (XExprValue t) where
    pprint (XELet name Nothing val) = "let " ++ name ++ " = " ++ pprint val
    pprint (XELet name (Just t) val) = "let " ++ name ++ ": " ++ pprint t ++ " = " ++ pprint val
    pprint (XEIdent name) = name
    pprint (XEBlock xs) = "{\n" ++ intercalate ";\n" (map pprint xs) ++ "\n}"
    pprint (XEIntLiteral val) = show val
    pprint (XEFloatLiteral val) = show val
    pprint (XEBoolLiteral val) = show val
    pprint (XECall f xs) = pprint f ++ "(" ++ intercalate ", " (map pprint xs) ++ ")"
    pprint (XEArray xs) = "[" ++ intercalate ", " (map pprint xs) ++ "]"
    pprint (XEAs x t) = pprint x ++ " as " ++ pprint t
    pprint (XEIf x y Nothing) = "if " ++ pprint x ++ " " ++ pprint y
    pprint (XEIf x y (Just z)) = "if " ++ pprint x ++ " " ++ pprint y ++ " else " ++ pprint z
    pprint (XEReturn val) = "return " ++ pprint val

instance (PrettyPrint t) => PrettyPrint (XExpr t) where
    pprint (XExpr t v) | pt == "" = pprint v
                       | otherwise = "(" ++ pprint v ++ " # " ++ pprint t ++ ")"
        where pt = pprint t

instance (PrettyPrint t) => PrettyPrint (XItem t) where
    pprint (XIFunction name sch body) = name ++ ": " ++ pprint sch ++ " = " ++ pprint body
    pprint (XIExternFunction name sch) = "extern " ++ name ++ ": " ++ pprint sch

instance (PrettyPrint t) => PrettyPrint (XProgram t) where
    pprint (XProgram is) = intercalate ";\n" $ map pprint is

instance PrettyPrint () where
    pprint _ = ""

instance PrettyPrint (TypeVar, Type) where
    pprint (u, t) = u ++ " +-> " ++ pprint t

instance (Show e, PrettyPrint t) => PrettyPrint (Result e t) where
    pprint (Ok t) = "Ok (" ++ pprint t ++ ")"
    pprint (Err e) = "Err (" ++ show e ++ ")"
