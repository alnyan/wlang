module Lib where

import Type (Type(..), TypeVar(..))
import Data.List (intercalate)

---- Pretty printing helper
class PrettyPrint a where
    pprint :: a -> String

-- Implement pretty-printing for types
instance PrettyPrint TypeVar where
    pprint (TVAny u) = u
    pprint (TVInt n) = "{integer #" ++ (show n) ++ "}"
    pprint (TVFloat n) = "{float #" ++ (show n) ++ "}"

instance PrettyPrint a => PrettyPrint [a] where
    pprint = intercalate ", " . map pprint

instance PrettyPrint Type where
    pprint (TFunction ts t) = "fn (" ++ (pprint ts) ++ ") -> " ++ pprint t
    pprint (TParameterized t ts) = pprint t ++ "<" ++ (pprint ts) ++ ">"
    pprint (TArray t n) = "[" ++ pprint t ++ "; " ++ (show n) ++ "]"
    pprint (TPointer t) = pprint t
    pprint (TConst tc) = tc
    pprint (TVar u) = pprint u
