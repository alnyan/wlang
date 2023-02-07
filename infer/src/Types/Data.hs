module Types.Data where

---- Data structure definitions
-- Type identifier (generated from high-level types through internment process)
type Id = String

-- Expressions
-- data Expr = EIntLiteral Int
--           | EFloatLiteral Float
--           | EIdent String
--           | EBoolLiteral Bool
--           | ECall Expr [Expr]
--           | EArray [Expr]
--           | EAs Expr Type
--           | EIf Expr Expr (Maybe Expr)
--           | EBlock [Expr]
--           | ELet String (Maybe Type) Expr
--           | EReturn Expr
-- data Item = IFunction String Scheme Expr
--           | IExternFunction String Scheme
-- newtype Program = Program [Item]

data XExprValue t = XEIntLiteral Int
                  | XEFloatLiteral Float
                  | XEBoolLiteral Bool
                  | XEIdent String
                  | XECall (XExpr t) [XExpr t]
                  | XEArray [XExpr t]
                  | XEAs (XExpr t) Type
                  | XEIf (XExpr t) (XExpr t) (Maybe (XExpr t))
                  | XEBlock [XExpr t]
                  | XELet String (Maybe Type) (XExpr t)
                  | XEReturn (XExpr t)
    deriving (Show, Eq)
data XExpr t = XExpr t (XExprValue t)
    deriving (Show, Eq)
data XItem t = XIFunction String Scheme (XExpr t)
             | XIExternFunction String Scheme
    deriving (Show, Eq)
newtype XProgram t = XProgram [XItem t]
    deriving (Show, Eq)

type TaggedExpr = XExpr Type
type TaggedItem = XItem Type
type TaggedProgram = XProgram Type

type ExprValue = XExprValue ()
type Expr = XExpr ()
type Item = XItem ()
type Program = XProgram ()

-- Types
-- TODO: struct/enum types
-- TODO: references
-- TODO: slices
-- TODO: anything else
data Type = TVar TypeVar                -- e.g. `{integer #n}`, `{float #m}`, `T`
          | TConst Id                   -- e.g. `MyType`
          | TParameterized Type [Type]  -- e.g. `Result<Type1, E>`
          | TArray Type Int             -- e.g. `[T; 1024]`
          | TPointer Type               -- e.g. `*T`
          | TFunction [Type] Type       -- e.g. `fn(T, U, V, ...) -> R`
    deriving (Show, Eq)

type TypeVar = Id;

-- Substitutions
type Subst = [(TypeVar, Type)]

-- Constraints
data Constraint = Implements Type Type
    deriving (Show, Eq)

lhs :: Constraint -> Type
lhs (Implements l _) = l

data Qualified t = [Constraint] :=> t
    deriving (Show, Eq)

data Scheme = Scheme [TypeVar] (Qualified Type)
    deriving (Show, Eq)

---- Errors enum
data TypeError = UnifyError Type Type
               | OccursCheck TypeVar Type
               | ArgumentCountMismatch [Type] [Type]
               | IntUnifyError TypeVar Type
               | FloatUnifyError TypeVar Type
               | MatchError Type Type
               | MergeError Subst Subst
               | UndefinedVariable String
               | UndefinedFunction String
               | EquateError Type (Qualified Type)
    deriving (Show, Eq)

---- Constants
-- Common types
tUnit = TConst "()"
tI64 = TConst "i64"
tI32 = TConst "i32"
tI16 = TConst "i16"
tI8 = TConst "i8"
tU64 = TConst "u64"
tU32 = TConst "u32"
tU16 = TConst "u16"
tU8 = TConst "u8"
tF64 = TConst "f64"
tF32 = TConst "f32"

tCoreIntNames = [ "i64", "i32", "i16", "i8",
                  "u64", "u32", "u16", "u8" ]
tCoreFloatNames = [ "f32", "f64" ]

isCoreInteger :: Type -> Bool
isCoreInteger (TConst tc) = tc `elem` tCoreIntNames
isCoreInteger _ = False

isCoreFloat :: Type -> Bool
isCoreFloat (TConst tc) = tc `elem` tCoreFloatNames
isCoreFloat _ = False

infixr 4 <:
(<:) :: Type -> Type -> Constraint
t1 <: t2 = Implements t1 t2


-- AST construction helpers
let_ name val = XExpr () $ XELet name Nothing val
letv_ name t val = XExpr () $ XELet name (Just t) val
lint_ val = XExpr () $ XEIntLiteral val
id_ name = XExpr () $ XEIdent name
as_ val ty = XExpr () $ XEAs val ty
call_ f xs = XExpr () $ XECall f xs
block_ xs = XExpr () $ XEBlock xs

externFn_ = XIExternFunction
fn_ = XIFunction
