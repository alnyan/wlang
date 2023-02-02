module Types.Data where

---- Data structure definitions
-- Type identifier (generated from high-level types through internment process)
type Id = String

-- Expressions
data Expr = EIntLiteral Int
          | EFloatLiteral Float
          | EIdent String
          | EBoolLiteral Bool
          | ECall Expr [Expr]
          | EArray [Expr]
          | EAs Expr Type
          | EIf Expr Block Block
          | EBlock Block
data Stmt = SExpr Expr
          | SLet String (Maybe Type) Expr
          | SReturn Expr
          | SIf Expr Block
data Block = Block [Stmt] (Maybe Expr)
data Item = IFunction String Scheme Block
          | IExternFunction String Scheme
newtype Program = Program [Item]

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

data TypeVar = TVAny Id                 -- Can take any type, like `T` in `struct X<T>`
             | TVInt Int                -- Can take any *integer* type, used for integer literals
             | TVFloat Int              -- Can take any *float* type, used for float literals
    deriving (Show, Eq)

-- Substitutions
type Subst = [(TypeVar, Type)]

-- Constraints
data Constraint = Implements Type Type
    deriving (Show, Eq)

data Qualified t = [Constraint] :=> t
    deriving Show

data Scheme = Scheme [TypeVar] (Qualified Type)
    deriving Show

---- Errors enum
data TypeError = UnifyError Type Type
               | OccursCheck TypeVar Type
               | ArgumentCountMismatch [Type] [Type]
               | IntUnifyError TypeVar Type
               | FloatUnifyError TypeVar Type
               | MatchError Type Type
               | MergeError Subst Subst
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
isCoreInteger (TVar (TVInt _)) = True
isCoreInteger (TConst tc) = tc `elem` tCoreIntNames
isCoreInteger _ = False

isCoreFloat :: Type -> Bool
isCoreFloat (TVar (TVFloat _)) = True
isCoreFloat (TConst tc) = tc `elem` tCoreFloatNames
isCoreFloat _ = False

infixr 4 <:
(<:) :: Type -> Type -> Constraint
t1 <: t2 = Implements t1 t2
