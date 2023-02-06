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
          | EIf Expr Expr (Maybe Expr)
          | EBlock [Expr]
          | ELet String (Maybe Type) Expr
          | EReturn Expr
data Item = IFunction String Scheme Expr
          | IExternFunction String Scheme
newtype Program = Program [Item]

-- TODO(alnyan): I will merge TaggedExpr with Expr in the next commit, I promise
data TaggedExprValue = TEIdent String
                     | TECall TaggedExpr [TaggedExpr]
                     | TEBlock [TaggedExpr]
                     | TELet String TaggedExpr
                     | TEIntLiteral Int
    deriving (Show, Eq)

type TaggedExpr = (Type, TaggedExprValue)
data TaggedItem = TIFunction Scheme TaggedExpr
    deriving Show
type TaggedProgram = [TaggedItem]

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
               | UndefinedVariable String
               | UndefinedFunction String
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
