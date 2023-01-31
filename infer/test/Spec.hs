import Test.HUnit

import Types.Unify
import Types.Subst
import Types.Data
import Result

ut = TVAny "T"
uu = TVAny "U"
ue = TVAny "E"
ur = TVAny "R"
i0 = TVInt 0
f0 = TVFloat 0

mkResult t e = TParameterized (TConst "Result") [t, e]
mkOption t = TParameterized (TConst "Option") [t]

-- Subst application
testTypeSubst1 = TestCase $
    assertEqual "{T +-> MyType} T → MyType" t2 (apply s t1)
        where t1 = TVar ut
              t2 = TConst "MyType"
              s = ut +-> t2

testTypeSubst2 = TestCase $
    assertEqual "{T +-> MyType} Result<T, U> → Result<MyType, U>" tres (apply s tsrc)
        where tsrc = mkResult (TVar ut) (TVar uu)
              tres = mkResult (TConst "MyType") (TVar uu)
              s = ut +-> (TConst "MyType")

testTypeSubst3 = TestCase $
    assertEqual "{i0 +-> i64} Result<T, U> → Result<T, U>" tsrc (apply s tsrc)
        where tsrc = mkResult (TVar ut) (TVar uu)
              s = i0 +-> tI64

testTypeSubst4 = TestCase $
    assertEqual "{i0 +-> i64} (f(i0, i0) -> i0) → f(i64, i64) -> i64" tres (apply s tsrc)
        where tsrc = TFunction [TVar i0, TVar i0] (TVar i0)
              tres = TFunction [tI64, tI64] tI64
              s = (i0 +-> tI64)

testTypeSubst5 = TestCase $
    assertEqual "{f0 +-> f64} [*f0; 16] → [*f64; 16]" tres (apply s tsrc)
        where tsrc = TArray (TPointer (TVar f0)) 16
              tres = TArray (TPointer tF64) 16
              s = (f0 +-> tF64)

testNullSubst = TestCase $
    assertEqual "{} T → T, {} i0 → i0, {} f0 → f0" tsrcs (apply nullSubst tsrcs)
        where tsrcs = [TVar ut, TVar i0, TVar f0]

-- Subst composition
-- TODO: unordered assertEqual?
testSubstComp1 = TestCase $
    assertEqual "{T +-> MyType} @@ {i0 +-> i64} → {T +-> MyType, i0 +-> i64}" sres (s2 @@ s1)
        where s1 = ut +-> (TConst "MyType")
              s2 = i0 +-> tI64
              sres = [(ut, TConst "MyType"), (i0, tI64)]

-- Subst merge
testSubstMerge1 = TestCase $
    assertEqual "merge {T +-> MyType} {U +-> OtherType} → Ok {T +-> MyType, U +-> OtherType}" (Ok sres) (merge s1 s2)
        where s1 = ut +-> (TConst "MyType")
              s2 = uu +-> (TConst "OtherType")
              sres = [(ut, TConst "MyType"), (uu, TConst "OtherType")]

testSubstMerge2 = TestCase $
    assertEqual "merge s1 s1 → Ok s1" (Ok sres) (merge s1 s1)
        where s1 = ut +-> (TConst "MyType")
              sres = s1

testSubstMergeConflict1 = TestCase $
    assertEqual "merge {T +-> MyType} {T +-> OtherType} → MergeError" (Err e) (merge s1 s2)
        where s1 = ut +-> (TConst "MyType")
              s2 = ut +-> (TConst "OtherType")
              e = MergeError s1 s2

-- Free type variables
testFtv1 = TestCase $
    assertEqual "ftv Result<Option<*T>, [E; 12]> → [T, E]" [ut, ue] (ftv t)
        where t' = mkOption (TPointer (TVar ut))
              t = mkResult t' (TArray (TVar ue) 12)

testFtv2 = TestCase $
    assertEqual "ftv (fn(i0, f0) -> T) → [i0, f0, T]" [i0, f0, ut] (ftv t)
        where t = TFunction [TVar i0, TVar f0] (TVar ut)

-- Unify tests
-- Unify nested types
testMguType1 = TestCase $
    assertEqual "mgu Result<T, E> Result<i64, E> → Ok {T +-> i64}" (Ok s) (mgu t1 t2)
        where t1 = mkResult (TVar ut) (TVar ue)
              t2 = mkResult tI64 (TVar ue)
              s = (ut +-> tI64)

testMguType2 = TestCase $
    assertEqual "mgu (fn (i0, f0) -> T) (fn (i32, f0) -> Result<i64, MyError>) → Ok { T +-> Result<...>, i0 +-> i32 }" (Ok s) (mgu t1 t2)
        where t' = mkResult tI64 (TConst "MyError")
              t1 = TFunction [TVar i0, TVar f0] (TVar ut)
              t2 = TFunction [tI32, TVar f0] t'
              s = (ut +-> t') @@ (i0 +-> tI32)

testMguType3 = TestCase $
    assertEqual "mgu [*f32; 16] [*f0; 16] → Ok { f0 +-> f32 }" (Ok s) (mgu t1 t2)
        where t1 = TArray (TPointer tF32) 16
              t2 = TArray (TPointer (TVar f0)) 16
              s = (f0 +-> tF32)

testMguType4 = TestCase $
    assertEqual "mgu T T → {}" (Ok nullSubst) (mgu (TVar ut) (TVar ut))

testMguTypeError1 = TestCase $
    assertEqual "mgu [f32; 16] Result<T, E> → UnifyError" (Err e) (mgu t1 t2)
        where t1 = TArray tF32 16
              t2 = mkResult (TVar (TVAny "T")) (TVar (TVAny "E"))
              e = UnifyError t1 t2

testMguTypeError2 = TestCase $
    assertEqual "mgu i32 i64 → UnifyError" (Err e) (mgu t1 t2)
        where t1 = tI32
              t2 = tI64
              e = UnifyError t1 t2

testMguTypeError3 = TestCase $
    assertEqual "mgu [T; 8] [i32; 4] → UnifyError" (Err e) (mgu t1 t2)
        where t1 = TArray (TVar ut) 8
              t2 = TArray tI32 4
              e = UnifyError t1 t2

testMguLengthError1 = TestCase $
    assertEqual "mgu (fn (T, U) -> R) (fn (i32) -> i32) → ArgumentCountMismatch" (Err e) (mgu t1 t2)
        where t1 = TFunction [TVar ut, TVar uu] (TVar ur)
              t2 = TFunction [tI32] tI32
              e = ArgumentCountMismatch [TVar ut, TVar uu] [tI32]

testMguIntError1 = TestCase $
    assertEqual "mgu i0 f32 → IntUnifyError" (Err e) (mgu t1 t2)
        where t1 = TVar i0
              t2 = tF32
              e = IntUnifyError i0 t2

testMguFloatError1 = TestCase $
    assertEqual "mgu f0 Result<i32, i32> → FloatUnifyError" (Err e) (mgu t1 t2)
        where t1 = TVar f0
              t2 = mkResult tI32 tI32
              e = FloatUnifyError f0 t2

testMguOccursCheckError1 = TestCase $
    assertEqual "mgu T Result<T, U> → OccursCheck" (Err e) (mgu t1 t2)
        where t1 = TVar ut
              t2 = mkResult (TVar ut) (TVar uu)
              e = OccursCheck ut t2

---- Match tests
testMatch1 = TestCase $
    assertEqual "match Result<T, U> Result<i32, i64> → Ok {T +-> i32, U +-> i64}" (Ok sres) (match t1 t2)
        where t1 = mkResult (TVar ut) (TVar uu)
              t2 = mkResult tI32 tI64
              sres = (uu +-> tI64) @@ (ut +-> tI32)


testMatch2 = TestCase $
    assertEqual "match (fn (T, [*i32; 8]) -> i64) (fn (i8, [*i32; 8]) -> i64) → Ok {T +-> i8}" (Ok sres) (match t1 t2)
        where t1 = TFunction [TVar ut, TArray (TPointer tI32) 8] tI64
              t2 = TFunction [tI8, TArray (TPointer tI32) 8] tI64
              sres = ut +-> tI8

testMatchError1 = TestCase $
    assertEqual "match Result<i32, i64> Result<T, U> → MatchError" (Err e) (match t1 t2)
        where t1 = mkResult tI32 tI64
              t2 = mkResult (TVar ut) (TVar uu)
              e = MatchError tI32 (TVar ut)

testMatchError2 = TestCase $
    assertEqual "match Result<T, U> Result<i32> → ArgumentCountMismatch" (Err e) (match t1 t2)
        where t1 = mkResult (TVar ut) (TVar uu)
              t2 = TParameterized (TConst "Result") [tI32]
              e = ArgumentCountMismatch [TVar ut, TVar uu] [tI32]

testMatchError3 = TestCase $
    assertEqual "match A B → MatchError" (Err e) (match t1 t2)
        where t1 = TConst "A"
              t2 = TConst "B"
              e = MatchError t1 t2

testMatchError4 = TestCase $
    assertEqual "match [T; 8] [i32; 4] → MatchError" (Err e) (match t1 t2)
        where t1 = TArray (TVar ut) 8
              t2 = TArray tI32 4
              e = MatchError t1 t2

main :: IO Counts
main = runTestTT $ TestList [ testTypeSubst1,
                              testTypeSubst2,
                              testTypeSubst3,
                              testTypeSubst4,
                              testTypeSubst5,
                              testNullSubst,
                              testSubstComp1,
                              testSubstMerge1,
                              testSubstMerge2,
                              testSubstMergeConflict1,
                              testFtv1,
                              testFtv2,
                              testMguType1,
                              testMguType2,
                              testMguType3,
                              testMguType4,
                              testMguTypeError1,
                              testMguTypeError2,
                              testMguTypeError3,
                              testMguLengthError1,
                              testMguIntError1,
                              testMguFloatError1,
                              testMguOccursCheckError1,
                              testMatch1,
                              testMatch2,
                              testMatchError1,
                              testMatchError2,
                              testMatchError3,
                              testMatchError4
                            ]
