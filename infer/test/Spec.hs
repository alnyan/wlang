import Test.HUnit

import Error
import Unify
import Subst
import Type

-- Subst application
testTypeSubst1 = TestCase $
    assertEqual "{T +-> MyType} T → MyType" t2 (apply s t1)
        where t1 = TVar (TVAny "T")
              t2 = TConst "MyType"
              s = (TVAny "T") +-> t2

testTypeSubst2 = TestCase $
    assertEqual "{T +-> MyType} Result<T, U> → Result<MyType, U>" tres (apply s tsrc)
        where ut = TVAny "T"
              uu = TVAny "U"
              tsrc = TParameterized (TConst "Result") [TVar ut, TVar uu]
              tres = TParameterized (TConst "Result") [TConst "MyType", TVar uu]
              s = ut +-> (TConst "MyType")

testTypeSubst3 = TestCase $
    assertEqual "{i0 +-> i64} Result<T, U> → Result<T, U>" tsrc (apply s tsrc)
        where ut = TVAny "T"
              uu = TVAny "U"
              tsrc = TParameterized (TConst "Result") [(TVar ut), (TVar uu)]
              s = (TVInt 0) +-> (TConst "i64")

testTypeSubst4 = TestCase $
    assertEqual "{i0 +-> i64} (f(i0, i0) -> i0) → f(i64, i64) -> i64" tres (apply s tsrc)
        where i0 = TVInt 0
              tsrc = TFunction [TVar i0, TVar i0] (TVar i0)
              tres = TFunction [tI64, tI64] tI64
              s = (i0 +-> tI64)

testTypeSubst5 = TestCase $
    assertEqual "{f0 +-> f64} [*f0; 16] → [*f64; 16]" tres (apply s tsrc)
        where f0 = TVFloat 0
              tsrc = TArray (TPointer (TVar f0)) 16
              tres = TArray (TPointer tF64) 16
              s = (f0 +-> tF64)

testNullSubst = TestCase $
    assertEqual "{} T → T, {} i0 → i0, {} f0 → f0" tsrcs (apply nullSubst tsrcs)
        where ut = TVAny "T"
              ui = TVInt 0
              uf = TVFloat 0
              tsrcs = [TVar ut, TVar ui, TVar uf]

-- Subst composition
-- TODO: unordered assertEqual?
testSubstComp1 = TestCase $
    assertEqual "{T +-> MyType} @@ {i0 +-> i64} → {T +-> MyType, i0 +-> i64}" sres (s2 @@ s1)
        where s1 = (TVAny "T") +-> (TConst "MyType")
              s2 = (TVInt 0 +-> (TConst "i64"))
              sres = [(TVAny "T", TConst "MyType"), (TVInt 0, TConst "i64")]

-- Free type variables
testFtv1 = TestCase $
    assertEqual "ftv Result<Option<*T>, [E; 12]> → [T, E]" [ut, ue] (ftv t)
        where ut = TVAny "T"
              ue = TVAny "E"
              option = TParameterized (TConst "Option") [TPointer (TVar ut)]
              t = TParameterized (TConst "Result") [option, TArray (TVar ue) 12]

testFtv2 = TestCase $
    assertEqual "ftv (fn(i0, f0) -> T) → [i0, f0, T]" [i0, f0, ut] (ftv t)
        where i0 = TVInt 0
              f0 = TVFloat 0
              ut = TVAny "T"
              t = TFunction [TVar i0, TVar f0] (TVar ut)

-- Unify tests
-- Unify nested types
testMguType1 = TestCase $
    assertEqual "mgu Result<T, E> Result<i64, E> → Ok {T +-> i64}" (Ok s) (mgu t1 t2)
        where ut = TVAny "T"
              ue = TVAny "E"
              t1 = TParameterized (TConst "Result") [TVar ut, TVar ue]
              t2 = TParameterized (TConst "Result") [tI64, TVar ue]
              s = (ut +-> tI64)

testMguType2 = TestCase $
    assertEqual "mgu (fn (i0, f0) -> T) (fn (i32, f0) -> Result<i64, MyError>) → Ok { T +-> Result<...>, i0 +-> i32 }" (Ok s) (mgu t1 t2)
        where ut = TVAny "T"
              i0 = TVInt 0
              f0 = TVFloat 0
              result = TParameterized (TConst "Result") [tI64, TConst "MyError"]
              t1 = TFunction [TVar i0, TVar f0] (TVar ut)
              t2 = TFunction [tI32, TVar f0] result
              s = (ut +-> result) @@ (i0 +-> tI32)

testMguType3 = TestCase $
    assertEqual "mgu [*f32; 16] [*f0; 16] → Ok { f0 +-> f32 }" (Ok s) (mgu t1 t2)
        where f0 = TVFloat 0
              t1 = TArray (TPointer tF32) 16
              t2 = TArray (TPointer (TVar f0)) 16
              s = (f0 +-> tF32)

testMguTypeError1 = TestCase $
    assertEqual "mgu [f32; 16] Result<T, E> → UnifyError" (Err e) (mgu t1 t2)
        where t1 = TArray tF32 16
              t2 = TParameterized (TConst "Result") [TVar (TVAny "T"), TVar (TVAny "E")]
              e = UnifyError t1 t2

testMguTypeError2 = TestCase $
    assertEqual "mgu i32 i64 → UnifyError" (Err e) (mgu t1 t2)
        where t1 = tI32
              t2 = tI64
              e = UnifyError t1 t2

testMguLengthError1 = TestCase $
    assertEqual "mgu (fn (T, U) -> R) (fn (i32) -> i32) → ArgumentCountMismatch" (Err e) (mgu t1 t2)
        where ut = TVAny "T"
              ue = TVAny "E"
              ur = TVAny "R"
              t1 = TFunction [TVar ut, TVar ue] (TVar ur)
              t2 = TFunction [tI32] tI32
              e = ArgumentCountMismatch [TVar ut, TVar ue] [tI32]

testMguIntError1 = TestCase $
    assertEqual "mgu i0 f32 → IntUnifyError" (Err e) (mgu t1 t2)
        where i0 = TVInt 0
              t1 = TVar i0
              t2 = tF32
              e = IntUnifyError i0 t2

testMguFloatError1 = TestCase $
    assertEqual "mgu f0 Result<i32, i32> → FloatUnifyError" (Err e) (mgu t1 t2)
        where f0 = TVFloat 0
              t1 = TVar f0
              t2 = TParameterized (TConst "Result") [tI32, tI32]
              e = FloatUnifyError f0 t2

testMguOccursCheckError1 = TestCase $
    assertEqual "mgu T Result<T, U>" (Err e) (mgu t1 t2)
        where ut = TVAny "T"
              uu = TVAny "U"
              t1 = TVar ut
              t2 = TParameterized (TConst "Result") [TVar ut, TVar uu]
              e = OccursCheck ut t2

main :: IO Counts
main = runTestTT $ TestList [ testTypeSubst1,
                              testTypeSubst2,
                              testTypeSubst3,
                              testTypeSubst4,
                              testTypeSubst5,
                              testNullSubst,
                              testSubstComp1,
                              testFtv1,
                              testFtv2,
                              testMguType1,
                              testMguType2,
                              testMguType3,
                              testMguTypeError1,
                              testMguTypeError2,
                              testMguLengthError1,
                              testMguIntError1,
                              testMguFloatError1,
                              testMguOccursCheckError1
                            ]
