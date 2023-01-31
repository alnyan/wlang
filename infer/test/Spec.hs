import Test.HUnit
import Lib

-- Subst application
testTypeSubst1 :: Test
testTypeSubst1 = TestCase $
    assertEqual "{T +-> MyType} T → MyType" t2 (apply s t1)
        where t1 = TVar (TVAny "T")
              t2 = TConst "MyType"
              s = ((TVAny "T") +-> t2)

testTypeSubst2 :: Test
testTypeSubst2 = TestCase $
    assertEqual "{T +-> MyType} Result<T, U> → Result<MyType, U>" tres (apply s tsrc)
        where ut = TVAny "T"
              uu = TVAny "U"
              tsrc = TParameterized (TConst "Result") [(TVar ut), (TVar uu)]
              tres = TParameterized (TConst "Result") [(TConst "MyType"), (TVar uu)]
              s = (ut +-> (TConst "MyType"))

testTypeSubst3 :: Test
testTypeSubst3 = TestCase $
    assertEqual "{i0 +-> i64} Result<T, U> → Result<T, U>" tsrc (apply s tsrc)
        where ut = TVAny "T"
              uu = TVAny "U"
              tsrc = TParameterized (TConst "Result") [(TVar ut), (TVar uu)]
              s = ((TVInt 0) +-> (TConst "i64"))

testNullSubst :: Test
testNullSubst = TestCase $
    assertEqual "{} T → T, {} i0 → i0, {} f0 → f0" tsrcs (apply nullSubst tsrcs)
        where ut = TVAny "T"
              ui = TVInt 0
              uf = TVFloat 0
              tsrcs = [TVar ut, TVar ui, TVar uf]

-- Subst composition
-- TODO: unordered assertEqual?
testSubstComp1 :: Test
testSubstComp1 = TestCase $
    assertEqual "{T +-> MyType} @@ {i0 +-> i64} → {T +-> MyType, i0 +-> i64}" sres (s2 @@ s1)
        where s1 = ((TVAny "T") +-> (TConst "MyType"))
              s2 = ((TVInt 0 +-> (TConst "i64")))
              sres = [((TVAny "T"), (TConst "MyType")), ((TVInt 0), (TConst "i64"))]

-- Free type variables
testFtv1 :: Test
testFtv1 = TestCase $
    assertEqual "ftv Result<Option<T>, E> → [T, E]" [ut, ue] (ftv t)
        where ut = TVAny "T"
              ue = TVAny "E"
              option = (TParameterized (TConst "Option") [TVar ut])
              t = (TParameterized (TConst "Result") [option, TVar ue])

main :: IO Counts
main = runTestTT $ TestList [ testTypeSubst1,
                              testTypeSubst2,
                              testTypeSubst3,
                              testNullSubst,
                              testSubstComp1,
                              testFtv1
                            ]
