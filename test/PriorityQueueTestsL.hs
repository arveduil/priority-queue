module PriorityQueueTestsL where
    import Test.QuickCheck
    import qualified Data.PriorityQueueBT as QBT
    import qualified Data.PriorityQueueL as QL
    import Data.List
    import Test.HUnit
        

    testL = TestCase $ assertEqual "test push1 empty Queue" (QL.PQueue [1]) ( QL.pqPush 1 (QL.PQueue []) )


    runTests :: IO ()
    runTests =   runTestTT testL 