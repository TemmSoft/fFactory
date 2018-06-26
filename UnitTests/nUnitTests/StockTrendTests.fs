module nUnitTests.StockTrendTests

open fFactory.StockTrends
open NUnit.Framework


type maxProfit1TestDataProvider () =    
    let values : seq<obj[]>  = seq {
                                      yield [| List<int>.Empty; (0, 0) |]                                // 1st test case
                                      yield [| [1]; (0, 0) |]                               // 2nd test case
                                      yield [| [11; 7]; (0, 0) |]                           // 3rd test case
                                      yield [| [5; 9; 2; 5; 7]; (2, 7) |] 
                                      yield [| [19; 2; 7; 3; 20; 4; 1; 18]; (2, 20) |]      // 4th test case
                                   }
    interface seq<obj[]> with
        member this.GetEnumerator () = values.GetEnumerator()
        member this.GetEnumerator () = values.GetEnumerator() :> System.Collections.IEnumerator

type maxProfit2TestDataProvider () =    
    let values : seq<obj[]>  = seq {
                                      yield [| [6; 1; 18; 3; 0]; 17 |]    // 1st test case
                                      yield [| [1; 7; 18; 3; 11; 4; 2; 20; 8; 3; 15; 0]; 1 |]    // 1st test case
                                   }
    interface seq<obj[]> with
        member this.GetEnumerator () = values.GetEnumerator()
        member this.GetEnumerator () = values.GetEnumerator() :> System.Collections.IEnumerator


//let maxProfit1TestDataProvider1 = 
    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit1 tests
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<TestFixture; Description("Test fixture of MaxProfit 1 function")>] 
type MaxProfit1Tests() =
    member x.Actual = [19; 2; 7; 3; 20; 4; 1; 18];
    member x.Expect = (2, 20)

    //[<TestFixtureSetup>]
    //member x.InitTestFixture() =


    [<Test; Description("Accumulator-based function test")>]
    member x.MaxProfit1AccUnitTest() =
        Assert.AreEqual(x.Actual |> maxProfit1Acc , x.Expect)


    [<Test; Description("Recursive-based function test")>]
    member x. MaxProfit1RecUnitTest() =
        Assert.AreEqual(x.Actual |> maxProfit1Rec , x.Expect)
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit2 tests
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//[<Test>]
//[<TestCaseSource("maxProfit2TestDataProvider")>]
//let ``MaxProfit 2`` (actual, expected) =
//    Assert.AreEqual<int> (actual |> maxProfit2 , expected)
