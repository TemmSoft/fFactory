module StockMarketTests

open System
open fFactory.StockTrends
open Xunit

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit1 tests
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<Theory>]
[<ClassData(typeof<maxProfit1TestDataProvider>)>]
let MaxProfit1AccUnitTest(actual, expected) =
    Assert.Equal(actual |> maxProfit1Acc , expected)

[<Theory>]
[<ClassData(typeof<maxProfit1TestDataProvider>)>]
let MaxProfit1RecUnitTest(actual, expected) =
    Assert.Equal(actual |> maxProfit1Rec , expected)
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit1 tests
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<Theory>]
[<ClassData(typeof<maxProfit2TestDataProvider>)>]
let ``MaxProfit 2`` (actual, expected) =
    Assert.Equal<int> (actual |> maxProfit2 , expected)
 
 (*     , 
[17; 18; 3; 11; 10; 10; 10; 12; 6; 4; 7; 2; 2; 2; 8; 0; 15; 7] |> maxProfit2
//[<InlineData(6, 1, 18, 3, 11, 4, 2, 20, 8, 0, 15, 7)>]
[18; 2; 6; 9; 7; 3; 11; 10; 12; 6; 19; 8; 9; 5; 17; 4; 13; 8; 6; 11; 7] |> maxProfit3 true

[1] |> maxProfit3 true
[1;2] |> maxProfit3 true
[] |> maxProfit3 true *)