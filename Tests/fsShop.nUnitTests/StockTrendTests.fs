module fsShop.nUnitTests

open NUnit.Framework
open fsShop.StockTrends
    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  MaxProfit1 tests
//
//  The tests are implemented as a  class.
//  Test cases are defined with TestCaseSource attribute and local static method as as data source
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<TestFixture; Description("Test fixture of MaxProfit 1 function")>] 
type MaxProfit1Tests() =
    // Source of test data
    static member testDataSource: seq<obj[]>  = 
                                    seq {                                       // Test cases...
                                          yield [| List<int>.Empty; (0, 0) |]
                                          yield [| [1]; (0, 0) |]
                                          yield [| [11; 7]; (0, 0) |]
                                          yield [| [5; 9; 2; 5; 7]; (2, 7) |]          
                                          yield [| [19; 2; 7; 3; 20; 4; 1; 18]; (2, 20) |]
                                        }
    // Tests of iterative version of maxProfit 1                               
    //  [<Test; Description("Test of Accumulator-based MaxProfit1 function")>] - using TestCaseSource makes using this attribute redundant
    [<TestCaseSource("testDataSource")>]
    member x.MaxProfit1AccUnitTest(actual, expected) = Assert.AreEqual(actual |> maxProfit1Iter , expected)

    // Tests of recursive version of maxProfit 1 
    [<TestCaseSource("testDataSource")>]
    member x. MaxProfit1RecUnitTest(actual, expected) = Assert.AreEqual(actual |> maxProfit1Rec , expected)
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  MaxProfit2 tests
//
//  The tests are implemented as a function.
//  Test cases are defined with TestCaseSource attribute and local static method as as data source
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
type MaxProfit2TestDataSource() =
    static member testDataSource : seq<obj[]>  = 
                                    seq {                                                                // Test cases...
                                            yield [| [6; 1; 18; 3; 0]; 17 |]    
                                            yield [| [1; 7; 18; 3; 11; 4; 2; 20; 8; 3; 15; 0]; 55 |]   
                                        }
//[<Test>]
[<TestCaseSource(typeof<MaxProfit2TestDataSource>, "testDataSource")>]
let ``MaxProfit 2`` actual expected = Assert.AreEqual(actual |> maxProfit2 , expected)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  MaxProfit3 tests
//
//  The tests are implemented as a class.
//  Test cases are defined with TestCaseSource attribute and local static method as as data source
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
[<TestFixture; Description("Test fixture of MaxProfit 3 function")>] 
type MaxProfit13ests() =
    // Source of test data
    static member DataSource: seq<obj[]>  = 
                                    seq {                                       // Test cases...
                                          yield [| List<int>.Empty; 0 |]
                                          yield [| [1]; 0 |]
                                          yield [| [11; 7]; 0 |]
                                          yield [| [5; 9; 2; 5; 7]; 9 |]          
                                          yield [| [19; 2; 7; 3; 20; 4; 1; 18]; 35 |]
                                        }
    // Tests of iterative version of maxProfit 1                               
    //  [<Test; Description("Test of Accumulator-based MaxProfit1 function")>] - using TestCaseSource makes using this attribute redundant
    [<TestCaseSource("DataSource")>]
    member x.MaxProfit3AccUnitTest(actual, expected) = Assert.AreEqual(actual |> maxProfit3 , expected)
