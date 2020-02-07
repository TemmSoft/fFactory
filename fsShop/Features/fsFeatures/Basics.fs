module fshop.Basics

open System

// single line comments use a double slash
(* multi line comments use (* . . . *) pair
-end of multi line comment- *)

// ======== "Variables" (but not really) ==========
// The "let" keyword defines an (immutable) value
let myInt = 5
let myFloat = 3.14
let myString = "hello" //note that no types needed

//  ====================================================================
//                              Types
//  ====================================================================
type priceType = float                                                  // new 'float' type
type quantityType = int                                                 // new 'int' type

//  ====================== Strings & characters ============
let theChar = 'h'
let theString = "Here is the glory string"
let charEight = theString.[8]                                           // symbol by index
let subString = theString.[12..16]                                      // slicing operator
let strLength = theString.Length

//  ============================= Tuples ===============================
type skuType = priceType * quantityType                                 // Definition of tuple tuple
let skuZero = 11.1,15                                                   // Construct of values
let skuOne = (16.7, 10)                                                 // Construct of other tuple
let skuTwo = skuType(14.3, 12)                                          // Value of typed tuple from another typle
let skuThreeo:skuType = skuZero                                         // Value of typed tuple from another typle 
let a, b = skuZero                                                      // Deconstruct - Matching from tuple value
let (a1,a2) = skuOne                                                    // Deconstruct

//  ============================= Records ==============================
type paymentType = { Amount: float; Cur: string; Date: DateTime }       // Record type
let paymentZero = { Amount = 55.3; Cur = "RUB"; Date = DateTime.Now }   // Construct 
let paymentOne = { Amount = 55.3; Cur = "EUR"; Date = DateTime(2020, 2, 2)}
let paymentTwo = { paymentOne with Cur = "USD"}                         // Custruct of another record with partial value assignment
let paymentThree = { paymentOne with Date = DateTime(2020, 2, 2, new System.Globalization.JulianCalendar()) }
let { Amount = amnt; Cur = curOne; Date = dt } = paymentZero            // Deconstruct
let { Cur = curTwo } = paymentOne                                       // Deconstruct
let curThree = paymentTwo.Cur                                           // Deconstruct


//  ============================= Unions ===============================
type lightType = 
    | Day 
    | Night 
type shapeType = Circle of int | Rectangle of int * int                 // Inline union
type choiceType<'a> = Choice1 of 'a | Choice2 of 'a * 'a
type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T
let lightDay = Day                                                      // Construct
let shapeOne = Circle 16
let shapeTwo = Rectangle (15,20)
let choiceOne:choiceType<lightType> = Choice1 Day
let choiceTwo:choiceType<lightType> = Choice2 (Night, Night)
                                                                        // Deconstruct
let deLight = function
    | Day -> printf "Sunlight! \n"     
    | Night -> printf "Moonlight! \n"
let deShape = function                                  
    | Circle r -> printf "Circle of radius %i \n" r                        
    | Rectangle (h,w) -> printf "Rectangle with hight of %i and width of %i \n" h w
let deChoice = function
    | Choice1 ch1 -> printf "Choice one %A \n" ch1.ToString                         
    | Choice2 (ch2,ch3) -> printf "hight of %i and width of %i" ch2 ch3

let a = choiceOne
deLight lightDay
deShape shapeOne
deShape shapeTwo
deChoice choiceOne





//  ============================= Classes ==============================
type MyClass(initX:int) =
   let x = initX
   member this.Method() = printf "x=%i" x

let a = (1,1)

let c = Circle 99
let c' = Rectangle (2,1)
let d = Month
let e = Choice1 "a"
let myVal = MyClass 99
myVal.Method()






// ======== Lists ============
let twoToFive = [2;3;4;5]                       // Square brackets create a list with semicolon delimiters.
let oneToFive = 1 :: twoToFive                  // :: creates list with new 1st element. The result is [1;2;3;4;5]
let zeroToFive = [0;1] @ twoToFive              // @ concats two lists
let forToFive = [for x in 0..5 -> x]            // creates list with iterator 
// IMPORTANT: commas are never used as delimiters, only semicolons!
let a = List.zip zeroToFive forToFive
// ======== Functions ========
// The "let" keyword also defines a named function.
let square x = x * x // Note that no parens are used.
square 3 // Now run the function. Again, no parens.

let add x y = x + y // don't use add (x,y)! It means something completely different.
add 2 3 // Now run the function.

// to define a multiline function, just use indents. No semicolons needed.
let evens list =
    let isEven x = x%2 = 0 // Define "isEven" as an inner ("nested") function
    List.filter isEven list // List.filter is a library function with two parameters: a boolean function and a list to work on
evens oneToFive // Now run the function

// You can use parens to clarify precedence. In this example, do "map" first, with two args, then do "sum" on the result.
// Without the parens, "List.map" would be passed as an arg to List.sum
let sumOfSquaresTo100 = List.sum ( List.map square [1..100] )

// You can pipe the output of one operation to the next using "|>"
// Here is the same sumOfSquares function written using pipes
let sumOfSquaresTo100piped = 
    [1..100] |> List.map square |> List.sum // "square" was defined earlier

// you can define lambdas (anonymous functions) using the "fun" keyword
let sumOfSquaresTo100withFun =
    [1..100] |> List.map (fun x->x*x) |> List.sum
// In F# returns are implicit -- no "return" needed. A function always returns the value of the last expression used.

let rec factorial = function
    | x when x = 0 -> 1
    | y -> y * factorial (y-1)

let z = factorial 31

// ======== Pattern Matching ========
// Match..with.. is a supercharged case/switch statement.
let simplePatternMatch =
    let x = "a"
    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is something else" // underscore matches anything

// Some(..) and None are roughly analogous to Nullable wrappers
let validValue = Some(99)
let invalidValue = None

// In this example, match..with matches the "Some" and the "None", and also unpacks the value in the "Some" at the same time.
let optionPatternMatch input =
    match input with
    | Some i -> printfn "input is an int=%d" i
    | None -> printfn "input is missing"
optionPatternMatch validValue
optionPatternMatch invalidValue

// Short form of pattern matching uses 'function' keyword. This notation is useful in lambdas.
// The notation force to omit function parameter used in pattern matching, so the last parameter of funcion call will be applied for pattern matching.
let shortPaternMatching = function
    | x when x % 2 = 0 -> printfn "even input"
    | _ -> printfn "odd input"
shortPaternMatching 11 


// ========= Complex Data Types =========
// Tuple types are pairs, triples, etc. Tuples use commas.
let twoTuple = 1,2
let threeTuple = "a",2,true

// Record types have named fields. Semicolons are separators.
type Person = {First:string; Last:string}
let person1 = {First="john"; Last="Doe"}

// Union types have choices. Vertical bars are separators.
type Temp =
    | DegreesC of float
    | DegreesF of float
let temp = DegreesF 98.6

// Types can be combined recursively in complex ways.
// E.g. here is a union type that contains a list of the same type:
type Employee =
    | Worker of Person
    | Manager of Employee list
let jdoe = {First="John";Last="Doe"}
let worker = Worker jdoe

// ========= Printing =========
// The printf/printfn functions are similar to the
// Console.Write/WriteLine functions in C#.
printfn "Printing an int %i, a float %f, a bool %b" 1 2.0 true
printfn "A string %s, and something generic %A" "hello" [1;2;3;4]
// all complex types have pretty printing built in
printfn "twoTuple=%A,\nPerson=%A,\nTemp=%A,\nEmployee=%A"
twoTuple person1 temp worker
// There are also sprintf/sprintfn functions for formatting data
// into a string, similar to String.Format.

And with that, let's start by comparing some simple F# code with the equivalent C# code.



