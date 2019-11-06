module fshop.Basics

open System

type price = float
type quantity = int

type sku = price * quantity

let sss:sku = (16.7, 10)

type news = { date: DateTime
              text: string 
              ch: int}

let nnn = { date = DateTime(1933, 3, 3); text = "Hire the news"; ch = 51 }
let nn = { nnn with ch = 22 }

type Message = 
    | C of news
    | Sku

let iii = sku(14.3, 12)

//
// Types
//
type A = int * int
type B = {FirstName:string; LastName:string}
type C = Circle of int | Rectangle of int * int
type D = Day | Month | Year
type E<'a> = Choice1 of 'a | Choice2 of 'a * 'a

type MyClass(initX:int) =
   let x = initX
   member this.Method() = printf "x=%i" x

let a = (1,1)
let b = { FirstName="Bob"; LastName="Smith" } 
let c = Circle 99
let c' = Rectangle (2,1)
let d = Month
let e = Choice1 "a"
let myVal = MyClass 99
myVal.Method()
let aa = (1,1)                                  // "construct"
let (a1,a2) = a                                // "deconstruct"

let bb = { FirstName="Bob"; LastName="Smith" }  // "construct"
let { FirstName = b1 } = b                     // "deconstruct" 

let cc = Circle 99                              // "construct"
match cc with                                   
| Circle c1 -> printf "circle of radius %i" c1 // "deconstruct"
| Rectangle (c2,c3) -> printf "%i %i" c2 c3    // "deconstruct"

let cc' = Rectangle (2,1)                       // "construct"
match cc' with                                   
| Circle c1 -> printf "circle of radius %i" c1 // "deconstruct"
| Rectangle (c2,c3) -> printf "%i %i" c2 c3    // "deconstruct"

type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T

