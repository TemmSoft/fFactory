module fFactory.StockTrends 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Stock trend queezes
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

open System

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// 1. Find the best buy-sell interval
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let bestSell lst =
    let mutable res = (0,0)
    let mutable min = Int32.MaxValue
    lst |> List.iter (function | x when x < min -> min <- x | x when (x - min) > (snd res) - (fst res) -> res <- (min, x) | _ -> ())
    res

[6; 1; 18; 3; 11; 4; 2; 20;8; 0; 15; 7] |> bestSell;;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// 2. Find the highest possible profit on whole interval. 
//    It's not allowed to buy several times before one sell and vice versa. One purchase -> one sell and the same again...
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let rec f prev min l = match l with 
                        | h::t when h >= prev -> f h min t
                        | h::t -> (prev - min) + f h h t
                        | [] -> 0
let maxProfitTwo = function h::t -> f h h t | [] -> 0

[18; 3; 11; 10; 12; 6; 4; 7; 2; 8; 0; 15; 7] |> maxProfitTwo;;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let maxProfitOne lst =
    let mutable prev = List.head lst
    let mutable min = prev
    [for x in lst do yield (match x with 
                            | x when x >= prev -> prev <- x ; 0 
                            | x when x < prev -> let a = prev - min;  
                                                 min <- x; prev <- x; a)] |> List.filter (fun x -> x > 0) |> List.fold (fun acc x -> acc + x) 0

[18; 3; 11; 10; 12; 6; 4; 2; 8; 0; 15; 7] |> maxProfitOne;;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let maxProfitTwo lst =
    let mutable prev = List.head lst
    let mutable min = prev
    lst |> List.fold (fun acc x -> match x with 
                                   | x when x >= prev -> prev <- x ; acc 
                                   | x when x < prev -> let a = prev - min; 
                                                        min <- x; prev <- x; acc + a) 0

[18; 3; 11; 10; 12; 6; 4; 2; 8; 0; 15; 7] |> maxProfitTwo;;
