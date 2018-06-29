////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Stock trend queezes
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module fFactory.StockTrends 

open System

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  MaxProfit 1
//
//  Find the highest possible profit on whole interval with only one buy-sell transaction.
//  The function returns interval with the largest profit 
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let maxProfit1Iter (lst:List<int>) =
    let mutable res = (0,0)
    if not lst.IsEmpty then
        let mutable min = lst.Head
        lst |> List.iter (function 
                            | x when x < min -> min <- x 
                            | x when (x - min) > (snd res) - (fst res) -> res <- (min, x) 
                            | _ -> ())
    res

let maxProfit1Rec = function
    | head::tail -> let rec f min res = function
                        | h::t when h < min -> t |> f h res 
                        | h::t -> t |> f min (if h - min > snd res - fst res then (min, h) else res)
                        | [] -> res
                    tail |> f head (0, 0)
    | [] -> (0,0)


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit 2: Find the highest possible profit on whole interval with any number of buy-sell tansactions.
//    
// It's not allowed to buy several times before one sell and vice versa. One purchase -> one sell and the same again...
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let maxProfit2 = function 
    | head::tail -> let rec f prev min = function
                        | h::t when h >= prev -> f h min t
                        | h::t -> (prev - min) + f h h t         // Check tail recursion
                        | [] -> 0   
                    f head head tail 
    | [] -> 0

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit 3: Find the highest possible profit on whole interval with only two of buy-sell tansactions. 
//
// It's not allowed to buy several times in a row before one sell and vice versa. One purchase -> one sell and the same again...
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let maxProfit3 verb l =
    let runMax x dif1 cont (max,dif2,acc) =                         // retuns back picking up maximums
        let max' = if x >= max then x else max                      // new max extremum
        let dif' = if max-x > dif2 then max-x else dif2             // new second maximum dif
        cont (max',dif',(dif1,dif')::acc)                           // call prev continuation

    let rec runMin min dif1 cont = function                         // goes forward picking up minimums
        | [] -> (0,0,[])                                            // guard close
        | h::t when t.IsEmpty -> cont (h,0,[])                      // turn around of last value
        | h::t -> let min' = if h < min then h else min             // new min extremum
                  let dif' = if h-min > dif1 then h-min else dif1   // new first maximum dif
                  t |> runMin min' dif' (runMax t.Head dif' cont)   // go to the next value

    match l with
    | [] -> printf "List is empty!\h"
    | _  -> let (_,_,acc) = l |> runMin l.Head 0 (fun x -> x) 
            if verb then printf "Found pairs : %A \n\n" acc
            let res = acc |> List.filter (fun x -> fst x > 0 || snd x > 0) |> List.map (fun (min,max) -> min+max)
            match res.Length with
            | 0 -> printf "It's impossible to find two bargains! \n\n"
            | _ -> printf "maxProfit of two bargain: %i \n\n" (res |> List.max)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// TODO: MaxProfit 4: Find the highest possible profit on whole interval with choosed number two of buy-sell tansactions.
//
// It's not allowed to buy several times in a row before one sell and vice versa. One purchase -> one sell and the same again...
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let rec convolute prev min list = match list with 
                                  | h::t when h >= prev -> convolute h min t
                                  | h::t when min = prev -> convolute h h t
                                  | h::t -> (min, prev)::(convolute h h t)
                                  | [] -> []
