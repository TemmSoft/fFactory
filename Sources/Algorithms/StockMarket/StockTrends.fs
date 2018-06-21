////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Stock trend queezes
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module fFactory.StockTrends 

open System

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// MaxProfit 1: Find the highest possible profit on whole interval with only one buy-sell transaction.
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let maxProfit1 (lst:List<int>) =
    let mutable res = (0,0)
    let mutable min = lst.Head
    lst |> List.iter (function | x when x < min -> min <- x | x when (x - min) > (snd res) - (fst res) -> res <- (min, x) | _ -> ())
    res

[6; 1; 18; 3; 11; 4; 2; 20;8; 0; 15; 7] |> maxProfit1

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

[17; 18; 3; 11; 10; 10; 10; 12; 6; 4; 7; 2; 2; 2; 8; 0; 15; 7] |> maxProfit2

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


[18; 2; 6; 9; 7; 3; 11; 10; 12; 6; 19; 8; 9; 5; 17; 4; 13; 8; 6; 11; 7] |> maxProfit3 true

[1] |> maxProfit3 true
[1;2] |> maxProfit3 true
[] |> maxProfit3 true

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let maxProfit3_One intervals (lst: List<int>) = 
    let extrems = convolute lst.Head lst.Head lst.Tail
    let mutable acc = List<int*int>.Empty
    let mutable matrix = Array2D.create extrems.Length extrems.Length 0

    let updateMtrx max indx =  for i = 0 to indx do matrix.[i,indx] <- snd extrems.[indx] - fst extrems.[i]; 
                               let max' = matrix.[0..indx,indx] |> Array.max 
                               if max' > max then max' else max

    let rec aggregate maxIn indx restXtrms = 
        match restXtrms with       
             | _::t -> let maxOut = aggregate (updateMtrx maxIn indx) (indx+1) t
                       let max'' =  matrix.[(indx),(indx)..(Array2D.length2 matrix - 1)] |> Array.max
                       let res = if max'' > maxOut then max'' else maxOut
                       if indx > 0 then  acc <- (maxIn,res)::acc else ()
                       res
             | [] -> 0


    aggregate 0 0 extrems |> ignore
    let res = acc |> List.map (fun (x,y) -> x + y) |> List.max

    printfn "Input: %A" lst
    printfn "Extrems: %A" extrems
    printfn "Matrix:\n %A" matrix
    printfn "Partials: %A" acc
    printfn "Result: %A" res


let rec convolute1 prev min list = match list with 
                                  | h::t when h >= prev -> convolute1 h min t
                                  | h::t when min = prev -> convolute1 h h t
                                  | h::t -> (min, prev)::(convolute1 h h t)
                                  | [] -> []

let maxProfit3_Two (lst: List<int>) = 
    let extrems = convolute1 lst.Head lst.Head lst.Tail
    let mutable matrix = Array2D.create extrems.Length extrems.Length 0

    let stepForward max indx = for i = 0 to indx do matrix.[i,indx] <- snd extrems.[indx] - fst extrems.[i]; 
                               let max' = matrix.[0..indx,indx] |> Array.max 
                               if max' > max then max' else max
    let stepBackward i max =
                           let max' =  matrix.[(i),(i)..(Array2D.length2 matrix - 1)] |> Array.max
                           if max' > max then max' else max

    let aggregate = function
        | [] -> List<int*int>.Empty
        | (eMin,eMax)::[] -> [((eMax-eMin),0)]
        | (eMin,eMax)::t -> 
                let rec loop indx maxIn acc cont = function//(*
                                                   | (min,max)::[] -> cont (indx, (max-min), (maxIn,max-min)::acc)
                                                   | _::t -> let newMax = stepForward maxIn indx
                                                             t |> loop (indx+1) newMax acc (fun x-> cont ((indx+1), maxIn , acc))

                t |> loop 1 (eMax-eMin) [] stepBackward

    let partials = extrems |> aggregate 
    let res = partials|> List.map (fun (x,y) -> x + y) |> List.max

    printfn "Input: %A" lst
    printfn "Extrems: %A" extrems
    printfn "Matrix:\n %A" matrix
    printfn "Partials: %A" partials
    printfn "Result: %A" res

[18; 2; 6; 9; 7; 3; 11; 10; 12; 6; 8; 9; 5; 17; 4; 13; 8; 0; 11; 7] |> maxProfit3_Two
