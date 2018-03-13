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
let maxProfit2_One = function 
    | head::tail -> let rec f prev min l = match l with 
                                           | h::t when h >= prev -> f h min t
                                           | h::t -> (prev - min) + f h h t         // Check tail recursion
                                           | [] -> 0   
                    f head head tail 
    | [] -> 0

[17; 18; 3; 11; 10; 10; 10; 12; 6; 4; 7; 2; 2; 2; 8; 0; 15; 7] |> maxProfit2_One;;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let maxProfit2_Two lst =
    let mutable prev = List.head lst
    let mutable min = prev
    [for x in lst do yield (match x with 
                            | x when x >= prev -> prev <- x ; 0 
                            | x when x < prev -> let a = prev - min;  
                                                 min <- x; prev <- x; a)] |> List.filter (fun x -> x > 0) |> List.fold (fun acc x -> acc + x) 0

[18; 3; 11; 10; 12; 6; 4; 2; 8; 0; 15; 7] |> maxProfit2_Two;;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let maxProfit2_Three lst =
    let mutable prev = List.head lst
    let mutable min = prev
    lst |> List.fold (fun acc x -> match x with 
                                   | x when x >= prev -> prev <- x ; acc 
                                   | x when x < prev -> let a = prev - min; 
                                                        min <- x; prev <- x; acc + a) 0

[18; 3; 11; 10; 12; 6; 4; 2; 8; 0; 15; 7] |> maxProfit2_Three;;


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// 3. Find the highest possible profit with only two non-itertransaction interval. 
//    It's not allowed to buy several times before one sell and vice versa. One purchase -> one sell and the same again...
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
let maxProfit3_One (lst: List<int>) = 
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// 2    accumulator & continuation
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

//*)
(*
                                                   | (min,max)::[] -> 
                                                   
                                                   con





                                                   
                                                   
                                                   t indx (max-min) [(maxIn,max-min)]
                                                   | _::t -> let newMax = stepForward maxIn indx
                                                             t |> loop (indx+1) newMax acc (fun x -> cont (indx+1) maxIn acc)
*)





    //let rec getMax i j max m = function
    //    | h::t -> matrix.[i,j] <- snd extrems.[j] - fst extrems.[i]; let max' = goNext (i+1) j max; 
    //              if matrix.[i,j] > max' then matrix.[i,j] else max'
    //    | [] -> max
    
 //   let indx = 3
 //   let maxOut = (mutable matrix.[0..indx,indx]) |>  Array.iteri (fun i x -> x <- x + i)


    //let a = 0;
  //  List.foldi (fun i x -> 
    //    | [] ->
            
    //                            matrix.[i,indx] <- snd extrems.[indx] - fst extrems.[i]
    //                            let max' = matrix.[0..indx,indx] |> Array.max 
    //                            if max' > max then max' else max




let maxProfit3_3 a = 
    let rec loop min dif1 cont = function
       | h::t when h < min  -> t |> loop h dif1 (function
                                                         | (max,dif2,acc) when h >= max -> cont (h,dif2,(dif1,dif2)::acc)               // new max
                                                         | (max,dif2,acc) when max-h > dif2 -> cont (max,max-h,(dif1,max-h)::acc)       // new dif2
                                                         | (max,dif2,acc) -> cont (max,dif2,(dif1,dif2)::acc))
       | h::t when h-min > dif1 -> t |> loop min (h-min) (function
                                                         | (max,dif2,acc) when h >= max -> cont (h,dif2,(dif1,dif2)::acc)               // new max
                                                         | (max,dif2,acc) when max-h > dif2 -> cont (max,max-h,(dif1,max-h)::acc)       // new dif2
                                                         | (max,dif2,acc) -> cont (max,dif2,(dif1,dif2)::acc))
       | h::t when t.IsEmpty -> cont (h,0,[])
       | h::t -> t |> loop min dif1 (function
                                                         | (max,dif2,acc) when h >= max -> cont (h,dif2,(dif1,dif2)::acc)               // new max
                                                         | (max,dif2,acc) when max-h > dif2 -> cont (max,max-h,(dif1,max-h)::acc)       // new dif2
                                                         | (max,dif2,acc) -> cont (max,dif2,(dif1,dif2)::acc))
       | _ -> cont (0,0,[])
    let (_,_,acc) = loop 1000 0 (fun x -> x) a
    printf "Aaa: %A of lengt: %i \n" ( acc) acc.Length
    acc |> List.map (fun (min,max) -> min+max) |> List.max
    

[18; 2; 6; 9; 7; 3; 11; 10; 12; 6; 8; 9; 5; 17; 4; 13; 8; 0; 11; 7] |> maxProfit3_3

///
let maxProfit3 (l:List<int>) =
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
    if l.Length > 1 then
        let (_,_,acc) = l |> runMin l.Head 0 (fun x -> x) 
        printf "Pairs: %A \n\n" acc
        if acc.Length > 0 then 
            let res = acc |> List.filter (fun x -> fst x > 0 && snd x > 0) |> List.map (fun (min,max) -> min+max) |> List.max
            printf "maxProfit of two bargain: %i \n\n" res 
        else 
            printf "You don't have two bargains! \n\n"
    else
        printf "The list is too short! \n\n"



[18; 2; 6; 9; 7; 3; 11; 10; 12; 6; 8; 9; 5; 17; 4; 13; 8; 0; 11; 7] |> maxProfit3
[1] |> maxProfit3
[1;2] |> maxProfit3
[] |> maxProfit3