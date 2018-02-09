module QueensTaks

open Microsoft.FSharp.Collections

let inline (>=.<) a (b, c) = a >= b && a < c
let inline (>.<) a (b,c) = a > b && a < c
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let setBishop square board =
    let (cols,rows) = (Array2D.length1 board, Array2D.length2 board)
    let markDiagonal f r incr =
        let rec markSquare x y yInc = if x < cols && y >=.< (0,rows) then board.[x,y] <- true; markSquare (x+1) (y+yInc) yInc
        match (r - incr * f) with
        |  b when b >= rows -> markSquare (b-rows+1) (rows-1) incr
        |  b1 when b1 < 0 -> markSquare -b1 0 incr
        |  b2 -> markSquare 0 b2 incr
    match square with 
    | (file, rank) when file >=.< (0, cols) && rank >=.< (0, rows) ->
        markDiagonal file rank 1
        markDiagonal file rank -1
    | (file, rank) -> failwith (sprintf """The square [%i,%i] is out of board!""" file rank)
    board

let setRook square board = 
    let (cols,rows) = (Array2D.length1 board, Array2D.length2 board)
    match square with
    | (file, rank) when file >=.< (0, cols) && rank >=.< (0, rows) ->
        board.[file,*] <- [| for i in 0..(Array2D.length2 board - 1) -> true|]
        board.[*,rank] <- [| for i in 0..(Array2D.length1 board - 1) -> true|]
    | (file, rank) -> failwith (sprintf """The square [%i,%i] is out of board!""" file rank)
    board

let setQueen square board = 
    //TODO: try to use real composition
    board |> setRook square |> setBishop square 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////







(*
////////////////////////////////////////////////////////////////////////////////////////////////////////////
// old 

let findQueens cols rows iCol iRow = 
//    let mutable board = Array2D.init n n (fun x y -> false)
    let mutable board = Array2D.create cols rows false

    // the function searches non-occupated sell on board and guarantees correct position
    let rec findNext pos = 
        match pos with
        | (x,y) when x < 0 || x > cols || y < 0 || y >= rows -> None            // unreal or beyond y border
        | (x,y) when x = cols -> (0, y + 1) |> findNext                         // move to the next row
        | (x,y) -> if  board.[x,y] then (x + 1, y) |> findNext else Some(x,y)
    
    // the function marks all beaten sells for a queen position
    let cutFields x y =
        let rec cutLine fX fY pos = 
            match pos with
            | (x,y) when (x >=.< (0, cols) && y >=.< (0, rows)) -> board.[x,y] <- true; cutLine fX fY (fX x, fY y)       //
            | _ -> ()
        board.[x,y] <- true;
        cutLine (fun x -> x - 1 ) (fun y -> y) (x - 1, y);  cutLine (fun x -> x + 1)  (fun y -> y) (x + 1, y)                    // cut row
        cutLine (fun x -> x) (fun y -> y - 1) (x, y - 1);  cutLine (fun x -> x) (fun y -> y + 1) (x, y + 1)                     // cut col
        cutLine (fun x -> x - 1) (fun y -> y - 1) (x - 1, y - 1);  cutLine (fun x -> x + 1) (fun y -> y + 1) (x + 1, y + 1);    // cut right diag
        cutLine (fun x -> x + 1) (fun y -> y - 1) (x + 1, y - 1);  cutLine (fun x -> x - 1) (fun y -> y + 1) (x - 1, y + 1);    // cut left diag;;

    // the function sets up queens on board and collect list of positions
    let rec setQueensUp p = 
        match findNext p with
        | Some(x,y) -> 
            cutFields x y 
            (x,y)::(setQueensUp (x,y))
        | None -> []

    let lst = []
    setQueensUp (iCol, iRow)

findQueens 8 8 0 0
*)