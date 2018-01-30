module QueensTaks

open Microsoft.FSharp.Collections

let inline (>=.<) a (b,c) = a >= b && a < c

let findQueens cols rows iCol iRow = 
//    let mutable board = Array2D.init n n (fun x y -> false)
    let mutable board = Array2D.create cols rows false

    // the function searches non-occupated sell on board and guarantees correct position
    let rec findNext pos = 
        match pos with
        | (x,y) when x < 0 || x > cols || y < 0 || y >= rows -> None        // unreal or beyond y border
        | (x,y) when x = cols -> (0, y + 1) |> findNext                         // move to the next row
        | (x,y) -> if  board.[x,y] then (x + 1, y) |> findNext else Some(x,y)
    
    // the function marks all beaten sells for a queen position
    let cutFields x y =
        let rec cutLine fX fY pos = 
            match pos with
            | (x,y) when (x >=.< (0, cols) && y >=.< (0, rows)) -> board.[x,y] <- true; cutLine fX fY (fX x, fY y)       //
            | _ -> ()
        board.[x,y] <- true;
        cutLine (fun x -> x - 1) (fun y -> y) (x - 1, y);  cutLine (fun x -> x + 1)  (fun y -> y) (x + 1, y)                    // cut row
        cutLine (fun x -> x) (fun y -> y - 1) (x, y - 1);  cutLine (fun x -> x) (fun y -> y + 1) (x, y + 1)                     // cut col
        cutLine (fun x -> x - 1) (fun y -> y - 1) (x - 1, y - 1);  cutLine (fun x -> x + 1) (fun y -> y + 1) (x + 1, y + 1);    // cut right diag
        cutLine (fun x -> x + 1) (fun y -> y - 1) (x + 1, y - 1);  cutLine (fun x -> x - 1) (fun y -> y + 1) (x - 1, y + 1);    // cut left diag

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