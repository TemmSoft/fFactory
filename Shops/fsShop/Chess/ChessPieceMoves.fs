module fsShop.ChessPieceMoves

/// chess pieces' move functions
/// they take square where a piece is set and check whether second square is biten by the piece
/// check elefant move
let eMove s1 s2 = 
    match (s1,s2) with
    | ((x,y),(x',y')) when x=x' || y=y' -> true
    | _ -> false
    
// check rook move
let rMove s1 s2 =
    match (s1,s2) with
    | ((x,y),(x',y')) when (y-x = y'-x') || (y+x = y'+x') -> true
    | _ -> false

// check queen move
let qMove s1 s2 = eMove s1 s2 || rMove s1 s2

