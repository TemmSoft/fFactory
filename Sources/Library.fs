module fFactory


//module Say =
//    let hello name =
//        printfn "Hello %s" name

//    let a = 12
//    let b = "gggoo"


///////////////////////////////////////////////////////////////////
//
// Continuaton passing
//
///////////////////////////////////////////////////////////////////
(fun acc -> (f 1)::acc) ((fun acc -> (f 2)::acc) ((fun acc -> (f 3)::acc) ((fun acc -> (f 4)::acc) ((fun acc -> (f 5)::acc) (id [])
                                                                                                   )
                                                                          )
                                                 )
                        ) |> ignore

let map f l =
    let rec loop cont = function
        | [] -> cont []
        | x::xs -> loop ( fun acc -> cont (f x::acc) ) xs
    loop id l

[1;2;3] |> map (fun x -> x * 2);;

type Tree =
| Node of int * Tree * Tree
| Tip of int

let rec sizeCont tree cont =
    match tree with
    | Tip _ -> cont 1
    | Node(_, treeLeft, treeRight) ->
        sizeCont treeLeft (fun leftSize ->
        sizeCont treeRight (fun rightSize ->
        cont (leftSize + rightSize)))

let size tree = sizeCont tree (fun x -> x)

let rec sizeContAcc acc tree cont =
    match tree with
    | Tip _ -> cont (1 + acc)
    | Node (_, treeLeft, treeRight) ->
        sizeContAcc acc treeLeft (fun accLeftSize ->
        sizeContAcc accLeftSize treeRight cont)

let sizeAcc tree = sizeContAcc 0 tree (fun x -> x)

let t = Node(1,Node(2,Node(3,Tip(4),Tip(5)),Tip(6)),Node(7,Tip(8),Tip(9)))

t |> sizeAcc

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string
    // The implementation of this property is tail recursive.
    member chain.Length =
        let rec loop c acc =
            match c with
            | ChainNode(_, _, subChain) -> loop subChain (acc + 1)
            | ChainEnd _ -> acc
        loop chain 0

let c = ChainNode(1,"one",ChainNode( 2,"two",ChainNode(3,"three",ChainEnd("end"))))

c.Length


let f' i p =
    let rec loop i p k =
        match p with
        | []    -> k []
        | x::xs -> loop (i*i) xs (fun a -> k ((x+i)::a))
    loop i p id

[1;2;3] |> f' 2 

