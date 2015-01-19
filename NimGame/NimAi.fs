module NimAi

let SetFromString (text : string)=
    let words = text.Split [|' '|]
    let board = (words |> Seq.map System.Int32.Parse) |> List.ofSeq
    (board)

let rec StringFromSet (sl:int list) = sl |> List.map (sprintf "%i") |> String.concat " "


let rec UserMove x y (board:int list) =
    match board with 
    |b::bs when x=1 && y<=b ->  let temp = b-y
                                temp::bs
    |b::bs when x=1 ->          let temp = 0
                                temp::bs
    |b::bs -> b::(UserMove (x-1) y bs)
    |[] -> []

let rec calcXor (board:int list) =
    match board with
        |x::xs -> List.fold (fun acc x -> x^^^acc) x xs
        |[]->failwith"Folderrz"


let rec del1 (board:int list) i = 
    match board with 
    |x::xs when i=x -> (x-1)::xs
    |x::xs when i<>x -> x::(del1 xs i)
    |_ ->[]

let rec setback (board:int list) i =
    match board with
    |x::xs when i>=board.Length-> printfn"%A" (x::xs)
                                  x::xs
    |x::xs when i<board.Length->  printfn"%A" (x::xs)
                                  setback (xs@[x]) (i+1)
    |_->[]

let CntZero (board:int list) i =
    match board with
    |b when i = 0 ->b
    |b -> (setback b i)
    |[]-> []

let rec del1s cnt k (board:int list) (original:int list) = 
    match board with
    |x::xs when x>0 ->  
                        let temp = x-1
                        //printfn"herp %A"(calcXor (temp::xs))
                        if calcXor (temp::xs) > 0 then del1s cnt k (temp::xs) board
                        else (CntZero (temp::xs) cnt)
    |x::xs ->del1s (cnt+1) xs.Head (xs@[k]) board
    |[]->[]
 

let AiMove (board:int list) = 
    match board with
    |board when (calcXor board)=0 ->printfn"0" 
                                    del1 board (List.max board)
    |board when (calcXor board)>0 ->printfn"not 0"
                                    del1s 0 board.Head board board
    |_ -> failwith "couldn't AI"

