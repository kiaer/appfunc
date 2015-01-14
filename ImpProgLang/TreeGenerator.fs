module TreeGenerator

open System

type 'a Tree = Node of 'a * ('a Tree list)
type Extent = (float*float) list

let movetree ((Node((label, x), subtrees)), x':float) =  Node((label, x + x'), subtrees)

let moveextent ((e:Extent), (x: float)) : Extent = List.map (fun (p, q) -> (p + x, q + x)) e 

let rec merge = function
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p,_) :: ps, (_,q) :: qs) -> (p,q) :: merge (ps, qs)

let rec merge' (es: (Extent*Extent)) : (Extent) = 
    match es with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p,_) :: ps, (_,q) :: qs) -> (p,q) :: merge' (ps, qs)

let mergelist (es:Extent list) : Extent =
    List.fold (fun acc elem -> merge (acc, elem)) [] es

let rmax (p:float) (q:float) = if p > q then p else q

let rec fit (e:(Extent*Extent)) : float =
    match e with
    | ((_, p) :: ps), ((q,_)::qs) -> rmax (fit (ps,qs)) (p - q + 1.0)
    | (_,_) -> 0.0

let fitlistl es = 
    let rec fitlistl' acc list = 
        match list with 
        |[] -> []
        |e :: es -> 
            let x = fit(acc,e)
            x :: fitlistl' (merge(acc, moveextent (e, x))) es
    fitlistl'[] es

let fitlistr es =
    let rec fitlistr' acc list =
        match list with
        | [] -> []
        | e :: es ->
            let x = fit(acc,e) * -1.0
            x :: fitlistr' (merge(moveextent (e, x), acc)) es
    List.rev (fitlistr' [] (List.rev es))

let mean (x,y) = (x + y) / 2.0

let fitlist es = 
    List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
    let rec design' (Node(label,subtrees)) : ('a * float) Tree * Extent = 
        let (trees, extents) = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map movetree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergelist pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst(design' tree)