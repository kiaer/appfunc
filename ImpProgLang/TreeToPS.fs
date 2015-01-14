module TreeToPS

open AST
open AstToTree
open TreeGenerator

let showString = "dup stringwidth pop 2 div neg 0 rmoveto show\n"
let moveto (x, y) = string x + " " + string y + " moveto\n"
let lineto (x, y) = " " + string x + " " + string y + " lineto\nstroke\n"
let newcord (x,y) flt =
    if flt = 0.0 then
        (x, y)
    else if flt < 0.0 then
        let nx = int(System.Math.Round((-flt) * 400.0))
        (x - nx, y)
        else
            let nx = int(System.Math.Round(flt * 400.0))
            (x + nx, y)
    

let rec designToPS tree ps (x,y) (ox, oy) =
    //printfn "%A" tree
    match tree with
    | [] -> ps + ""
    | Node((label,flt),[]) :: xs -> 
            let (nx, ny) = newcord (x,y) flt
            printfn "--> new %A old %A" (nx,ny)  (ox,oy)
            printfn "---> lineto %A" (lineto(ox,oy))
            let ps' = ps + moveto(nx,ny)  + "(" + label + ") " + showString + moveto(nx, ny + 20) + lineto(x,oy - 20)
            designToPS xs ps' (nx, ny) (nx,y)
    | Node((label,flt),l::ls) :: xs ->       
            //intfn "----> %A" (nx,ny)
            let (nx, ny) = newcord (x,y) flt
            printfn "--> new %A old %A" (nx,ny)  (ox,oy)
            printfn "---> lineto %A" (lineto(ox,oy))
            let ps' = ps + moveto(nx,ny) + "(" + label + ") " + showString + moveto(nx, ny + 20) + lineto(x,oy - 20)
            let ps'' = designToPS [l] ps' (nx, ny - 60) (nx,y)
            let ps''' = designToPS ls ps'' (nx, ny - 60) (x,y)
            designToPS xs ps''' (x, y) (x,oy)