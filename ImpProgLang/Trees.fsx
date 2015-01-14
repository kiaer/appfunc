#r "FSharp.PowerPack.dll"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "ParserUtil.fs"
#load "TreeGenerator.fs"
#load "AstToTree.fs"
#load "TreeToPs.fs"

open System
open AST
open ParserUtil
open TreeGenerator
open AstToTree
open TreeToPS

let test = Node(("a"), [Node(("b"),[Node(("d"),[]);Node(("e"),[])]);Node(("c"),[])])

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;
let s2 = parseStm "let n: 4; y: 1
                   in print (n)
                   end";;
let p3 = parseFromFile "Factorial1.while";;
let p4 = parseFromFile "Factorial2.while";;
let p5 = parseFromFile "Factorial3.while";;
let p6 = parseFromFile "Factorial4.while";;
let p7 = parseFromFile "Factorial5.while";;

let ast1 = design (translateStm p3)

let testTree = design test

let init = "%!
<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice
1 1 scale
700 999 translate
newpath
/Times-Roman findfont 10 scalefont setfont
"
let test2 = [design test]
let initcord = (0, -10)
let result = designToPS test2 init initcord initcord  + "showpage"

System.IO.File.WriteAllText("test.ps", result)