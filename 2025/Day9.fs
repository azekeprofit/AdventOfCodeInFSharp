module Day9Year2025
open Common
open FSharpPlus


let infinity=1000
let minInf= -1000L
type point=Point of int64*int64
let x (Point(x,y))=x
let y (Point(x,y))=y

let parsePoints data=seq{
    for line in Common.splitLines data do
        match String.split [| "," |] line |>Seq.map tryParse|>Seq.toList with
            | Some s :: Some e :: [] -> Point(s, e)
            | _ -> failwith "Incorrect points format" }

let area a b=
    (abs(x a-x b)+1L)*(abs(y a-y b)+1L)

let part1 data=
    let points=parsePoints data
    seq{ for a,b in Seq.allPairs points points do
            if a>b then area a b }
    |>Seq.max

let normalize (coord:point->int64) (a,b) =
    let min=min (coord a) (coord b)
    let max=max (coord a) (coord b)
    min, max

let crosses rect (point1,point2)=
    let vertical=(x point1)=(x point2)
    let equalAxis=if vertical then x else y
    let movingAxis=if vertical then y else x

    let minx,maxx=normalize equalAxis rect
    let miny,maxy=normalize movingAxis rect

    let movingMin=min (movingAxis point1) (movingAxis point2)
    let movingMax=max (movingAxis point1) (movingAxis point2)
    let equalValue=equalAxis point1
    minx<equalValue && equalValue<maxx && movingMin<maxy && miny<movingMax

let part2 data=
    let points=parsePoints data|>Seq.toArray
    let lines=points|>Seq.pairwise|>Seq.cons (points[0],Array.last points)|>Seq.toArray

    seq{ for a,b in Seq.allPairs points points do
             if a<b then
                let wasCrossed=lines|>Array.exists(crosses (a,b))
                if not wasCrossed then 
                    area a b }|>Seq.max
        

let Puzzle()=part2 Day9Year2025Inputs.data