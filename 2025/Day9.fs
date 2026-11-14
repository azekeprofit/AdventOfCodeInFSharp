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

let crosses equalAxis movingAxis equalValue moving1 moving2 rect=
    let minx,maxx=normalize equalAxis rect
    let miny,maxy=normalize movingAxis rect
    let movingMin=min moving1 moving2
    let movingMax=max moving1 moving2
    minx<equalValue && equalValue<maxx && movingMin<maxy && miny<movingMax

let part2 data=
    let points=parsePoints data|>Seq.toArray
    let lines=points|>Seq.pairwise|>Seq.cons (points[0],Array.last points)|>Seq.toArray

    seq{ for a,b in Seq.allPairs points points do
            if a<b then
                let wasCrossed=lines|>Array.exists(
                    fun (c,d)->
                                if (x c)=(x d) then
                                    crosses x y (x c) (y c) (y d) (a,b)
                                else 
                                    crosses y x (y c) (x c) (x d) (a,b)
                    )
                if not wasCrossed then 
                    area a b }|>Seq.max
        

let Puzzle()=part2 Day9Year2025Inputs.data