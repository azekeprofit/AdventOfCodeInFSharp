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

let isInsideBy coord rect point=
    let min1, max1=normalize coord rect
    let v=coord point
    min1<=v && v<=max1

let isInside rect point=
    isInsideBy x rect point &&
    isInsideBy y rect point

let part2 data=
    let points=parsePoints data|>Seq.toArray

    seq{ for a,b in Seq.allPairs points points do
            // let inside=points|>Seq.except(seq{a; b})|>Seq.exists (isInside (a,b))|>Seq.toList
            // let areaRes=area a b
            if a>b && not(points|>Seq.except(seq{a; b})|>Seq.exists (isInside (a,b)))
            then area a b }|>Seq.max
        

let Puzzle()=part2 Day9Year2025Inputs.example