module Day9Year2025
open Common
open FSharpPlus

type point=Point of int64*int64
let parseRanges s=seq{
    for line in Common.splitLines s do
        let r=String.split [| "," |] line |>Seq.map tryParse|>Seq.toList
        match r with
            | Some s :: Some e :: [] -> Point(s, e)
            | _ -> failwith "Incorrect range format" }

let area (Point(x,y)) (Point(x2,y2))=
    (abs(x-x2)+1L)*(abs(y-y2)+1L)

let part1 data=
    let points=parseRanges data
    seq{ for a,b in Seq.allPairs points points do
            if a>b then area a b }
    |>Seq.max


let Puzzle()=part1 Day9Year2025Inputs.data