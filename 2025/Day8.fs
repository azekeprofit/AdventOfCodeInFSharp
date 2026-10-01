module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics


let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line |>Seq.toArray
        match tryParseArray<int*int*int> point with
            | Some(x,y,z) -> yield x,y,z
            | _ -> failwith "Wrong points format" ]


let Puzzle()=
    let res=lines Day8Year2025Inputs.example
    999