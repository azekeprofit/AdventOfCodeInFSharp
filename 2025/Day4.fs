module Day4Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

let map data= 
    String.split [| "\n"; "\r" |] data
    |>Seq.filter(fun s -> String.length(s)>0)
    |>Seq.map(function 
        s when Regex.IsMatch(s,@"^[\@\.]+$") -> s|>String.toSeq
        | _ -> failwith "Incorrect map!" )
    |>array2D

let part1 data=
    let arr= map data
    123

let Puzzle()=
    part1 Day4Year2025Inputs.example