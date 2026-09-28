module Day5Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type range=Range of bigint * bigint
type ingredient=Ingredient of bigint

let parseRanges s=[
    for line in Common.splitLines s do
        let r=String.split [| "-" |] line |> Seq.truncate 2 |> Seq.toArray
        match tryParseArray r with
            | Some (s, e) -> yield Range(s, e)
            | _-> failwith "Incorrect range format" ]

let parseIngredients s=[
    for line in Common.splitLines s do
        match tryParse line with
            | Some s -> yield Ingredient s
            | _-> failwith "Incorrect ingredient id format" ]
        
let spoiled (Ingredient i) ranges=
    ranges|>List.exists(function Range(s, e) -> s<=i && i<=e) 
    

let part1 data=
    let arr=Regex.Split(data, @"\n\r?\n\r?")
    if arr.Length<>2 then failwith "Incorrect input format"
    let ranges=parseRanges arr[0]
    let ingredients=parseIngredients arr[1]
    ingredients 
    |>Seq.map(fun i -> if spoiled i ranges then 1 else 0)
    |>Seq.sum

let Puzzle()=
    part1 Day5Year2025Inputs.data