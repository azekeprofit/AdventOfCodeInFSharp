module Day6Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type operation=Addition | Multiplication

let parseOps line=[
    for op in line |> Common.splitBySpaces do
        match op with 
            | "+" -> yield Addition
            | "*" -> yield Multiplication
            | _ -> failwith "Incorrect operations row" ]

                
let parseLines data=
    let lines=Seq.toArray(Common.splitLines data)
    let table=lines|>Array.take(lines.Length-1)
    let ops=parseOps(Array.last lines)
    (table,ops)

let transpose table=
    Array2D.init (Array2D.length2 table) (Array2D.length1 table) (fun i j -> table[j, i])

let parse data=
    let (lines,ops)=parseLines data

    let array=seq {
        for line in lines do    
            yield seq{
              for number in Common.splitBySpaces line |> Seq.map(tryParse<bigint>) do
                match number with 
                    | Some n -> yield n 
                    | _ -> failwith "Incorrect numbers table" }
                    }|>array2D

    if array|>Array2D.length2<>ops.Length then failwith "Numbers and operations don't match"
    (transpose array,ops)


let part1 data=
    let (arr,ops)=parse data
    ops
    |>List.mapi(fun i op -> 
        match op with
        | Addition -> arr[i,0..]|>Seq.sum
        | Multiplication-> fold (*) 1I arr[i,0..]
    )|>List.sum
    


let Puzzle()=
    part1 Day6Year2025Inputs.example