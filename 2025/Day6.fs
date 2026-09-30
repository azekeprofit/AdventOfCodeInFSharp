module Day6Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type operation=Addition | Multiplication

let parseOps line=
    let ops=Common.splitBySpaces line
    ops|>Seq.map(function 
            | "+" -> Addition
            | "*" -> Multiplication
            | _ -> failwith "Incorrect operations row")
    |>Seq.toArray

                
let parseLines data=
    let lines=Seq.toArray(Common.splitLines data)
    let table=lines|>Array.take(lines.Length-1)
    let ops=parseOps(Array.last lines)
    (table,ops)

let transpose table=
    Array2D.init (Array2D.length2 table) (Array2D.length1 table) (fun i j -> table[j, i])

let part1 data=
    let (lines,ops)=parseLines data

    let arr=seq {
        for line in lines do    
            yield seq{
              for number in Common.splitBySpaces line |> Seq.map(tryParse<bigint>) do
                match number with 
                    | Some n -> yield n 
                    | _ -> failwith "Incorrect numbers table" }
                    }|>array2D

    if arr|>Array2D.length2<>ops.Length then failwith "Numbers and operations don't match"

    let rotated=transpose arr
    ops|>Seq.mapi(fun i op -> 
        match op with
        | Addition -> rotated[i,0..]|>Seq.sum
        | Multiplication-> fold (*) 1I rotated[i,0..]
    )|>Seq.sum

let part2 data=
    let (lines,ops)=parseLines data
    let chars=lines|>Array.map String.toSeq|>array2D|>transpose
    let numbers=seq {
                let buffer=ResizeArray()
                for i in 1..Array2D.length1 chars do
                    match String.ofSeq chars[i-1,0..] |> tryParse<bigint> with
                        | Some n -> buffer.Add n
                        | None -> 
                            yield buffer.ToArray()
                            buffer.Clear()
                if buffer.Count<>0 then yield buffer.ToArray()
                }|>Seq.toArray

    if numbers.Length<>ops.Length then failwith "Numbers don't correspond to operations"

    ops
    |>Seq.mapi(fun i op -> 
        match op with
        | Addition -> numbers[i]|>Array.sum
        | Multiplication-> fold (*) 1I numbers[i]
    )|>Seq.sum


let Puzzle()=
    part2 Day6Year2025Inputs.data