module Day6Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type operation=Addition | Multiplication

let parse data=
    let lines=Common.splitLines data |> Seq.toArray
    let operations=[
        for op in lines |> Array.last |> Common.splitBySpaces do
            match op with 
                | "+" -> yield Addition
                | "*" -> yield Multiplication
                | _ -> failwith "Incorrect operations row" ]

    let array=seq {
        for line in lines|>Array.take (lines.Length-1) do    
            yield seq{
              for number in Common.splitBySpaces line |> Seq.map(tryParse<bigint>) do
                match number with 
                    | Some n -> yield n 
                    | _ -> failwith "Incorrect numbers table" }
                    }|>array2D

    if array|>Array2D.length2<>operations.Length then failwith "Numbers and operations don't match"
    (operations,array)
    


let part1 data=
    let (ops,arr)=parse data
    
    let processRow (sum:bigint array) (row:bigint array)=
        row|>Array.mapi(fun i v->
                match ops[i] with
                    | Addition -> v+sum[i]
                    | Multiplication-> v*sum[i])

    let firstRow=arr[0,0..]
    let restOfRow=seq{ 
        for i in 1..(Array2D.length1 arr)-1 do
            yield arr[i,0..] }
    let final=fold processRow firstRow restOfRow
    Array.sum final



let Puzzle()=
    part1 Day6Year2025Inputs.data