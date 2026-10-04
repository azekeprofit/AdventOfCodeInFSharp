module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics


let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line|>Seq.map tryParse<uint64>|>Seq.toList
        match point with
            | Some x :: Some y :: Some z :: [] -> x,y,z
            | _ -> failwith "Wrong points format" ]

let sq x=x*x
let distanceSq (x,y,z) (a,b,c)=sq(x-a)+sq(y-b)+sq(z-c)
// no need to calculate square root



let Puzzle()=
    let points=lines Day8Year2025Inputs.example
    let connections=ResizeArray()
    let r=seq{
            let indexed=Seq.indexed points
            for (i,a),(j,b) in Seq.allPairs indexed indexed do
                if i<>j then 
                    distanceSq a b, i, j
                }
            |>Seq.sortBy(fun (d,_,_) -> d)|>Seq.truncate 10|>Seq.toList

    999