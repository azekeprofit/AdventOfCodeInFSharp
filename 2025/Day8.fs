module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type point=Point of uint64 * uint64 * uint64
let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line|>Seq.map tryParse|>Seq.toList
        match point with
            | Some x :: Some y :: Some z :: [] -> Point(x,y,z)
            | _ -> failwith "Wrong points format" ]

let sq x=x*x
let distanceSq (Point(x,y,z)) (Point(a,b,c))=sq(x-a)+sq(y-b)+sq(z-c)
// no need to calculate square root

let addToGroup (group:point ResizeArray) circuits point=
        group.Add point
        circuits|>Map.add point group    

let joinedCircuits circuits (_,(a,b))=
    match circuits|>Map.tryFind a,circuits|>Map.tryFind b with
        | None, None -> 
            let newGroup=ResizeArray()
            fold (addToGroup newGroup) circuits [a;b]
        | Some aGroup, None -> 
            addToGroup aGroup circuits  b
        | None, Some bGroup -> 
            addToGroup bGroup circuits a
        | Some aGroup, Some bGroup when aGroup<>bGroup ->
            fold (addToGroup aGroup) circuits bGroup
        | _ -> circuits

let part1 data size=
    let points=lines data
    let cables=seq{
            for a,b in Seq.allPairs points points do
                if a>b
                then distanceSq a b, (a, b) }
            |>Seq.sortBy fst
            |>Seq.truncate size

    cables
            |>Seq.fold joinedCircuits Map.empty 
            |>Map.values
            |>Seq.distinct
            |>Seq.map _.Count
            |>Seq.sortDescending
            |>Seq.truncate 3
            |>Seq.fold (*) 1


let Puzzle()=
    part1 Day8Year2025Inputs.data 1000