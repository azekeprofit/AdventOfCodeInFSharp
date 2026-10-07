module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type point=Point of int * int * int
let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line|>Seq.map tryParse|>Seq.toList
        match point with
            | Some x :: Some y :: Some z :: [] -> Point(x,y,z)
            | _ -> failwith "Wrong points format" ]

let sq x=x*x
let distanceSq (Point(x,y,z)) (Point(a,b,c))=abs(sq(x-a)+sq(y-b)+sq(z-c))
// no need to calculate square root

type groupId=GroupId of point

let joinedCircuits circuits (_,(a,b))=
    match circuits|>Map.tryFind a,circuits|>Map.tryFind b with
        | Some aGroup, Some bGroup ->
            if aGroup<>bGroup then
                let min=min aGroup bGroup
                let max=max aGroup bGroup
                circuits|>Map.map(fun key value->if value=max then min else value)
            else circuits
        | Some aGroup, None -> circuits|>Map.add b aGroup
        | None, Some bGroup -> circuits|>Map.add a bGroup
        | None, None -> 
            let newGroupId=GroupId(min a b)
            circuits|>Map.add a newGroupId|>Map.add b newGroupId

let part1 data size=
    let points=lines data
    let cables=seq{
            for a,b in Seq.allPairs points points do
                if a>b
                then distanceSq a b, (a, b) }
            |>Seq.sortBy fst
            |>Seq.truncate size
            |>Seq.toList

    let united=fold joinedCircuits Map.empty cables
            |>Map.values
            |>Seq.groupBy(id)
            |>Seq.toList

    let a=united
        |>Seq.map(fun (id,g)-> Seq.length g)
        |>Seq.sortDescending
        |>Seq.truncate 3

    a|>Seq.fold (*) 1


let Puzzle()=
    part1 Day8Year2025Inputs.data 1000