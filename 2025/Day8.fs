module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics


let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line|>Seq.map tryParse|>Seq.toList
        match point with
            | Some x :: Some y :: Some z :: [] -> x,y,z
            | _ -> failwith "Wrong points format" ]

let sq x=x*x
let distanceSq (x,y,z) (a,b,c)=abs(sq(x-a)+sq(y-b)+sq(z-c))
// no need to calculate square root

let joinedCircuits circuits (_,(a,b))=
    match circuits|>Map.tryFind a,circuits|>Map.tryFind b with
        | Some aGroup, Some bGroup ->
                circuits|>Map.map(fun key value->if key=b || value=bGroup then aGroup else value)
        | Some aGroup, None -> circuits|>Map.add b aGroup
        | None, Some bGroup -> circuits|>Map.add a bGroup
        | None, None -> 
            let newGroupId=min a b
            circuits|>Map.add a newGroupId|>Map.add b newGroupId

let part1 data size=
    let points=lines data
    let cables=seq{
            let indexed=Seq.indexed points
            for (i,a),(j,b) in Seq.allPairs indexed indexed do
                if j>i then 
                    distanceSq a b, (i, j) }
            |>Seq.sortBy fst
            |>Seq.truncate size
            |>Seq.toList

    let r=fold joinedCircuits Map.empty cables
        |>Map.values
        |>Seq.groupBy(id)
        |>Seq.map(fun (id,g)-> g|>Seq.length)
        |>Seq.sortDescending
        |>Seq.truncate 3

    r|>Seq.fold (*) 1


let Puzzle()=
    part1 Day8Year2025Inputs.data 1000
