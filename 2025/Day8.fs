module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type point=Point of int64 * int64 * int64
let getX point=
    let (Point(x,y,z))=point
    x

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

let joinedCircuits (lastConnected,circuits) (_,(a,b))=
    let newConnected=lastConnected|>Map.add a b|>Map.add b a
    match circuits|>Map.tryFind a,circuits|>Map.tryFind b with
        | None, None -> 
            let newGroup=ResizeArray()
            newConnected,fold (addToGroup newGroup) circuits [a;b]
        | Some aGroup, None -> 
            newConnected, addToGroup aGroup circuits b
        | None, Some bGroup -> 
            newConnected, addToGroup bGroup circuits a
        | Some aGroup, Some bGroup when aGroup<>bGroup ->
            newConnected, fold (addToGroup aGroup) circuits bGroup
        | _ -> lastConnected, circuits


let cables data=
        let points=lines data
        seq{
        for a,b in Seq.allPairs points points do
            if a>b
            then distanceSq a b, (a, b) }
        |>Seq.sortBy fst

let part1 data size=
    cables data
        |>Seq.truncate size
        |>Seq.fold joinedCircuits (Map.empty,Map.empty)
        |>snd
        |>Map.values
        |>Seq.distinct
        |>Seq.map _.Count
        |>Seq.sortDescending
        |>Seq.truncate 3
        |>Seq.reduce (*)

let part2 data=
    let points=cables data
    let connected, circuits=fold joinedCircuits (Map.empty,Map.empty) points
    let lastGroup=circuits|>Map.values|>Seq.item 0
    let lastPoint=Seq.last lastGroup
    getX lastPoint*getX connected[lastPoint]

let Puzzle()=
    part2 Day8Year2025Inputs.data