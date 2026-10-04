module Day7Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics
    
let (|FirstLine|) line=
    line    
    |>Seq.choosei(fun i c -> 
        match c with
            | 'S' -> Some i
            | '.' -> None
            | _ -> failwith "Incorrect 1st line format") 
        |>Seq.tryExactlyOne, line|>Seq.length

let (|Spacer|) (line:string)=
    if Regex.IsMatch(line,@"^\.+$") then Some line.Length else None

let (|Splitters|) line=
    line    
    |>Seq.choosei(fun i c -> 
        match c with
            | '^' -> Some i
            | '.' -> None
            | _ -> failwith "Incorrect splitters line format")|>Seq.toList

let rec tree lines width=
    match lines with
        | Spacer(Some w) :: Splitters splitters :: rest -> 
            if w<>width then failwith "Line widths don't match"
            [splitters] @ tree rest width
        | [Spacer(Some w)] ->
            if w<>width then failwith "Line widths don't match"
            []
        | _ -> failwith "Incorrect tree format"

let lines data=
    let lines=Seq.toList(Common.splitLines data)
    match lines with
    | FirstLine(Some start, width) :: rest -> start, width, tree rest width
    | _ -> failwith "Incorrect input data"



type splitCounter={counter:int; beams:int list}
type fallResult=BeamSplit of int list | Passthrough of int

let splitBeams beams splitters width=
        beams|>List.map(fun b-> 
                if splitters|>List.contains b 
                then BeamSplit [
                    if b>0 then b-1
                    if b<width-1 then b+1 ]
                else Passthrough b)

let collectBeams result=[ 
    for r in result do
                match r with 
                | BeamSplit list -> yield! list
                | Passthrough b -> yield b ]

let part1 data=
    let start, width, splitters=lines data

    let fall state splitters=
        let result=splitBeams state.beams splitters width
        {beams=collectBeams result|>List.distinct;
        counter=state.counter+(result|>Seq.sumBy(function BeamSplit _ -> 1 | _ -> 0))}

    let final=fold fall {beams=[start];counter=0} splitters
    final.counter

type quantumBeams=QBeams of int * bigint // quantum beams of position and how many timelines are inside
let sumTimelines=Seq.sumBy(function QBeams(_,t)->t)

let splitQuantumBeams width beams splitters=
            seq{
            for QBeams(b, timelines) in beams do
                    if splitters|>List.contains b
                    then 
                        if b>0 then QBeams(b-1, timelines)
                        if b<width-1 then QBeams(b+1, timelines)
                    else QBeams(b, timelines)}
            |>Seq.groupBy(function QBeams(b,_) ->b)
            |>Seq.map(fun (b,list)->QBeams(b,sumTimelines list))
            |>Seq.toList

let part2 data=
    let start, width, splitters=lines data
    let final=fold (splitQuantumBeams width) [QBeams(start,1I)] splitters
    sumTimelines final

let Puzzle()=
    part2 Day7Year2025Inputs.data