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
        |>Seq.tryExactlyOne

let (|Spacer|) (line:string)=
    if Regex.IsMatch(line,@"^\.+$") then Some line.Length else None

let (|Splitters|) line=
    line    
    |>Seq.choosei(fun i c -> 
        match c with
            | '^' -> Some i
            | '.' -> None
            | _ -> failwith "Incorrect 1st line format")|>Seq.toList
        
type state={width:int; beams:int list list}

let rec tree lines=
    match lines with
        | Spacer(Some(width)) :: Splitters splitters :: rest -> 
            let {width=w;beams=b}=tree rest
            if w<>width then failwith "Line widths don't match"
            {width=width; beams=[splitters] @ b }
        | [Spacer(Some(width))] -> {width=width; beams=[]}
        | _ -> failwith "Incorrect tree format"

let lines data=
    let lines=Seq.toList(Common.splitLines data)
    match lines with
    | FirstLine(Some start) :: rest -> (start,tree rest)
    | _ -> failwith "Incorrect input data"


type splitCounter={counter:int; beams:int list}
let part1 data=
    let (start, {width=width; beams=splitters})=lines data

    let fall {beams=beams; counter=c} splitters=
        let mutable newC=c
        let newBeams=[
            for b in beams do
                if splitters|>List.contains b then 
                    newC<-newC+1
                    if b>0 then yield b-1
                    if b<width-1 then yield b+1
                else yield b     ]|>List.distinct
        {beams=newBeams; counter=newC}

    let final=fold fall {counter=0;beams=[start]} splitters
    final.counter


let Puzzle()=
    part1 Day7Year2025Inputs.data