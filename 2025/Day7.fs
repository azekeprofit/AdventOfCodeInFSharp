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
        
type state={width:int; beams:int list list}

let rec tree lines width=
    match lines with
        | Spacer(Some w) :: Splitters splitters :: rest -> 
            if w<>width then failwith "Line widths don't match"
            [splitters] @ (tree rest width)
        | [Spacer(Some w)] ->
            if w<>width then failwith "Line widths don't match"
            []
        | _ -> failwith "Incorrect tree format"

let lines data=
    let lines=Seq.toList(Common.splitLines data)
    match lines with
    | FirstLine(Some start, width) :: rest -> (start,tree rest width,width)
    | _ -> failwith "Incorrect input data"



type splitCounter={counter:int; beams:int list}
type fallResult=BeamSplit of int list | Passthrough of int

let part1 data=
    let (start, splitters, width)=lines data

    let fall state splitters=
        let result=state.beams|>List.map(fun b-> 
            if splitters|>List.contains b 
            then BeamSplit [
                if b>0 then yield b-1
                if b<width-1 then yield b+1 ]
            else Passthrough b)

        {beams=[ for r in result do
                    match r with 
                    | BeamSplit list -> yield! list
                    | Passthrough b -> yield b ]|>List.distinct;
        counter=state.counter+(result|>Seq.filter(function BeamSplit _ -> true | _ -> false)|>Seq.length)}

    let final=fold fall {beams=[start];counter=0} splitters
    final.counter


let Puzzle()=
    part1 Day7Year2025Inputs.example