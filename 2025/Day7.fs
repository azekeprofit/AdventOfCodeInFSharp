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
    if Regex.IsMatch(line,@"^\.+$") then Some () else None

let (|Splitters|) line=
    line    
    |>Seq.choosei(fun i c -> 
        match c with
            | '^' -> Some i
            | '.' -> None
            | _ -> failwith "Incorrect 1st line format")|>Seq.toList
        
let rec tree lines=
    match lines with
        | Spacer _ :: Splitters splitters :: rest -> [splitters] @ tree rest
        | [Spacer _] -> []
        | _ -> failwith "Incorrect tree format"

let lines data=
    let lines=Seq.toList(Common.splitLines data)
    match lines with
    | FirstLine(Some start) :: rest -> (start,tree rest)
    | _ -> failwith "Incorrect input data"


let part1 data=
    let (start,splitters)=lines data
    123


let Puzzle()=
    part1 Day7Year2025Inputs.example