module Day10Year2025
open Common
open FSharpPlus

let wrap openBracket closeBracket inside token=
    let len=String.length token
    if token[0]<>openBracket then None
    else if token[len-1]<>closeBracket then None
    else Some(inside(token.Substring(1,len-2)))
    
    

let (|ParseIndicators|)=
    wrap '[' ']' (Seq.map(function 
            | '#' -> true 
            | '.' -> false 
            | _ -> failwith "Incorrect indicator value"))

let parseNumbers list=
    let items=list|>String.split [|","|]
    let len=Seq.length items
    let result=items|>Seq.choose tryParse<int>|>Seq.toList
    if Seq.length result<>len then failwith "Wrong number format inside list"
    else result, len

let Button=wrap '(' ')' parseNumbers
let Joltage=wrap '{' '}' parseNumbers

let (|ButtonsAndJoltage|) rest=
    let len=Seq.length rest
    rest|>Seq.choosei(fun i-> if i=len-1 then Joltage else Button)

let parseLines data=seq{
    for line in splitLines data do
        let tokens=splitBySpaces line|>Seq.toList
        match tokens with
        | ParseIndicators(Some state) :: ButtonsAndJoltage(buttonsAndJoltage) -> 
            let len=Seq.length buttonsAndJoltage
            let joltage,joltLen=Seq.last buttonsAndJoltage
            if Seq.length state<>joltLen then failwith "Number of joltage values doesn't correspond to number of indicators"
            let buttons=buttonsAndJoltage|>Seq.take(len-1)|>Seq.map fst|>Seq.toList
            ()
        | _ -> failwith "Wrong input data"
        }

let part1 data=
    let lines=parseLines data|>Seq.toList
    ()

let Puzzle()=
    part1 Day10Year2025Inputs.inputValidation3
