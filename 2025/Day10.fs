module Day10Year2025
open Common
open FSharpPlus

let wrap openBracket closeBracket inside token=
    let len=String.length token
    if token[0]<>openBracket then None
    elif token[len-1]<>closeBracket then None
    else Some(inside(token.Substring(1,len-2)))

let (|ParseIndicators|)=
    wrap '[' ']' (Seq.map(function
            | '#' -> true
            | '.' -> false
            | _ -> failwith "Incorrect indicator value"))

let parseNumbers list=
    let items=list|>String.split [|","|]
    let len=Seq.length items
    let result=
        items
        |>Seq.choose tryParse<int>
        |>Seq.toList
    if Seq.length result<>len then failwith "Wrong number format inside list"
    else result, len

let Button=wrap '(' ')' parseNumbers
let Joltage=wrap '{' '}' parseNumbers

let (|ButtonsAndJoltage|) rest=
    let last=Seq.length(rest)-1
    let items=rest|>Seq.choosei(fun i-> if i=last then Joltage else Button)
    let buttons=
        items
        |>Seq.take last
        |>Seq.map (fun (list,_)->
                    list
                    |>Seq.map(pown 2)
                    |>Seq.sum)
        |>Seq.toArray

    buttons,Seq.last items

let getTarget display=
    let target,_=
        display
        |>Seq.fold (fun (sum,positional) digit->
        (if digit then sum+positional else sum), positional*2) (0,1)
    target

let parseLines data=seq{
    for line in splitLines data do
        let tokens=splitBySpaces line|>Seq.toList
        match tokens with
        | ParseIndicators(Some display) :: ButtonsAndJoltage(buttons,(joltage,joltLen)) ->
            if Seq.length display<>joltLen then failwith "Number of joltage values doesn't correspond to number of indicators"
            getTarget display,buttons,joltage
        | _ -> failwith "Wrong input data" }


let rec increment (bank:ResizeArray<int>) step last maxDepth=
    if step>maxDepth then false else
        let digit=bank[step]
        if digit<last then 
            bank[step]<-digit+1
            true
        else 
            bank[step]<-0
            increment bank (step+1) last maxDepth


let part1 data=
    seq{
    for target,buttons,_ in parseLines data do
        let bank=ResizeArray(Seq.replicate buttons.Length 0)
        let solution=
            seq{
            while increment bank 0 1 (buttons.Length-1) do                
                let result,bits,_=
                    bank
                    |>Seq.fold(fun (acc,bits,pos) i->
                        if i%2=1 then buttons[pos]^^^acc,bits+1,pos+1
                        else acc,bits,pos+1)
                        (0,0,0)
                if result=target then bits }
            |>Seq.min
        solution
    }|>Seq.sum


let Puzzle()=
    part1 Day10Year2025Inputs.data
