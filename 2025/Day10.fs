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


let maxDepth=10

let rec increment (bank:ResizeArray<int>) step last=
    if step>maxDepth then failwith "Went too deep!"
    if bank.Count<=step then bank.Add 0
    else 
        let digit=bank[step]
        bank[step]<-
            if digit<last then digit+1 else 
            increment bank (step+1) last
            0


let part1 data=
    seq{
    for target,buttons,_ in parseLines data do
        let bank=ResizeArray()
        let solution=
            seq{
            while true do
                increment bank 0 (buttons.Length-1)
                let result=bank|>Seq.fold(fun acc i->buttons[i]^^^acc) 0
                if result=target then bank.Count }
            |>Seq.tryItem 0

        match solution with 
        | Some result->result 
        | None -> failwith $"Combination not found within maxDepth={maxDepth}"
    }|>Seq.sum


let Puzzle()=
    part1 Day10Year2025Inputs.data
