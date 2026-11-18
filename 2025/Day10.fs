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
        |>Seq.toArray
    if result.Length<>len then failwith "Wrong number format inside list"
    else result, len

let Button=wrap '(' ')' parseNumbers
let Joltage=wrap '{' '}' parseNumbers

let (|ButtonsAndJoltage|) rest=
    let last=Seq.length(rest)-1
    let items=rest|>Seq.choosei(fun i-> if i=last then Joltage else Button)
    let buttons=
        items
        |>Seq.take last
        |>Seq.map fst
        |>Seq.toArray
    buttons,Seq.last items


let parseLines data=seq{
    for line in splitLines data do
        let tokens=splitBySpaces line|>Seq.toList
        match tokens with
        | ParseIndicators(Some display) :: ButtonsAndJoltage(buttons,(joltage,joltLen)) ->
            if Seq.length display<>joltLen then failwith "Number of joltage values doesn't correspond to number of indicators"
            let target,_=
                display
                |>Seq.fold (fun (sum,positional) digit->
                (if digit then sum+positional else sum), positional*2) (0,1)
            target,buttons,joltage
        | _ -> failwith "Wrong input data" }


let rec increment bank step dontCarry maxDepth=
    if step>=maxDepth then false else
        let digit=bank|>Array.item step
        if dontCarry bank step then 
            bank[step]<-digit+1
            true
        else 
            bank[step]<-0
            increment bank (step+1) dontCarry maxDepth


let checkDisplay bank binary target=
    seq{
    let result,bits,_=
        bank
        |>Seq.fold(fun (acc,bits,pos) i->
        if i%2=1 then (binary|>Array.item pos)^^^acc,bits+1,pos+1
        else acc,bits,pos+1) (0,0,0)
    if result=target then bits }

let findMinimal buttons target carryCondition=
    let binary=
        buttons
        |>Seq.map(
            Seq.map(pown 2)
            >>Seq.sum)
        |>Seq.toArray
                        
    let bank=Array.zeroCreate binary.Length
    seq{
    while increment bank 0 carryCondition binary.Length do
        yield! checkDisplay bank binary target }
    |>Seq.min

let part1 data=
    seq{
    for target,buttons,_ in parseLines data do
        findMinimal buttons target (fun bank step->bank[step]<=1)
    }|>Seq.sum

let checkJoltage bank joltage buttons=
    let sums=joltage|>Array.length|>Array.zeroCreate
    
    bank
    |>Seq.mapi(fun i digit->
        if digit=0 then false else
        buttons
        |>Array.item i
        |>Array.map(fun pos->
            let newJoltage=sums[pos]+digit
            sums[pos]<-newJoltage
            newJoltage>joltage[pos])
        |>Seq.exists id)
    |>Seq.exists id
    |>not
    
let part2 data=
    seq{
    for target,buttons,joltage in parseLines data do
        findMinimal buttons target (fun bank _->checkJoltage bank joltage buttons)
    }|>Seq.sum


let Puzzle()=
    part2 Day10Year2025Inputs.example
