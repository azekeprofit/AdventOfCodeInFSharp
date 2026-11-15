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

let indicatorStep (sum,positional) digit=
    (if digit then sum+positional else sum), positional*2

let maxDepth=10

let rec loop n acc radix=
    if n = 0I then acc
    else loop (n / radix) ((n % radix) :: acc) radix

let rec extractDigits n acc depth radix (buttons:int array)=
    if n=0I then acc,depth else
        let digit=n%radix
        let newAcc=acc^^^buttons[int digit]
        extractDigits (n/radix) newAcc (depth+1) radix buttons

let part1 data=seq{
    for line in splitLines data do
        let tokens=splitBySpaces line|>Seq.toList
        match tokens with
        | ParseIndicators(Some display) :: ButtonsAndJoltage(buttonsAndJoltage) ->
            let joltage,joltLen=Seq.last buttonsAndJoltage
            if Seq.length display<>joltLen then failwith "Number of joltage values doesn't correspond to number of indicators"
            let target,_=display|>Seq.fold indicatorStep (0,1)

            let buttons=
                buttonsAndJoltage
                |>Seq.take(Seq.length(buttonsAndJoltage)-1)
                |>Seq.map (fun (list,_)->
                    list
                    |>Seq.map(pown 2)
                    |>Seq.sum)
                |>Seq.toArray
            let size=Array.length buttons


            let r=seq{
                for i in 0I..pown size maxDepth do
                    let result,depth=extractDigits i 0 0 size buttons
                    if result=target then depth }
                    |>Seq.item 0
            r
        | _ -> failwith "Wrong input data" }|>Seq.sum


let Puzzle()=
    part1 Day10Year2025Inputs.data
