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

let maxDepth=10

let rec extractDigits n acc depth radix (buttons:int array)=
    if n=0I then acc,depth else
        let digit=n%radix
        let newAcc=acc^^^buttons[int digit]
        extractDigits (n/radix) newAcc (depth+1) radix buttons

let part1 data=seq{
    for line in splitLines data do
        let tokens=splitBySpaces line|>Seq.toList
        match tokens with
        | ParseIndicators(Some display) :: ButtonsAndJoltage(buttons,(joltage,joltLen)) ->
            if Seq.length display<>joltLen then failwith "Number of joltage values doesn't correspond to number of indicators"            
            let size=bigint buttons.Length
            let target=getTarget display

            let solution=
                seq{
                for i in 0I..pown size maxDepth do
                    let result,depth=extractDigits i 0 0 size buttons
                    if result=target then depth }
                |>Seq.tryItem 0

            match solution with 
            | Some result->result 
            | None -> failwith $"Combination not found within maxDepth={maxDepth}"
        | _ -> failwith "Wrong input data" }|>Seq.sum


let Puzzle()=
    part1 Day10Year2025Inputs.data
