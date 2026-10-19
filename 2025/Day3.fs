module Day3Year2025
open FSharpPlus
open System.Text.RegularExpressions
open System.Numerics

let banks data=
        Common.splitLines data
        |>Seq.map(function 
            | s when Regex.IsMatch(s,@"^\d+$") -> s|>String.getBytes System.Text.Encoding.ASCII |>Array.map(fun b->b-48uy)
            | _ -> failwith "Incorrect battery joltage data -- non-digit character")

// exhaustive dumb loop
let part1Dumb lines=
    seq{
    for bank in banks lines do
    let maxJoltage=seq { 
            let len=bank.Length
            for i in 0..len-2 do 
                for secondDigit in bank[i+1..len-1] do
                    $"{bank[i]}{secondDigit}"
                      } |>Seq.max
    match tryParse<int> maxJoltage with
        | Some n -> n
        | _ -> failwith "This is not supposed happen" }
    |>Seq.sum

// greedy, much faster
let part1 lines=
    seq{
    for bank in banks lines do
        let len=bank.Length
        let firstPos, firstDigit=Array.indexed bank[0..len-2]|>Array.maxBy snd
        let secondDigit=Array.max bank[firstPos+1..]
        int firstDigit*10+int secondDigit } |>Seq.sum

let rec greedy batteries (bank:array<int*byte>) start endd remainder=
    let newDigit=bank[start..endd]|>Array.maxBy snd
    let num=batteries*10I+bigint(snd newDigit)
    if remainder<2 then num else
    greedy num bank (fst newDigit+1) (endd+1) (remainder-1)


let part2 lines=
    let size=12
    seq{
    for bank in banks lines do
        let bankPos=Array.indexed bank
        greedy 0 bankPos 0 (bank.Length-size) size
    }|>Seq.sum

[<TailCall>] // as of F# 10, yield!-ed tail call recursion *should* be unrolled into loop automatically
// this attribute is here just to cause warnings if in the future function is changed and it's no longer tail-callable
let rec iteration batteries (bank:byte array) start endd remainder=seq{
    match remainder with
        | 0 -> yield batteries/10I // this never runs, because last stage of recursion gets captured by 1 case below
        | 1 -> yield batteries+bigint(Seq.max bank[start..endd]) // last digit is always maximum of remaining batteries
        | _ -> 
            let mutable localMaximum=bank[start]
            for i in start..endd do
                let newDigit=bank[i]
                if i=start || localMaximum<newDigit then 
                    localMaximum<-newDigit
                    yield! iteration ((batteries+bigint newDigit)*10I) bank (i+1) (endd+1) (remainder-1)
                    // 10 multiplication is done ahead of time --^
}
    

let part2Dumb lines=
    seq{
    for bank in banks lines do
        seq {
        let size=12
        yield! iteration 0 bank 0 (bank.Length-size) size
                }|>Seq.max
    }|>Seq.sum

let Puzzle()=
    part2 Day3Year2025Inputs.data