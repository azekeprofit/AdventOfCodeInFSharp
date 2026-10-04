module Day3Year2025
open FSharpPlus
open System.Text.RegularExpressions
open System.Numerics

let banks data=
        Common.splitLines data
        |>Seq.map(function 
            | s when Regex.IsMatch(s,@"^\d+$") -> s 
            | _ -> failwith "Incorrect battery joltage data -- non-digit character")

let part1 bank=
    let maxJoltage=seq { 
            let len=String.length bank
            for i in 0..len-2 do 
                for secondDigit in bank[i+1..len-1] do
                    $"{bank[i]}{secondDigit}"
                      } |>Seq.max
    match tryParse<int> maxJoltage with
        | Some n -> n
        | _ -> failwith "This is not supposed happen"

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
    

let part2 bank=seq {
        let size=12
        let bankArray=bank|>String.getBytes(System.Text.Encoding.ASCII)|>Array.map(fun b->b-48uy)
        yield! iteration 0 bankArray 0 (bank.Length-size) size
                }|>Seq.max

let Puzzle()=
    let allCombinations=banks Day3Year2025Inputs.data |> Seq.map part2
    // let list=allCombinations |>Seq.toArray
    allCombinations |>Seq.sum