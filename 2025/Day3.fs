module Day3Year2025
open FSharpPlus
open System.Text.RegularExpressions
open System.Numerics

let banks data=
    let result=String.split [| "\n"; "\r" |] data |>Seq.filter(function s -> String.length(s)>0)
    result|>Seq.iter(function s -> if not(Regex.IsMatch(s,@"^\d+$")) then failwith "Incorrect battery joltage data!")
    result

let part1 bank=seq { 
        let len=String.length(bank)
        for i in 0..len-2 do 
            for secondDigit in bank[i+1..len-1] do
                yield $"{bank[i]}{secondDigit}"
                  } |>Seq.max

                  
let rec iteration (batteries:bigint) (bank:byte[]) start endd remainder=seq{
    if remainder<=0 then 
        yield batteries
    else
        for i in start..endd do
            let newDigit=bank[i]
            if (i=start) || (newDigit>bank[i-1]) then
                yield! iteration ((batteries*10I)+bigint newDigit) bank (i+1) (endd+1) (remainder-1)
}
    

let part2 bank=seq {
        let size=12
        let bankArray=bank|>String.getBytes(System.Text.Encoding.ASCII)|>Array.map(function b->b-48uy)
        yield! iteration 0 bankArray 0 (bank.Length-size) size
                }|>Seq.max

let Puzzle()=
    let allCombinations=banks Day3Year2025Inputs.data |> Seq.map part2
    // let list=allCombinations |>Seq.toArray
    allCombinations |>Seq.sum