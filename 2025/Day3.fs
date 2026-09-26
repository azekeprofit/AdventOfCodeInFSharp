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

                  
let countOnes (n: bigint) =
    n.ToByteArray()
    |> Array.sumBy (fun b -> BitOperations.PopCount(uint32 b))

let part2 bank=seq {
        let size=12
        let len=String.length bank
        let limit=bigint.Pow(2, len)
        for n in bigint.Pow(2, size)-1I..limit do 
            if size=countOnes n then
                let s=seq {
                    let mutable flags=n
                    let mutable i=0
                    while i<len && flags>0I do
                        if flags%2I=1I then
                            yield bank[i]
                        flags<-flags>>>1 
                        i<-i+1 }
                yield s|>Seq.toArray|>System.String
                }|>Seq.max

let Puzzle()=
    let allCombinations=banks Day3Year2025Inputs.data |> Seq.map part2
    let list=allCombinations |> Seq.map parse<bigint> |>Seq.toArray
    list |>Seq.sum