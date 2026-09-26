module Day3Year2025
open FSharpPlus
open System.Text.RegularExpressions

let banks data=
    let result=String.split [| "\n"; "\r" |] data |>Seq.filter(function s -> String.length(s)>0)
    result|>Seq.iter(function s -> if not(Regex.IsMatch(s,@"^\d+$")) then failwith "Incorrect battery joltage data!")
    result

let max bank=seq { 
                    let len=String.length(bank)
                    for i in 0..len-2 do 
                        for secondDigit in bank[i+1..len-1] do
                            yield $"{bank[i]}{secondDigit}"
                  } |>Seq.max

let Puzzle()=
    let allCombinations=banks Day3Year2025Inputs.data |> Seq.map max
    allCombinations |> Seq.map parse<int> |>Seq.sum