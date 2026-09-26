module Day2Year2025
open FSharpPlus

type range=Range of bigint * bigint

let ranges data=seq {
    for line in String.split [| "," |] data do 
        let ranges=String.split [|"-"|] line |> Seq.take(2) |>Seq.toList 


        let sng= match ranges with 
                        | [ s; e; ]-> (tryParse s, tryParse e)
                        | _ -> (None, None)
        match sng with 
            |(Some s,Some e) -> yield Range(s, e)
            | _  -> ()
        }

let part1 data=seq { 
        for Range(s,e) in ranges data do
            for n in s..e do 
            let s=n.ToString()
            let len=s.Length

            if len%2=0 && s[0..len/2-1]=s[len/2..] then yield n } |>Seq.sum


let part2 data=seq {
       for Range(start,endd) in ranges data do
            for n in start..endd do 
                let s=n.ToString()
                let strSeq=String.toSeq s
                let len=s.Length

                let repeatedChunk=seq{ 
                    for size in 1..len-1 do 
                    if len%size=0 then 
                        let chunks=Seq.chunkBySize size strSeq
                        let first=chunks|>Seq.item 0
                        if not (chunks|>Seq.exists (function c -> c <> first)) then 
                            yield n }|>Seq.truncate 1
                yield! repeatedChunk
                } |> Seq.sum

let Puzzle()=part2 Day2Year2025Inputs.data