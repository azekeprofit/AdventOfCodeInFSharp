module Day4Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

let map data= 
    String.split [| "\n"; "\r" |] data
    |>Seq.filter(fun s -> String.length(s)>0)
    |>Seq.map(function 
        s when Regex.IsMatch(s,@"^[\@\.]+$") -> String.toSeq s
        | _ -> failwith "Incorrect map!" )
    |>array2D

let adjacent i j=seq{
        yield (i-1,j-1); yield (i-1,j); yield (i-1,j+1)
        yield (i,j-1);                  yield (i,j+1)
        yield (i+1,j-1); yield (i+1,j); yield (i+1,j+1)
        }

let part1 data=
    let arr= map data
    seq {
    for i in 0 .. Array2D.length1 arr - 1 do
        for j in 0 .. Array2D.length2 arr - 1 do
            let accessible=seq {
                for (x,y) in adjacent i j do
                    if 0<=x && x<Array2D.length1 arr &&
                       0<=y && y<Array2D.length2 arr then
                        yield if arr[x,y]='@' then 1 else 0  
                             }
            yield if arr[i,j]='@' && Seq.sum(accessible)<4 then 1 else 0
     }|>Seq.sum

let Puzzle()=
    part1 Day4Year2025Inputs.data