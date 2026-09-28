module Day4Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

let map data= 
    String.split [| "\n"; "\r" |] data
    |>Seq.filter(fun s -> String.length(s)>0)
    |>Seq.map(function 
        s when Regex.IsMatch(s,@"^[\@\.]+$") -> s|>String.toSeq|>Seq.map(fun c-> if c='@' then 1uy else 0uy)
        | _ -> failwith "Incorrect map!" )
    |>array2D

let adjacent i j=seq{
        yield (i-1,j-1); yield (i-1,j); yield (i-1,j+1)
        yield (i,j-1);                  yield (i,j+1)
        yield (i+1,j-1); yield (i+1,j); yield (i+1,j+1)
        }

let adjacentCells arr i j=seq {
    for (x,y) in adjacent i j do
        if 0<=x && x<Array2D.length1 arr &&     // why do i have to do bounds checking manually?
           0<=y && y<Array2D.length2 arr then
            if arr[x,y]=1uy then yield 1uy }

            
let removable arr=seq {
    for i in 0 .. Array2D.length1 arr - 1 do
        for j in 0 .. Array2D.length2 arr - 1 do
            if arr[i,j]=1uy then
                let a=(adjacentCells arr i j)|>Seq.length
                if a<4 then yield (i,j) }


let part1 data=
    let arr= map data
    Seq.length(removable arr)

let iter arr=
    let r=Seq.toArray(removable arr)
    for (i,j) in r do
        arr[i,j]<-0uy
    r.Length

let part2 data=
    let arr= map data
    seq {
    let mutable lastRemovable=1
    while(lastRemovable<>0) do 
        lastRemovable<-iter arr
        yield lastRemovable
    }|>Seq.sum
    


let Puzzle()=
    part2 Day4Year2025Inputs.data