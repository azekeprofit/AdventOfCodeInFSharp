module Day8Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics


let lines data=[
    let l=Common.splitLines data
    for line in l do
        let point=Common.splitBy [|","|] line |>Seq.toArray
        match tryParseArray point with
            | Some(x,y,z) -> yield x,y,z
            | _ -> failwith "Wrong points format" ]

let sq x=x*x
let distanceSq (x,y,z) (a,b,c)=(sq x-a)+(sq y-b)+(sq z+c)
// no need to calculate square root



let Puzzle()=
    let points=lines Day8Year2025Inputs.example
    let r=[
            let indexed=List.indexed points
            for a,b in List.allPairs indexed indexed do
                let i,aPoint=a
                let j,bPoint=b
                yield distanceSq aPoint bPoint, i, j ]
            |>List.sortBy(fun (d,_,_) -> d)
    999