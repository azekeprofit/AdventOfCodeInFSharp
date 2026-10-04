module Day5Year2025
open FSharpPlus
open System
open System.Text.RegularExpressions
open System.Numerics

type range=Range of bigint * bigint
type ingredient=Ingredient of bigint

let parseRanges s=[
    for line in Common.splitLines s do
        let r=String.split [| "-" |] line |>Seq.map tryParse|>Seq.toList
        match r with
            | Some s :: Some e :: [] -> Range(s, e)
            | _ -> failwith "Incorrect range format" ]

let parseIngredients s=[
    for line in Common.splitLines s do
        match tryParse line with
            | Some s -> Ingredient s
            | _-> failwith "Incorrect ingredient id format" ]

let parse data=
    let arr=Regex.Split(data, @"\n\r?\n\r?")
    if arr.Length<>2 then failwith "Incorrect input format"
    (parseRanges arr[0], parseIngredients arr[1])
        
let spoiled (Ingredient i) ranges=
    ranges|>List.exists(function Range(s, e) -> s<=i && i<=e) 

let part1 data=
    let ranges,ingredients=parse data
    ingredients
    |>Seq.sumBy(fun i -> if spoiled i ranges then 1 else 0)


type intersections=
    | Intersection of bigint * bigint * int
    | FullyEnvelops

type indexedRanges=IndexedRange of bigint * bigint * int
let indexRanges ranges=
    ranges|>Array.mapi(fun i (Range(s,e)) -> IndexedRange(s,e,i))

[<TailCall>]
let rec iter aligned (Range(s,e))=
    let intersection=indexRanges aligned
                    |>Array.tryPick(function 
                         |IndexedRange(a, b, i) when a<=s && s<=b && a<=e && e<=b -> Some(FullyEnvelops)
                         |IndexedRange(a, b, i) when s<=a && a<=e && s<=b && b<=e -> Some(Intersection(s, e, i))
                         |IndexedRange(a, b, i) when a<=s && s<=b -> Some(Intersection(a, max b e, i))
                         |IndexedRange(a, b, i) when a<=e && e<=b -> Some(Intersection(min a s, b, i))
                         | _ -> None)

    match intersection with
        | Some FullyEnvelops -> aligned
        | None -> [| yield! aligned; yield Range(s,e) |]
        | Some(Intersection(a,b,i)) -> 
            let newAligned=aligned |> Array.removeAt i
            iter newAligned (Range(a,b))


let part2 data=
    let ranges,_=parse data
    let aligned=fold iter Array.empty ranges
    aligned |> Array.sumBy(function Range(s,e) -> e-s+1I)
        

let Puzzle()=
    part2 Day5Year2025Inputs.data