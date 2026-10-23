module Day9Year2025
open Common
open FSharpPlus


let infinity=System.Int64.MaxValue
type point=Point of int64*int64
let x (Point(x,y))=x
let y (Point(x,y))=y

let parsePoints data=seq{
    for line in Common.splitLines data do
        match String.split [| "," |] line |>Seq.map tryParse|>Seq.toList with
            | Some s :: Some e :: [] -> Point(s, e)
            | _ -> failwith "Incorrect points format" }

let area a b=
    (abs(x a-x b)+1L)*(abs(y a-y b)+1L)

type direction=Up | Down | Left | Right

type verticalDirection=Vertical|VeersLeft|VeersRight
type horizonthalDirection=Horizonthal|VeersUp|VeersDown


let getDirection a b=
        match x a-x b with
        | d when d>0 -> VeersRight 
        | d when d<0 -> VeersLeft
        | _ -> Vertical
        ,
        match y a-y b with
        | d when d>0 -> VeersUp
        | d when d<0 -> VeersDown
        | _ -> Horizonthal

let getCutout start direction endd nextPoint=
    match direction, getDirection endd nextPoint with
        | Up, (dir,Horizonthal)
        | Down, (dir, Horizonthal) -> start, Point((if dir=VeersLeft then infinity else 0), y endd)
        | Left, (Vertical, dir) 
        | Right, (Vertical, dir)   -> start, Point(x endd, (if dir=VeersUp then infinity else 0))
        | _ -> failwith "incorrect points data: points continue line instead of turning"


let cutout a b c=
    match getDirection a b with
        | Vertical, Horizonthal -> failwith "point doesn't move"
        | Vertical, dir    -> getCutout a (if dir=VeersUp then Up else Down) b c
        | dir, Horizonthal -> getCutout a (if dir=VeersLeft then Left else Right) b c
        | _ -> failwith "non cardinal direction"

let part1 data=
    let points=parsePoints data
    seq{ for a,b in Seq.allPairs points points do
            if a>b then area a b }
    |>Seq.max


let normalize (coord:point->int64) a b =
    let min=min (coord a) (coord b)
    let max=max (coord a) (coord b)
    min, max

let intersectBy coord a b c d=
    let min1, max1=normalize coord a b 
    let min2, max2=normalize coord c d
    min1 < max2 && min2 < max1

let intersectRect (a,b) (c,d)=
    intersectBy x a b c d && intersectBy y a b c d

let part2 data=
    let points=parsePoints data|>Seq.toArray
    let cutouts=[
        for i in 0..points.Length-3 do
            cutout points[i] points[i+1] points[i+2]
        cutout points[points.Length-2] points[points.Length-1] points[0]
        cutout points[points.Length-1] points[0] points[1] ]

    
    seq{ for a,b in Seq.allPairs points points do
            if a>b && not (cutouts|>List.exists (intersectRect (a,b))) 
                then area a b }|>Seq.max
        

let Puzzle()=part2 Day9Year2025Inputs.example