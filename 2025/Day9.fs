module Day9Year2025
open Common
open FSharpPlus


let infinity=1000
let minInf= -1000L
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

let getCutout (Point(sx,sy)) direction endd nextPoint=
    match direction, getDirection endd nextPoint with
        | Up, (dir,Horizonthal)
        | Down, (dir, Horizonthal) -> Point(sx+(if dir=VeersLeft then 1L else -1L),sy), Point((if dir=VeersLeft then infinity else minInf), y endd)
        | Left, (Vertical, dir) 
        | Right, (Vertical, dir)   -> Point(sx,sy+(if dir=VeersUp then 1L else -1L)), Point(x endd, (if dir=VeersUp then infinity else minInf))
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
    (min1<=min2 && min2<=max1) ||
    (min1<=max2 && max2<=max1) ||
    (min2<=min1 && min1<=max2) ||
    (min2<=max1 && max1<=max2)
    
let intersectRect (a,b) (c,d)=
    let byX=intersectBy x a b c d
    let byY=intersectBy y a b c d 
    byX && byY


let showBoard ab cd=
    let board=seq{ 
        "\n\n\n"
        for y in 0L..20L do
            for x in 0L..20L do
                let point=Point(x,y)
                let p=point,point
                match intersectRect p ab,intersectRect p cd with
                | true, false -> "|"
                | false, true -> "-"
                | true, true -> "+"
                | false,false -> "."
            "\n" }
    for line in board do printf "%s" line


let bbbb (a:point,b:point) (c:point,d:point)=
    showBoard (a,b) (c,d)
    let res=intersectRect (a,b) (c,d)
    printf "%s" (if res then "x" else "_")
    res


let part2 data=
    let points=parsePoints data|>Seq.toArray
    let cutouts=[
        for i in 0..points.Length-3 do
            cutout points[i] points[i+1] points[i+2]
        cutout points[points.Length-2] points[points.Length-1] points[0]
        cutout points[points.Length-1] points[0] points[1] ]

    
    seq{ for a,b in Seq.allPairs points points do
            if a>b && not (cutouts|>List.exists (bbbb (a,b))) 
                then area a b }|>Seq.max
        

let Puzzle()=part2 Day9Year2025Inputs.example