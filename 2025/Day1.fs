module Day1Year2025

open FSharpPlus


type Command= Left of int | Right of int

let lines data= seq {
    for line in String.split [| "\n"; "\r" |] data do     
                let letter=line|>String.take 1
                let number=line|>String.skip 1 |> tryParse<int>
                match letter, number with
                    | "L", Some n -> yield Left n
                    | "R", Some n -> yield Right n
                    | _ -> () }
    
let part1 data = seq {
        let mutable wheel=50
        for c in lines data do
            wheel<- (match c with | Left n->wheel-n+100; | Right n->wheel+n)%100
            yield if wheel=0 then 1 else 0 } |> Seq.sum



type accumulator= { wheel: int; zeroes: int }

let turns n=abs n/100

let turn {zeroes=z ; wheel=w} n=
    let sum=w+n 
    { wheel=sum%100 |> function s when s<0 -> 100+s; | s -> s
      zeroes= match sum with 
                | 0 -> z+1
                | over100 when over100>=100 -> z + turns over100
                | negative when negative<0 ->  z + (if w=0 then 0 else 1) + turns negative
                | s -> z + turns s
     }
    
let part2 data =
    let result=fold turn {wheel=50; zeroes=0} (data|>lines|>Seq.map(function Left n -> -n ; | Right n -> n))
    result.zeroes                    
    

let Puzzle() =part2 Day1Year2025Inputs.data