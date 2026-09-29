module Common
open FSharpPlus

let splitBy delims text=
    String.split delims text
            |>Seq.filter(fun s -> String.length s>0)

let splitLines=splitBy [| "\r\n"; "\n"; "\r" |]
let splitBySpaces=splitBy [| " " |]
