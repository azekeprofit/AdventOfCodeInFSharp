module Common
open FSharpPlus

let splitLines text=
        String.split [| "\r\n"; "\n"; "\r" |] text
            |>Seq.filter(fun s -> String.length s>0)