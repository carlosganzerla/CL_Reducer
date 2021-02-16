
module Program

open System
open Parser
open CombinatoryLogic

let parseAndReduce = parseString >> reduce >> toString

let rec run () =
    try
            Console.WriteLine "Enter expression:"
            |> Console.ReadLine
            |> parseAndReduce
            |> printfn "Weak normal form: %s"
            |> run
    with
        | ex -> printfn "Error: %s" ex.Message |> run

[<EntryPoint>]
let main argv =
    run ()
    0 // return an integer exit code
