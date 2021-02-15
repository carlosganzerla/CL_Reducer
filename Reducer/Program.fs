// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Program

open System
open Parser
open CombinatoryLogic

let parseAndReduce = Console.ReadLine >> parseString >> reduce >> toString  

let run () = 
    try
        () |> Console.ReadLine |> parseString |> reduce |> toString 
           |> printfn "Weak normal form: %s"
    with
        | ex -> printfn "Error: %s" ex.Message

[<EntryPoint>]
let rec main argv =
    Console.WriteLine "Enter expression:"
    run ()
    main argv |> ignore
    0 // return an integer exit code