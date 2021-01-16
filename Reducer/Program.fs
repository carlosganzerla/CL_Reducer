// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Program

open System
open Parser
open CombinatoryLogic

let run = (Console.ReadLine >> parseString >> toWeakNf >> toString 
    >> printfn "Weak NF: %s") 

[<EntryPoint>]
let rec main argv =
    Console.WriteLine "Enter expression:"
    run ()
    main argv |> ignore
    0 // return an integer exit code