module Parser

open System

open CombinatoryLogic

let private parseChar char  =
    match char with
    | 'S' -> S
    | 'K' -> K
    | 'I' -> I
    | other when Char.IsLetter(other)  -> Var char
    | other -> failwithf "Invalid char %c" other

let rec private parseChars chars =
    let rec loop (accumulator: CLTerm list) (context: CLTerm list list) = function
        | [] ->
            match context with
            | [] -> accumulator |> List.rev
            | _ -> failwith "Missing ')'"
        | '('::tail -> loop [] (accumulator::context) tail
        | ')'::tail ->
            match context with
            | top::stack ->
                let term = accumulator |> List.rev
                match term with
                | [] -> failwith "There cannot be empty terms"
                | _ -> loop (Term term::top) stack tail
            | _ -> failwith "Missing '('"
        | head::tail ->
            let term = parseChar head
            loop (term::accumulator) context tail
    loop [] [] chars

let parseString str =
    str |> Seq.toList |> parseChars
