module CombinatoryLogic

type CLTerm =
    | S
    | K
    | I
    | Var of char
    | Term of CLTerm list

let identity x = Var x

let always x () = x

let private mapTerm fS fK fI fVar fTerm term =
    let rec loop accumulator = function
        | S::tail -> loop ((fS ())::accumulator) tail
        | K::tail -> loop ((fK ())::accumulator) tail
        | I::tail -> loop ((fI ())::accumulator) tail
        | Var var::tail -> loop ((fVar var)::accumulator) tail
        | Term term::tail ->  loop ((fTerm term)::accumulator) tail
        | [] -> accumulator |> List.rev
    loop [] term

let private mapInnerTerms =  mapTerm (always S) (always K) (always I) identity

let rec contract term =
    match term with
    | Term term::tail -> contract (term @ tail)
    | S::x::y::z::tail -> Some (x::z::(Term [ y; z; ])::tail)
    | I::x::tail
    | K::x::_::tail -> Some (x::tail)
    | _ ->  None

let rec toString term =
    let fS () = "S"
    let fk () = "K"
    let fI () = "I"
    let fVar var = new string([|var|])
    let fTerm term =  $"({toString term})"
    term |> mapTerm fS fk fI fVar fTerm |> System.String.Concat

let rec length term =
    let fAtom _ = 1
    term |> mapTerm fAtom fAtom fAtom fAtom length |> List.reduce (+)

let private extract = function
    | Ok ok -> ok
    | Error err -> failwith err

let reduce term =
    let maxIterations = term |>  length |> (*) 1000
    let rec reduce i term =
        if i >= maxIterations then
            failwithf @"Could not reach the terminus after %d iterations. Term most likely does not have a weak normal form." i
        else
            match contract term with
            | Some contracted -> reduce (i + 1) contracted
            | None -> term |> mapInnerTerms (Term << reduce i)
    reduce 0 term
