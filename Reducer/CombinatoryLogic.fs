module CombinatoryLogic

type CLTerm = 
    | S
    | K
    | I
    | Var of char 
    | Term of CLTerm list

let identity x = Var x

let always x = fun () -> x

let private mapTerm fS fK fI fVar fTerm term =
    let rec loop accumulator = function
        | S::tail -> loop ((fS ())::accumulator) tail
        | K::tail -> loop ((fK ())::accumulator) tail
        | I::tail -> loop ((fI ())::accumulator) tail
        | Var var::tail -> loop ((fVar var)::accumulator) tail   
        | Term term::tail ->  loop ((fTerm term)::accumulator) tail
        | [] -> accumulator |> List.rev
    loop [] term

let mapInnerTerms =  mapTerm (always S) (always K) (always I) identity


let rec private clean term =
    let cleanAtoms term =  
        let trimmed = clean term
        match trimmed with
        | [ atom ] -> atom
        | _ -> Term (clean term)
    match term with
    | Term term::tail -> clean (term @ tail)
    | _ -> mapInnerTerms cleanAtoms term

let contract term =
    let cleaned = clean term
    match cleaned with
    | S::x::y::z::tail -> Some (x::z::(Term [ y; z; ])::tail)
    | I::x::tail
    | K::x::_::tail -> Some (x::tail)
    | _ when cleaned <> term -> Some cleaned
    | _ ->  None

let rec toWeakNf term =
    let contracted = contract term
    match contracted with
    | Some contraction -> toWeakNf contraction
    | None ->  term |> mapInnerTerms (Term << toWeakNf) |> clean

let rec toString term =
    let fS () = "S"
    let fk () = "K"
    let fI () = "I"
    let fVar var = new string([|var|]) 
    let fTerm term =  $"({toString term})"
    term |> mapTerm fS fk fI fVar fTerm |> System.String.Concat

