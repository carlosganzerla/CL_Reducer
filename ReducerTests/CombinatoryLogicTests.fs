module CombinatoryLogicTests

open FsUnit.Xunit
open Xunit

open CombinatoryLogic
open Parser
open ParserTests

[<Fact>]
let ``S redex contracts correctly`` () =
    [ S; Var 'a'; Var 'b'; Var 'c'; Var 'd' ] |> contract
    |> should equal (Some [ Var 'a'; Var 'c'; Term [ Var 'b'; Var 'c' ]; Var 'd'])

[<Fact>]
let ``S redex should not contract if there are terms missing`` () =
    let attempts = [
        [ S; ];
        [ S; Var 'c' ];
        [ S; Var 'a'; Var 'b'; ];
    ]
    attempts |> List.fold (fun x att -> x && (contract att = None)) true
             |> should equal true

[<Fact>]
let ``I redex contracts correctly`` () =
    [ I; Var 'a'; Var 'b'; ] |> contract |> should equal (Some [ Var 'a'; Var 'b' ])

[<Fact>]
let ``I redex does not contract on its own`` () =
    [ I ] |> contract |> should equal None

[<Fact>]
let ``K redex should contract correctly`` () =
    [ K; Var 'a'; Var 'b'; Var 'c' ] |> contract |> should equal (Some [ Var 'a'; Var 'c'; ])

[<Fact>]
let ``K redex does not contract when one term is missing`` () =
    [ K; Var 'a' ] |> contract |> should equal None

[<Fact>]
let ``Contraction flattens leftmost terms`` () =
    [ Term [ I; Var 'a' ]; Term [ Var 'b'; Var 'c'; ]; Var 'd'; ]
    |> contract |> should equal (Some [ Var 'a'; Term [ Var 'b'; Var 'c'; ]; Var 'd'; ])

[<Fact>]
let ``Normal forms are found correctly`` () =
    let terms = [
        "SIKx";
        "SSKxy";
        "S(SK)xy";
        "S(KS)Sxyz";
        "SS(KS)KS(KS)KIxy";
    ]
    let expected = [
        [Var 'x'; Term [K; Var 'x']];
        [Var 'x'; Var 'y'; Term [ Var 'x' ] ];
        [ Term [ Var 'x'; Var 'y' ]];
        [Var 'x'; Term [Var 'y'; Var 'z']; Term [Var 'z'; Term [Var 'y'; Var 'z']]];
        [ Term [ Var 'x'; Var 'y' ] ];
    ]
    terms |> List.map (parseString >> reduce) |> should equal expected

[<Fact>]
let ``Terms are converted to string correctly`` () =
    let terms = [
        [ S; Term [ K; Var 'x'; Var 'z'; Term [ S; I; K ]; Var 'x' ]; ]
        [ S; I; K; Var 'x']
        [ Term [ Term [ Term [ Var 'x'; Var 'y'; ] ]; K ] ]
    ]
    let expected = [
        "S(Kxz(SIK)x)";
        "SIKx";
        "(((xy))K)";
    ]
    terms |> List.map toString |> should equal expected

[<Fact>]
let ``Term lengths are calcualted correctly`` () =
    let terms = [
        [ S; Term [ K; Var 'x'; Var 'z'; Term [ S; I; K ]; Var 'x' ]; ];
        [ S; I; K; Var 'x'];
        [ Term [ Term [ Term [ Var 'x'; Var 'y'; ] ]; K ] ];
        [ S; I; I; Term [ S; I; I; ] ];
    ]
    let expected = [
        8;
        4;
        3;
        6;
    ]
    terms |> List.map length |> should equal expected

[<Fact>]
let ``Term without normal form throws error eventually`` () =
    let term = [ S; I; I; Term [ S; I; I; ] ]
    term |> mustThrow reduce


