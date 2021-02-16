
module ParserTests

open FsUnit.Xunit
open Xunit

open CombinatoryLogic
open Parser

let mustThrow func arg =
    try
        func arg |> ignore
        Assert.True(false)
    with
    | :? Sdk.TrueException ->
        failwith "The test did not throw an exception, but should have."
    | _ -> ()

[<Fact>]
let ``Parsing 'S' must return combinator S`` () =
    "S" |> parseString |> should equal [ S ]

[<Fact>]
let ``Parsing 'K' must return combinator K`` () =
    "K" |> parseString |> should equal [ K ]

[<Fact>]
let ``Parsing 'I' must return combinator I`` () =
    "I" |> parseString |> should equal  [ I ]

[<Fact>]
let ``Parsing alphabetic chars should return Vars`` () =
    let vars = "ABCDEFGHJLMNOPQRTUVWXYabcdefghijklmnopqrstuvwxyz"
    let expected = vars |> Seq.map Var |> Seq.toList
    vars |> parseString |>  should equal expected

[<Fact>]
let ``Parsing any non-alphabetic char should thrown an exception`` () =
    "1234567890!@#$%¨&*()[]{}" |> (mustThrow parseString)

[<Fact>]
let ``Parsing string with no parens should return a plain list`` () =
    "abc" |> parseString |> should equal [ Var 'a'; Var 'b'; Var 'c' ]


[<Fact>]
let ``Parsing a string with parens creates terms`` () =
    let input = [
        "(abc)"
        "(((a)))";
        "a((bc)de)";
        "((a)b)c";
    ]
    let expected = [
        [ Term [ Var 'a'; Var 'b'; Var 'c'; ] ];
        [ Term [ Term [ Term [ Var 'a' ] ] ] ];
        [ Var 'a'; Term [ Term [ Var 'b'; Var 'c'; ]; Var 'd'; Var 'e'] ];
        [ Term  [ Term [Var 'a'; ]; Var 'b'; ]; Var 'c' ];
    ]
    input |> List.map parseString |> should equal expected


[<Fact>]
let ``Parsing large term must yield correct result`` () =
    "KSIabc(de(fg)h)" |> parseString |> should equal [
        K;
        S;
        I;
        Var 'a';
        Var 'b';
        Var 'c';
        Term [ Var 'd'; Var 'e'; Term [ Var 'f'; Var 'g' ]; Var 'h' ]
    ]

[<Fact>]
let ``Parsing a term with unbalanced parens must throw exception`` () =
    [
        ")";
        "(";
        "abc)";
        "abc(de))";
        "I(K";
        "((((I)))";
    ] |> List.iter (mustThrow parseString)

[<Fact>]
let ``Parsing term with empty parens must throw an exception`` () =
    [
        "()";
        "abcd(e())";
        "((()))"
        "()a";
        "a()";
    ] |> List.iter (mustThrow parseString)

