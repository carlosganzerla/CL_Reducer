# CL Reducer
Implementation of a Combinatory Logic (**SKI** calculus) reducer in F#, running
interactively in the console. It parses user input to combinatory logic terms
and performs a series of contractions to find the weak normal form, printing the
response on the screen. If a term has no weak normal form, it displays an error
message. If the input is incorrect or invalid, an error message is also
displayed.in

## How to use it
Install [.NET](). Then, just navigate to the project directory:

```bash
    cd ~/../yourstuff/Reducer/Reducer
```
To build and run:
```bash
    dotnet build -c Release
    dotnet ./bin/Release/net5.0/Reducer.dll
```

To build and run as a standalone .exe:
```bash
    dotnet publish -c Release -o [YOUR_DIRECTORY]
```

To test:
```bash
    dotnet test
```

## Limitations
I could not make it able to detect if a term has no weak normal form using the
quasi-leftmost reduction theorem. I also think it's not possible to use the
theorem in a program, because there's no way to generalize an algorithm to pick
the correct non-leftmost reductions for every irreducible term.

## See also
[Lambda-Calculus and Combinators - An
Introduction](https://www.cin.ufpe.br/~djo/files/Lambda-Calculus%20and%20Combinators.pdf)


