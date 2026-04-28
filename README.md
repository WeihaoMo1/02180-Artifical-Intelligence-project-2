# 02180-Artifical-Intelligence-project-2

Belief Revision System in F#

## Requirements

This project uses:  
.NET SDK version 9.0.200  
F# is included automatically when installing the .NET SDK.

To install .NET (and F#), download the SDK from:  
https://dotnet.microsoft.com/en-us/download

Verify installation with:

```bash
dotnet --version
```

(should print 9.0.200 or a higher version)

## Run the project

From the project root run the following command:

```bash
dotnet run
```

## Run the tests

From the project root run the following command:

```bash
dotnet test
```

The tests verifies the AGM postulates for contraction and revision.

## Description

When the program starts, you must enter a belief base.  
Format example:

```bash
{ A:1, B:2, (!A & C):3 }
```

where the integers are the priority for each belief. If you just write:

```bash
{ A, B, (!A & C) }
```

the priority of each belief will be set to 1.  
You are allowed to write as many believes as you like seperated by comma. An empty belief base is also allowed.  
Make sure to write the correct format - otherwise the program will throw an error and terminate.

You can now choose:  
1 - Revision  
2 – Contraction  
3 – Expansion  
4 – Entailment check  
5 – New belief base

Option 1-3 will update the belief base based on which operation you choose. Option 4 will return a boolean stating whether or not the belief base entails the formula you write.

For option 1-4, enter a formula.  
You may optionally add a priority using:

```bash
<formula> : <priority>
```

Variables are single letters: A, B, C, q, r, s, ...

## Formula grammar (recursive syntax)

```bash
Formula ::=
      Var
    | ! Formula
    | ( Formula | Formula )
    | ( Formula & Formula )
    | ( Formula -> Formula )
    | ( Formula <-> Formula )
```

Where Var is a single single letter: A, B, C, a, b, c, ...

## Supported formula syntax (examples)

```bash
!A
(A & B)
(A | B)
(A -> B)
(A <-> B)
((A & B) -> (C | D))
...
```

## Example usage

```bash
Write a belief base: { A:1, B:2 }
Choose one of the following options:
1) Revision
2) Contraction
3) Expansion
4) Entailment
5) New belief base
1
Write a formula: (A -> B) : 3
{ (A -> B) : 3, A : 1, B : 2 }
Do you want to continue? y/n
n
Good bye
```
