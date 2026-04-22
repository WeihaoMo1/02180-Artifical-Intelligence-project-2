module Formula

type Formula =
    | Var of char
    | Not of Formula
    | Or of Formula * Formula
    | And of Formula * Formula
    | Imply of Formula * Formula
    | BiImply of Formula * Formula

let rec toStringF f =
    match f with
    | Var x -> string x
    | Not p -> "!" + toStringF p
    | Or(p, q) -> "(" + toStringF p + " | " + toStringF q + ")"
    | And(p, q) -> "(" + toStringF p + " & " + toStringF q + ")"
    | Imply(p, q) -> "(" + toStringF p + " -> " + toStringF q + ")"
    | BiImply(p, q) -> "(" + toStringF p + " <-> " + toStringF q + ")"
