module CNF

open Formula
open BeliefBase

let rec eliminateBiImply f =
    match f with
    | Var _ -> f
    | Not p -> Not(eliminateBiImply p)
    | Or(p, q) -> Or(eliminateBiImply p, eliminateBiImply q)
    | And(p, q) -> And(eliminateBiImply p, eliminateBiImply q)
    | Imply(p, q) -> Imply(eliminateBiImply p, eliminateBiImply q)
    | BiImply(p, q) -> And(Imply(eliminateBiImply p, eliminateBiImply q), Imply(eliminateBiImply q, eliminateBiImply p))

let rec eliminateImply f =
    match f with
    | Not p -> Not(eliminateImply p)
    | Or(p, q) -> Or(eliminateImply p, eliminateImply q)
    | And(p, q) -> And(eliminateImply p, eliminateImply q)
    | Imply(p, q) -> Or(Not(eliminateImply p), eliminateImply q)
    | _ -> f

let rec toNNF f =
    match f with
    | Not(Not p) -> toNNF p
    | Not(Or(p, q)) -> And(toNNF (Not p), toNNF (Not q))
    | Not(And(p, q)) -> Or(toNNF (Not p), toNNF (Not q))
    | Or(p, q) -> Or(toNNF p, toNNF q)
    | And(p, q) -> And(toNNF p, toNNF q)
    | Not p -> Not(toNNF p)
    | _ -> f

let rec distribute f =
    match f with
    | Or(And(p, q), r) -> And(distribute (Or(p, r)), distribute (Or(q, r)))
    | Or(p, And(q, r)) -> And(distribute (Or(p, q)), distribute (Or(p, r)))
    | Or(p, q) -> Or(distribute p, distribute q)
    | And(p, q) -> And(distribute p, distribute q)
    | _ -> f

let toCNF f =
    f |> eliminateBiImply |> eliminateImply |> toNNF |> distribute
