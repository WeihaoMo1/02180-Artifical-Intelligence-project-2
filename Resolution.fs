module Resolution

open BeliefBase
open CNF
open Formula

type Literal =
    | Pos of char
    | Neg of char

type Clause = Set<Literal>
type Clauses = Set<Clause>

let rec splitAnd f =
    match f with
    | And(p, q) -> splitAnd p @ splitAnd q
    | _ -> [ f ]

let rec splitOr f =
    match f with
    | Or(p, q) -> splitOr p @ splitOr q
    | _ -> [ f ]

let rec toLiteral f =
    match f with
    | Var x -> Pos x
    | Not(Var x) -> Neg x
    | _ -> failwith "Not a literal"

let toClauses f : Clauses =
    f
    |> splitAnd
    |> List.map (fun cf -> cf |> splitOr |> List.map toLiteral |> Set.ofList)
    |> Set.ofList


let complementary l1 l2 =
    match l1, l2 with
    | Pos x, Neg y when x = y -> true
    | Neg x, Pos y when x = y -> true
    | _ -> false

let resolve (c1: Clause) (c2: Clause) : Clause list =
    [ for l1 in c1 do
          for l2 in c2 do
              if complementary l1 l2 then
                  let newClause = Set.remove l1 c1 |> Set.union (Set.remove l2 c2)
                  yield newClause ]

let rec resolution (clauses: Clauses) =
    let newClauses =
        [ for c1 in clauses do
              for c2 in clauses do
                  if c1 <> c2 then
                      yield! resolve c1 c2 ]
        |> Set.ofList
        |> Set.filter (fun c -> not (Set.contains c clauses))

    if Set.exists Set.isEmpty newClauses then true
    elif Set.isEmpty newClauses then false
    else resolution (Set.union clauses newClauses)


let entails (kb: BeliefBase) f =
    let kbf = kb |> Set.toList |> List.reduce (fun acc f -> And(acc, f))
    let kbcnf = toCNF kbf
    let notfcnf = toCNF (Not f)
    let cnfContradiction = And(kbcnf, notfcnf)
    printfn "cnf for contradiction: %s" (toStringF cnfContradiction)
    let clauses = toClauses cnfContradiction
    printfn "clauses: %A" clauses
    resolution clauses
