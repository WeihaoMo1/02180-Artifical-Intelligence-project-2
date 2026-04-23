module BeliefBase

open Formula

type Priority = int
type BeliefBase = (Formula * Priority) Set

let toStringBB (b: BeliefBase) =
    if Set.isEmpty b then
        "{}"
    else
        let temp =
            b
            |> Set.map (fun (f, p) -> toStringF f + " : " + string p)
            |> String.concat ", "

        "{ " + temp + " }"

let toStringPrettyBB (b: BeliefBase) =
    if Set.isEmpty b then
        "{}"
    else
        let temp = b |> Set.map (fun (f, _) -> toStringF f) |> String.concat ", "

        "{ " + temp + " }"
