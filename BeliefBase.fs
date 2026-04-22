module BeliefBase

open Formula

type BeliefBase = Formula Set

let toStringBB (b: BeliefBase) =
    if Set.isEmpty b then
        "{}"
    else
        let temp = b |> Set.map toStringF |> String.concat ", "
        "{ " + temp + " }"
