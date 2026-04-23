module BeliefRevision

open BeliefBase
open Resolution
open Formula

let powerSet s =
    s
    |> Set.fold (fun acc x -> acc |> Set.map (Set.add x) |> Set.union acc) (Set.singleton Set.empty)

let maximalSubsets kb f =
    //printfn "Pow: %A" (powerSet kb)
    let candidates = powerSet kb |> Set.filter (fun s -> not (entails s f))
    //printfn "Can: %A" candidates
    candidates
    |> Set.filter (fun s1 -> not (candidates |> Set.exists (fun s2 -> s1 <> s2 && Set.isSubset s1 s2)))

let contraction kb f =
    if not (entails kb f) then
        kb
    else
        let maximalSubsets = maximalSubsets kb f
        //printfn "Max: %A" maximalSubsets
        maximalSubsets |> Seq.maxBy (Seq.sumBy snd)

let expansion kb (f, p) : BeliefBase =
    match kb |> Seq.tryFind (fun (phi, pri) -> phi = f) with
    | None -> kb |> Set.add (f, p)
    | Some(_, oldP) -> kb |> Set.remove (f, oldP) |> Set.add (f, p)

let revision kb (f, p) = 
    expansion (contraction kb (Not f)) (f, p)
