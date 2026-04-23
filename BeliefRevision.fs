module BeliefRevision

open BeliefBase
open Resolution
open Formula

let contraction kb f =
    if not (entails kb f) then kb else Set.empty

let expansion kb f = Set.union kb (Set.singleton (f, 1))

let revision kb f = Set.empty
