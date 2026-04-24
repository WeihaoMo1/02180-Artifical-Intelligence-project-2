module BeliefRevisionTests

open Xunit
open BeliefRevision
open Formula
open Resolution
open CNF

let A = Var 'A'
let B = Var 'B'
let C = Var 'C'

let kb = Set [ A, 1; B, 1 ] // KB = { A, B }

// Rationality Posulates of Contraction
[<Fact>]
let ``1. Closure - Contraction`` () =
    let phi = A

    let o = contraction kb phi

    Assert.False(entails o (And(A, Not A)))

    for phi, _ in o do
        Assert.True(entails o phi)

[<Fact>]
let ``2. Success - Contraction`` () =
    let phi = A

    let o = contraction kb phi

    Assert.False(entails o phi)

[<Fact>]
let ``3. Inclusion - Contraction`` () =
    let phi = A

    let o = contraction kb phi

    Assert.True(Set.isSubset o kb)

[<Fact>]
let ``4. Vacuity - Contraction`` () =
    let phi = C

    let o = contraction kb phi

    Assert.True((o = kb))

[<Fact>]
let ``5. Extensionality - Contraction`` () =
    let phi = Imply(A, B)
    let psi = Imply(Not B, Not A)

    let o1 = contraction kb phi
    let o2 = contraction kb psi

    Assert.True((o1 = o2))

[<Fact>]
let ``6. Recovery - Contraction`` () =
    let phi = A

    let o = expansion (contraction kb phi) (phi, 1)

    Assert.True(Set.isSubset kb o)

[<Fact>]
let ``7. Conjunctive inclusion - Contraction`` () =
    let newKB = Set [ A, 1; B, 2 ]
    let phi = A
    let psi = B

    let o1 = contraction newKB (And(phi, psi))
    let o2 = contraction newKB phi

    Assert.False(entails o1 phi)
    Assert.True(Set.isSubset o1 o2)

[<Fact>]
let ``8. Conjunctive overlap - Contraction`` () =
    let phi = A
    let psi = B

    let o1 = contraction kb phi
    let o2 = contraction kb psi
    let o3 = contraction kb (And(phi, psi))

    let intersection = Set.intersect o1 o2

    Assert.True(Set.isSubset intersection o3)


// Rationality Postulates of Revision
[<Fact>]
let ``1. Closure - Revision`` () =
    let phi = A

    let o = revision kb (phi, 1)

    Assert.False(entails o (And(A, Not A)))

    for phi, _ in o do
        Assert.True(entails o phi)

[<Fact>]
let ``2. Success - Revision`` () =
    let phi = A

    let o = revision kb (phi, 1)

    Assert.True(entails o phi)

[<Fact>]
let ``3. Inclusion - Revision`` () =
    let phi = A

    let o1 = revision kb (phi, 1)
    let o2 = expansion kb (phi, 1)

    Assert.True(Set.isSubset o1 o2)

[<Fact>]
let ``4. Vacuity - Revision`` () =
    let phi = C

    let o1 = revision kb (phi, 1)
    let o2 = expansion kb (phi, 1)

    Assert.False(entails kb (Not phi))
    Assert.True((o1 = o2))

[<Fact>]
let ``5. Consistency - Revision`` () =
    let phi = A

    let o = revision kb (phi, 1)

    Assert.False(entails o (And(A, Not A)))

[<Fact>]
let ``6. Extensionality - Revision`` () =
    let phi = Imply(A, B)
    let psi = Imply(Not B, Not A)

    let o1 = revision kb (phi, 1)
    let o2 = revision kb (psi, 1)

    Assert.True(entails o1 phi)
    Assert.True(entails o1 psi)
    Assert.True(entails o2 phi)
    Assert.True(entails o2 psi)

[<Fact>]
let ``7. Superexpansion - Revision`` () =
    let phi = A
    let psi = B

    let left = revision kb (And(phi, psi), 1)
    let right = expansion (revision kb (phi, 1)) (psi, 1)

    let checkEntailment = left |> Set.forall (fun (f, _) -> entails right f)

    Assert.True checkEntailment

[<Fact>]
let ``8. Subexpansion - Revision`` () =
    let phi = A
    let psi = C

    let o = revision kb (phi, 1)

    let left = expansion (revision kb (phi, 1)) (psi, 1)
    let right = revision kb (And(phi, psi), 1)

    let checkEntailment = left |> Set.forall (fun (f, _) -> entails right f)

    Assert.False(entails o psi)
    Assert.True checkEntailment
