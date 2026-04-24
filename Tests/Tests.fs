module BeliefRevisionTests

open Xunit
open BeliefRevision
open Formula
open Resolution

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
