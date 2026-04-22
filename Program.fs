open Parser

open BeliefBase

[<EntryPoint>]
let main argv =
    printf "Write a belief base: "
    let input = System.Console.ReadLine()

    let b = parseBeliefBase input
    let s = toStringBB b

    printfn "The belief base is: %s" s
    0
