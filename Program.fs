open Parser
open CNF
open Formula
open BeliefBase

[<EntryPoint>]
let main argv =
    printf "Write a belief base: "
    let input = System.Console.ReadLine()

    let b = parseFormula input
    let cnf = toCNF b
    let s = toStringF cnf

    printfn "The belief base is: %s" s
    0
