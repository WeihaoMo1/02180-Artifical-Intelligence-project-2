open Parser
open Resolution

[<EntryPoint>]
let main argv =
    printf "Write a belief base: "
    let kbStr = System.Console.ReadLine()

    printf "Write a formula: "
    let fStr = System.Console.ReadLine()

    let kb = parseBeliefBase kbStr
    let f = parseFormula fStr
    let entails = entails kb f

    printfn "entails: %b" entails
    0
