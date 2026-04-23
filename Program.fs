open Parser
open Resolution
open BeliefBase
open BeliefRevision
open Formula

[<EntryPoint>]
let main argv =
    printf "Write a belief base: "
    let kbStr = System.Console.ReadLine()
    let mutable kb = parseBeliefBase kbStr

    let rec loop () =
        printfn "Choose one of the following options:"
        printfn "1) Revision"
        printfn "2) Contraction"
        printfn "3) Expansion"
        printfn "4) Entailment"
        printfn "5) New belief base"
        let opt = int (System.Console.ReadLine())

        let mutable f = null
        let mutable p = 1

        if opt <> 5 then
            printf "Write a formula: "
            let fStr = System.Console.ReadLine()
            let parts = (fStr.Trim()).Split ':'

            if parts.Length = 2 then
                p <- int parts.[1]
            else if parts.Length > 2 then
                failwith "Expected Formula : Priority"

            f <- parseFormula parts.[0]

        let result =
            match opt with
            | 1 ->
                kb <- revision kb (f, p)
                toStringBB kb
            | 2 ->
                kb <- contraction kb f
                toStringBB kb
            | 3 ->
                kb <- expansion kb (f, p)
                toStringBB kb
            | 4 ->
                printfn "%s ⊨ %s" (toStringPrettyBB kb) (toStringF f)
                string (entails kb f)
            | 5 ->
                printf "Write new belief base: "
                let newKb = System.Console.ReadLine()
                kb <- parseBeliefBase newKb
                toStringBB kb
            | _ -> "Invalid option"

        printfn "%s" result

        if opt <> 5 then
            printfn "Do you want to continue? y/n"
            let con = System.Console.ReadLine()

            if con = "y" then loop () else printfn "Good bye"
        else
            loop ()

    loop ()
    0
