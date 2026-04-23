module Parser

open Formula
open BeliefBase

type Token =
    | VAR of char
    | NOT
    | AND
    | OR
    | IMPLY
    | BIIMPLY
    | LPAR
    | RPAR

let tokenize (s: string) =
    let rec loop i acc =
        if i >= s.Length then
            List.rev acc
        else
            match s.[i] with
            | ' ' -> loop (i + 1) acc
            | '!' -> loop (i + 1) (NOT :: acc)
            | '&' -> loop (i + 1) (AND :: acc)
            | '|' -> loop (i + 1) (OR :: acc)
            | '-' when i + 1 < s.Length && s.[i + 1] = '>' -> loop (i + 2) (IMPLY :: acc)
            | '<' when i + 1 < s.Length && s.[i + 1] = '-' && i + 2 < s.Length && s.[i + 2] = '>' ->
                loop (i + 3) (BIIMPLY :: acc)
            | '(' -> loop (i + 1) (LPAR :: acc)
            | ')' -> loop (i + 1) (RPAR :: acc)
            | c when System.Char.IsLetter c -> loop (i + 1) (VAR c :: acc)
            | _ -> failwithf "Invalid symbol: %c" s.[i]

    loop 0 []

let parseTokens tokens =
    let rec expr toks =
        match toks with
        | VAR c :: rest -> Var c, rest

        | NOT :: rest ->
            let p, rest2 = expr rest
            Not p, rest2

        | LPAR :: rest ->
            let left, rest1 = expr rest

            match rest1 with
            | AND :: rest2 ->
                let right, rest3 = expr rest2

                match rest3 with
                | RPAR :: rest4 -> And(left, right), rest4
                | _ -> failwith "Missing )"

            | OR :: rest2 ->
                let right, rest3 = expr rest2

                match rest3 with
                | RPAR :: rest4 -> Or(left, right), rest4
                | _ -> failwith "Missing )"

            | IMPLY :: rest2 ->
                let right, rest3 = expr rest2

                match rest3 with
                | RPAR :: rest4 -> Imply(left, right), rest4
                | _ -> failwith "Missing )"

            | BIIMPLY :: rest2 ->
                let right, rest3 = expr rest2

                match rest3 with
                | RPAR :: rest4 -> BiImply(left, right), rest4
                | _ -> failwith "Missing )"

            | _ -> failwith "Expected operator"

        | _ -> failwith "Unexpected token"

    let f, rest = expr tokens

    match rest with
    | [] -> f
    | _ -> failwith "Extra tokens after parsing"

let parseFormula s = tokenize s |> parseTokens

let parseBeliefBase (s: string) : BeliefBase =
    let s = s.Trim()

    if not (s.StartsWith "{" && s.EndsWith "}") then
        failwith "Belief base must start with { and end with }"

    let inner = s.Substring(1, s.Length - 2)

    if inner = "" then
        Set.empty
    else
        inner.Split ','
        |> Array.map (fun s ->
            let parts = s.Split ':'

            let mutable priority = 1

            if parts.Length = 2 then
                priority <- int parts.[1]
            else if parts.Length > 2 then
                failwith "Expected Formula : Priority"

            let formula = parseFormula parts.[0]

            formula, priority)
        |> Set.ofArray
