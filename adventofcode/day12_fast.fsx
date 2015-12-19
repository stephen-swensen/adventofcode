[<AutoOpen>]
module day12_fast

open System
open System.Numerics
open System.Text.RegularExpressions
//started with http://fssnip.net/8y
module Parser =
    type jtoken =
        | LBrace | RBrace | Colon | Comma | LBracket | RBracket
        | String of string
        | Int of int
    type T = jtoken

    let tokenize (s:char[]) =
        let isNumChar c = let cint = int(c) in cint >= 48 && cint <= 57

        List.unfold (fun index ->
            if index = s.Length then None
            else
                let nextChar = s.[index]
                let readNum () =
                    let firstNonNumCharIndex = Array.FindIndex(s,index+1, fun c ->
                        isNumChar c |> not
                    )
                    let sb = System.Text.StringBuilder(11)
                    ignore <| sb.Append(s.[index..firstNonNumCharIndex-1])
                    let v = sb.ToString()
                    T.Int(Int32.Parse(v)), firstNonNumCharIndex
                
                let readStr() =
                    let firstQuoteCharIndex = Array.FindIndex(s,index+1, fun c ->
                        c = '"'    
                    )
                    let sb = System.Text.StringBuilder()
                    ignore <| sb.Append(s.[(index+1)..firstQuoteCharIndex-1])
                    let v = sb.ToString()
                    T.String(v), (firstQuoteCharIndex+1)

                let t, nextIndex =
                    match nextChar with
                    | '{' -> 
                        T.LBrace, index+1
                    | '}' -> 
                        T.RBrace, index+1
                    | '[' -> 
                        T.LBracket, index+1
                    | ']' -> 
                        T.RBracket, index+1
                    | ':' -> 
                        T.Colon, index+1
                    | ',' -> 
                        T.Comma, index+1
                    | '-' ->
                        readNum ()
                    | _ when nextChar |> isNumChar ->
                        readNum ()
                    | '"' ->
                        readStr ()
                    | _ -> failwithf "token not found: i=%A, nextChar=%A" index nextChar
                    
                Some(t,nextIndex)
        ) 0

    type jexpr =
        | Int of int
        | String of string
        | Array of jexpr list
        | Object of (string * jexpr) list
    type E = jexpr

    //token list -> (expression production * remaining token list)
    //t is "tail"
    let rec (|Value|_|) tokens = 
        match tokens with        
        | T.Int(n)::t -> 
            Some(E.Int(n),t)
        | T.String(s)::t -> 
            Some(E.String(s),t)
        //                    T.RBrace pops the closing brace off the tail
        | T.LBracket::Values(vs,T.RBracket::t) ->
            Some(E.Array(vs),t)
        | T.LBrace::Pairs(ps,T.RBrace::t) -> 
            Some(E.Object(ps),t)
        | _ -> None
    //match a single expression or a list of expressions like e1,e2,...
    and (|Values|_|) tokens = 
        match tokens with        
        //match the first value, then find next recursively with aux thereafter
        | Value(p,t) ->
            let rec aux p' tokens' = 
                match tokens' with
                | T.Comma::Value(p,t) ->
                     aux (p::p') t //next token is ',', keep going
                | t -> 
                    p' |> List.rev,t //no more expressions
            Some(aux [p] t)
        | _ -> None
    and (|Pair|_|) tokens = 
        match tokens with
        | T.String(k)::T.Colon::Value(v,t) -> 
            Some((k,v), t)
        | _ -> 
            None
    and (|Pairs|_|) tokens = 
        match tokens with
        | Pair(p,t) ->
            let rec aux p' = function
                | T.Comma::Pair(p,t) -> 
                    aux (p::p') t
                | t -> 
                    p' |> List.rev,t
            Some(aux [p] t)
        | _ -> None

    let parse s = 
        let tokens = tokenize s
        match tokens with
        | Value(v,[]) -> //if remainder is non-empty list of tokens, we had a problem
            v
        | _ ->
            failwith "Failed to parse JSON"

let text = System.IO.File.ReadAllText(@"c:\sb\adventofcode\adventofcode\input12.txt")
let tokens = Parser.tokenize (text.ToCharArray())
let json = Parser.parse (text.ToCharArray())

let part1 =
    let rec sum acc json =
        match json with
        | Parser.E.Array(xs) -> acc + List.sumBy (fun x -> sum acc x) xs
        | Parser.E.Object(xs) ->acc + List.sumBy (fun (_,x) -> sum acc x) xs
        | Parser.E.Int(x) -> acc + BigInteger(x)
        | Parser.E.String(_) -> acc + BigInteger(0)
    sum (BigInteger(0)) json

let part2 =
    let rec sum acc json =
        match json with
        | Parser.E.Array(xs) -> acc + List.sumBy (fun x -> sum acc x) xs
        | Parser.E.Object(xs) ->
            let hasRedValue =
                xs
                |> List.exists (fun (_,v) ->
                    match v with
                    | Parser.E.String("red") -> true
                    | _ -> false
                )
            if hasRedValue then
                acc + BigInteger(0)
            else
                acc + List.sumBy (fun (_,x) -> sum acc x) xs
        | Parser.E.Int(x) -> acc + BigInteger(x)
        | Parser.E.String(_) -> acc + BigInteger(0)
    sum (BigInteger(0)) json