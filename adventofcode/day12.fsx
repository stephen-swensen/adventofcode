[<AutoOpen>]
module day1

open System
open System.Numerics
open System.Text.RegularExpressions
//started with http://fssnip.net/8y
module Parser =
    type token =
        | Symbol of char
        | StrToken of string
        | NumToken of int

    let (|Match|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
        if m.Success then Some m.Value else None

    let unquote (s:string) = s.Substring(1,s.Length-2)

    //return the first token match in the input string. return value is the (raw match * token)
    let toToken = function
        | Match @"^""[^""\\]*(?:\\.[^""\\]*)*""" s -> s, s |> unquote |> StrToken
        | Match @"^\{|^\}|^\[|^\]|^:|^," s -> s, s.[0] |> Symbol
        //fix to be only integer and allow negatives
        | Match @"^(-)?\d+" s -> s, s |> int |> NumToken
        | _ -> invalidOp "Unknown token"

    //convert input string into a list of tokens. improved from the original by avoiding stackoverflow
    let tokenize (s:String) =
        List.unfold (fun index ->
            if index = s.Length then None
            else
                let next = s.Substring(index)
                let text, token = toToken next
                Some(token,text.Length+index)
        ) 0

    type json =
        | Number of int
        | String of string
        | Array of json list
        | Object of (string * json) list

    //t is "tail"
    let rec (|Value|_|) = function
        | NumToken(n)::t -> Some(Number(n),t)
        | StrToken(s)::t -> Some(String(s),t)
        //                       Symbol(']') pops the closing brace off the tail
        | Symbol('[')::Values(vs,Symbol(']')::t) -> Some(Array(vs),t)
        | Symbol('{')::Pairs(ps,Symbol('}')::t) -> Some(Object(ps),t)
        | _ -> None
    //match a single expression or a list of expressions like e1,e2,...
    and (|Values|_|) = function
        //match the first value, then find next recursively with aux thereafter
        | Value(p,t) ->
            let rec aux p' = function
                | Symbol(',')::Value(p,t) -> aux (p::p') t //next token is ',', keep going
                | t -> p' |> List.rev,t //no more expressions
            Some(aux [p] t)
        | _ -> None
    and (|Pair|_|) = function
        | StrToken(k)::Symbol(':')::Value(v,t) -> Some((k,v), t)
        | _ -> None
    and (|Pairs|_|) = function
        | Pair(p,t) ->
            let rec aux p' = function
                | Symbol(',')::Pair(p,t) -> aux (p::p') t
                | t -> p' |> List.rev,t
            Some(aux [p] t)
        | _ -> None

    let parse s = 
        tokenize s |> function 
        | Value(v,[]) -> v
        | _ -> failwith "Failed to parse JSON"

let json = Parser.parse (System.IO.File.ReadAllText(@"c:\sb\adventofcode\adventofcode\input12.txt"))

let part1 =
    let rec sum acc json =
        match json with
        | Parser.Array(xs) -> acc + List.sumBy (fun x -> sum acc x) xs
        | Parser.Object(xs) ->acc + List.sumBy (fun (_,x) -> sum acc x) xs
        | Parser.Number(x) -> acc + BigInteger(x)
        | Parser.String(_) -> acc + BigInteger(0)
    sum (BigInteger(0)) json

let part2 =
    let rec sum acc json =
        match json with
        | Parser.Array(xs) -> acc + List.sumBy (fun x -> sum acc x) xs
        | Parser.Object(xs) ->
            let hasRedValue =
                xs
                |> List.exists (fun (_,v) ->
                    match v with
                    | Parser.String("red") -> true
                    | _ -> false
                )
            if hasRedValue then
                acc + BigInteger(0)
            else
                acc + List.sumBy (fun (_,x) -> sum acc x) xs
        | Parser.Number(x) -> acc + BigInteger(x)
        | Parser.String(_) -> acc + BigInteger(0)
    sum (BigInteger(0)) json