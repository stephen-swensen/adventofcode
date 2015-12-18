[<AutoOpen>]
module day1_fast

open System
open System.Numerics
open System.Text.RegularExpressions
//started with http://fssnip.net/8y
module Parser =
    type token =
        | Symbol of char
        | StrToken of string
        | NumToken of int

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
                    NumToken(Int32.Parse(v)), firstNonNumCharIndex
                
                let readStr() =
                    let firstQuoteCharIndex = Array.FindIndex(s,index+1, fun c ->
                        c = '"'    
                    )
                    let sb = System.Text.StringBuilder()
                    ignore <| sb.Append(s.[(index+1)..firstQuoteCharIndex-1])
                    let v = sb.ToString()
                    StrToken(v), (firstQuoteCharIndex+1)

                let t, nextIndex =
                    match nextChar with
                    | '{' | '}' | '[' | ']' | ':' | ',' -> 
                        Symbol nextChar, index + 1
                    | '-' ->
                        readNum ()
                    | _ when nextChar |> isNumChar ->
                        readNum ()
                    | '"' ->
                        readStr ()
                    | _ -> failwithf "token not found: i=%A, nextChar=%A" index nextChar
                    
                Some(t,nextIndex)
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

let text = System.IO.File.ReadAllText(@"c:\sb\adventofcode\adventofcode\input12.txt")
let tokens = Parser.tokenize (text.ToCharArray())
let json = Parser.parse (text.ToCharArray())

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