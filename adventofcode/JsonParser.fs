namespace Swensen.Json

open System
open System.Numerics
open System.Text.RegularExpressions

type jtoken =
    | LBrace | RBrace | Colon | Comma | LBracket | RBracket
    | String of string
    | Int of int
type T = jtoken

type jexpr =
    | Int of int
    | String of string
    | Array of jexpr list
    | Object of (string * jexpr) list
type E = jexpr

//started with http://fssnip.net/8y
module Parser =
    type System.String with
        member this.IndexOf(startIndex:int, predicate:(char -> bool)) =
            let rec aux cur = 
                if cur = this.Length then
                    -1
                elif predicate(this.Chars(cur)) then
                    cur
                else  
                    aux (cur+1)
            aux startIndex

    let tokenize (s:string) =
        let isNumChar c = let cint = int(c) in cint >= 48 && cint <= 57

        List.unfold (fun index ->
            if index = s.Length then None
            else
                let nextChar = s.[index]
                let t, nextIndex =
                    match nextChar with
                    | '{' -> T.LBrace, index+1
                    | '}' -> T.RBrace, index+1
                    | '[' -> T.LBracket, index+1
                    | ']' -> T.RBracket, index+1
                    | ':' -> T.Colon, index+1
                    | ',' -> T.Comma, index+1
                    | _ when nextChar = '-' || nextChar |> isNumChar ->
                        let firstNonNumCharIndex = s.IndexOf(index+1, isNumChar>>not)
                        T.Int(Int32.Parse(s.Substring(index, firstNonNumCharIndex-index))), firstNonNumCharIndex
                    | '"' ->
                        let firstQuoteCharIndex = s.IndexOf(index+1, (=) '"')
                        T.String(s.Substring(index+1, firstQuoteCharIndex-index-1)), firstQuoteCharIndex+1
                    | _ -> failwithf "token not found: i=%A, nextChar=%A" index nextChar
                    
                Some(t,nextIndex)
        ) 0

    let parse tokens = 
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

        match tokens with
        | Value(v,[]) -> //if remainder is non-empty list of tokens, we had a problem
            v
        | _ ->
            failwith "Failed to parse JSON"