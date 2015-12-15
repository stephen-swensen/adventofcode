[<AutoOpen>]
module day7
open System

type Expr =
    | Var of string
    | Value of uint16
    | And of Expr * Expr
    | Or of Expr * Expr
    | LShift of Expr * int
    | RShift of Expr * int
    | Not of Expr

let rec eval expr (env:System.Collections.Generic.Dictionary<_,Lazy<_>>) =
    match expr with
    | Var x -> (env.[x]).Force()
    | Value x -> x
    | And (x, y) -> eval x env &&& eval y env
    | Or (x, y) -> eval x env ||| eval y env
    | LShift (x, y) -> eval x env <<< y
    | RShift (x, y) -> eval x env >>> y
    | Not x -> ~~~ (eval x env)

let rec evalStmts stmts env =
    match stmts with
    | (var, expr)::stmts ->
        let x = lazy(eval expr env)
        env.[var] <- x
        evalStmts stmts env
    | [] ->
        env

let parseStmt (str:string) = 
    let (|VarOrValue|) x =
        let xval = ref 0us
        if UInt16.TryParse(x, xval) then
            Value(!xval)
        else
            Var(x)
        
    let tokens = str.Split(' ')
    match tokens with
    | [|VarOrValue x;"->";ident|] -> 
        (ident, x)
    | [|VarOrValue x; "AND"; VarOrValue y; "->";ident|] -> 
        (ident, And(x, y))
    | [|VarOrValue x; "OR"; VarOrValue y; "->";ident|] -> 
        (ident, Or(x, y))
    | [|VarOrValue x; "LSHIFT"; y; "->";ident|] -> 
        (ident, LShift(x, Int32.Parse(y)))
    | [|VarOrValue x; "RSHIFT"; y; "->";ident|] -> 
        (ident, RShift(x, Int32.Parse(y)))
    | [|"NOT"; VarOrValue x ; "->";ident|] -> 
        (ident, Not(x))
    | _ -> failwithf "input not recognized: %A" tokens

let part1 =
    let lines = System.IO.File.ReadAllLines(@"c:\sb\adventofcode\adventofcode\input7.txt")
    let stmts = lines |> Seq.map parseStmt |> Seq.toList
    let env = evalStmts stmts (System.Collections.Generic.Dictionary<_,_>())
    (env.["a"]).Force()

let part2 =
    let lines = System.IO.File.ReadAllLines(@"c:\sb\adventofcode\adventofcode\input7_part2.txt")
    let stmts = lines |> Seq.map parseStmt |> Seq.toList
    let env = evalStmts stmts (System.Collections.Generic.Dictionary<_,_>())
    (env.["a"]).Force()