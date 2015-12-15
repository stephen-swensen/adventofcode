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

let rec eval expr env =
    match expr with
    | Var x -> env |> Map.find x
    | Value x -> x
    | And (x, y) -> eval x env &&& eval y env
    | Or (x, y) -> eval x env ||| eval y env
    | LShift (x, y) -> eval x env <<< y
    | RShift (x, y) -> eval x env >>> y
    | Not x -> ~~~ (eval x env)

let rec evalStmts stmts env =
    match stmts with
    | (var, expr)::stmts ->
        let x = eval expr env
        evalStmts stmts (env |> Map.add var x)
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

let lines = System.IO.File.ReadAllLines(@"c:\sb\adventofcode\adventofcode\input7.txt")
let stmts = lines |> Seq.map parseStmt |> Seq.toList
let env = evalStmts stmts Map.empty
let answer = env |> Map.find "a"

let input = "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"

let ``day 7 part 1 formal`` =
    let stmts = [
        "x", Value(123us)
        "y", Value(456us)
        "d", (And(Var("x"), Var("y")))
        "e", (Or(Var("x"), Var("y")))
        "f", (LShift(Var("x"), 2))
        "g", (RShift(Var("y"), 2))
        "h", (Not(Var("x")))
        "i", (Not(Var("y")))
    ]
    evalStmts stmts Map.empty

let ``day 7 part 1`` =
    let mutable env = Map.empty
    env <- env.Add ('x', 123us)
    env <- env.Add ('y', 456us)
    env <- env.Add ('d', env.['x'] &&& env.['y'])
    env <- env.Add ('e', env.['x'] ||| env.['y'])
    env <- env.Add ('f', env.['x'] <<< 2)
    env <- env.Add ('g', env.['y'] >>> 2)
    env <- env.Add ('h', ~~~ env.['x'])
    env <- env.Add ('i', ~~~ env.['y'])
    env
