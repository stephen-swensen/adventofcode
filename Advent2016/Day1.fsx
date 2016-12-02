#load "Prelude.fsx"
open System

type Orientation = N | S | E | W
type Movement = R of int | L of int
let movements = 
    let input = readText "input1_part1.txt"
    input 
    |> split ", "
    |> Seq.map (fun movement ->
        match movement |> Seq.toList with
        | 'R'::Int'(m) -> R m
        | 'L'::Int'(m) -> L m)
    |> Seq.toList

let rec walk movements ((cur_x, cur_y) as cur) orientation =
    match movements with
    | [] -> cur
    | movement::movements' ->
        match movement with
        | R m -> 
            match orientation with
            | N -> walk movements' (cur_x + m, cur_y) E 
            | E -> walk movements' (cur_x, cur_y - m) S 
            | S -> walk movements' (cur_x - m, cur_y) W 
            | W -> walk movements' (cur_x, cur_y + m) N 
        | L m -> 
            match orientation with
            | N -> walk movements' (cur_x - m, cur_y) W 
            | W -> walk movements' (cur_x, cur_y - m) S 
            | S -> walk movements' (cur_x + m, cur_y) E 
            | E -> walk movements' (cur_x, cur_y + m) N 

let (x,y) = walk movements (0,0) N
let distance = Math.Abs(x) + Math.Abs(y)