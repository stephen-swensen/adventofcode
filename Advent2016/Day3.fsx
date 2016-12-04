#load "Prelude.fsx"
open System

let input =
    let lines = readLines "input3_part1.txt"
    lines 
    |> Seq.map (split " " >> (Seq.map int) >> Seq.toList) 
    |> Seq.toList

let possibleTriangle [x;y;z] =
    x + y > z && y + z > x && x + z > y

let countPossibleTriangles input =
    input
    |> Seq.map possibleTriangle
    |> Seq.filter id
    |> Seq.length

let part1 = countPossibleTriangles input

let part2 =
    let input' = 
        let rec loop xl = seq { 
            match xl with
            | [] -> ()
            | [x;y;z]::[x';y';z']::[x'';y'';z'']::xl' ->
                yield! [[x;x';x''];[y;y';y''];[z;z';z'']] 
                yield! loop xl' }
        loop input
    countPossibleTriangles input'