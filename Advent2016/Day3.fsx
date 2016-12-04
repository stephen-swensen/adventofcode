#load "Prelude.fsx"
open System

let input =
    let lines = readLines "input3_part1.txt"
    lines 
    |> Seq.map (split " " >> (Seq.map int) >> Seq.toList) 
    |> Seq.toList

let possibleTriangle [x;y;z] =
    x + y > z && y + z > x && x + z > y

let part1 =
    input
    |> Seq.map possibleTriangle
    |> Seq.filter id
    |> Seq.length