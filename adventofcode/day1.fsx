[<AutoOpen>]
module day1

open System

let readChars file =
    let text = System.IO.File.ReadAllText(file)
    text.ToCharArray()

let ``day 1`` =
    readChars @"c:\sb\adventofcode\adventofcode\input1.txt"
    |> Seq.sumBy (function '(' -> 1 | ')' -> -1)

let ``day 1 part 2`` =
    readChars @"c:\sb\adventofcode\adventofcode\input1.txt"
    |> Seq.map (function '(' -> 1 | ')' -> -1)
    |> Seq.scan (+) 0
    |> Seq.findIndex (fun x -> x = -1)