module adventofcode
open System

let readChars file =
    let text = System.IO.File.ReadAllText(file)
    text.ToCharArray()

let ``day 1`` =
    let chars = readChars @"c:\sb\adventofcode\adventofcode\input1.txt"
    let countCharOccurs c = chars |> Seq.filter (fun x -> x = c) |> Seq.length
    let up = countCharOccurs '('
    let down = countCharOccurs ')'
    up - down

let ``day 1, solution b`` =
    readChars @"c:\sb\adventofcode\adventofcode\input1.txt"
    |> Seq.sumBy (function '(' -> 1 | ')' -> -1)

let ``day 1 part 2`` =
    readChars @"c:\sb\adventofcode\adventofcode\input1.txt"
    |> Seq.map (function '(' -> 1 | ')' -> -1)
    |> Seq.scan (+) 0
    |> Seq.findIndex (fun x -> x = -1)

