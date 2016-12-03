#load "Prelude.fsx"
open System

///Up, Down, Left, Right
type Instruction = U | D | L | R
let unitVector = function
    | U -> (0,1)
    | D -> (0,-1)
    | L -> (-1,0)
    | R -> (1,0)

let keypad = [
    7, (0,0)
    8, (1,0)
    9, (2,0)
    4, (0,1)
    5, (1,1)
    6, (2,1)
    1, (0,2)
    2, (1,2)
    3, (2,2) ]

let keypadCoords = keypad |> List.map snd

let instructions =
    let input = readLines "input2_part1.txt"
    input
    |> Seq.map (fun line ->
        line |> Seq.map (fun c -> printfn "%A" c; match c with 'U' -> U | 'D' -> D | 'L' -> L | 'R' -> R))

let move ((x,y) as cur) inst =
    let (x',y') = unitVector inst
    let cur' = x+x',y+y'
    if keypadCoords |> Seq.contains cur' then cur'
    else cur

let nextKey initialCoord keyInstructions =
    keyInstructions
    |> Seq.fold move initialCoord

let part1 = 
    instructions
    |> Seq.scan nextKey (1,1)
    |> Seq.skip 1
    |> Seq.map (fun coord -> keypad |> Seq.find (fun (_,coord') -> coord=coord') |> fst)
    |> Seq.toList