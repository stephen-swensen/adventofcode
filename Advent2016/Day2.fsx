#load "Prelude.fsx"
open System

///Up, Down, Left, Right
type Instruction = U | D | L | R
let unitVector = function
    | U -> (0,1)
    | D -> (0,-1)
    | L -> (-1,0)
    | R -> (1,0)

let instructions =
    let input = readLines "input2_part1.txt"
    input
    |> Seq.map (fun line ->
        line |> Seq.map (function 'U' -> U | 'D' -> D | 'L' -> L | 'R' -> R))

let keypadCodes keypad initialCoord = 
    let keypadCoords = keypad |> List.map snd

    let move ((x,y) as cur) inst =
        let (x',y') = unitVector inst
        let cur' = x+x',y+y'
        if keypadCoords |> Seq.contains cur' then cur'
        else cur

    let nextKey initialCoord' keyInstructions =
        keyInstructions
        |> Seq.fold move initialCoord'

    instructions
    |> Seq.scan nextKey initialCoord
    |> Seq.skip 1
    |> Seq.map (fun coord -> keypad |> Seq.find (fun (_,coord') -> coord=coord') |> fst |> string)
    |> Seq.reduce (+)

let part1 =
    let keypad = [
        '7', (0,0)
        '8', (1,0)
        '9', (2,0)
        '4', (0,1)
        '5', (1,1)
        '6', (2,1)
        '1', (0,2)
        '2', (1,2)
        '3', (2,2) ]
    keypadCodes keypad (1,1)

let part2 =
    let keypad = [
        'D', (2,0) 
        'A', (1,1) 
        'B', (2,1) 
        'C', (3,1) 
        '5', (0,2) 
        '6', (1,2) 
        '7', (2,2) 
        '8', (3,2) 
        '9', (4,2) 
        '2', (1,3) 
        '3', (2,3) 
        '4', (3,3) 
        '1', (2,4) ]
    keypadCodes keypad (0,2)