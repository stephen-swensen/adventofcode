#load "Prelude.fsx"
open System

///Directions for a movement: Right or Left a number of steps
type Movement = R of int | L of int

///Orientation in the vector space: North, South, East, West
type Orientation = N | E | S | W
let unitVector = function | N -> (1,0) | E -> (0,-1) | S -> (-1,0) | W -> (0,1)
let rotate orientation movement =
    match movement with
    | R _ -> [(N,E);(E,S);(S,W);(W,N)]  
    | L _ -> [(N,W);(W,S);(S,E);(E,N)]
    |> Seq.find (fun (x,_) -> x = orientation)
    |> snd

///Parse the input into a list of Movements
let movements = 
    let input = readText "input1_part1.txt"
    input 
    |> split ", "
    |> Seq.map (fun movement ->
        match movement |> Seq.toList with
        | 'R'::Int'(m) -> R m
        | 'L'::Int'(m) -> L m)
    |> Seq.toList

///Starting at origin (0,0) and facing North, follow the Movement directions, yielding the coordinates of every step
let walk movements =
    let rec walk movements cur orientation = seq {
        match movements with
        | [] -> yield cur 
        | movement::movements' ->
            let (R m | L m) = movement
            let orientation' = rotate orientation movement
            let (x,y) = unitVector orientation'
            //expand steps between cur and destination
            let steps = Seq.scan (fun (x',y') _ -> x'+x, y'+y) cur [1..m]
            yield! steps |> Seq.take m // yield all but the last element
            yield! walk movements' (steps |> Seq.last) orientation' }
    walk movements (0,0) N

let part1 = 
    let (x,y) = walk movements |> Seq.last
    abs x + abs y

let part2 =
    let (x,y) = 
        let locations = walk movements |> Seq.toList
        //Yield all coordinates up to and including the first point already visited
        let locations' =
            let rec walk' (cur::locations') visited = seq {
                if visited |> Seq.contains cur then yield! (cur::visited)
                else yield! walk' locations' (cur::visited) }
            walk' locations [] |> Seq.rev
        locations' |> Seq.last
    abs x + abs y