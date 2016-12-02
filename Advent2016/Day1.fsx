#load "Prelude.fsx"
open System

///Directions for a movement: Right or Left a number of steps
type Movement = R of int | L of int with
    member this.Magnitude = 
        match this with
        | R m | L m -> m

///Orientation in the vector space: North, South, East, West
type Orientation = N | S | E | W with
    member this.UnitDirection = 
        match this with
        | N -> (1, 0)
        | E -> (0, -1)
        | S -> (-1, 0)
        | W -> (0, 1)
    member this.Rotate(movement) =
        match movement with
        | R _ -> 
            match this with
            | N -> E 
            | E -> S 
            | S -> W 
            | W -> N 
        | L _ -> 
            match this with
            | N -> W 
            | W -> S 
            | S -> E 
            | E -> N 
        
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
    let rec walk (movements:Movement list) ((cur_x, cur_y) as cur) (orientation:Orientation) = seq {
        match movements with
        | [] -> yield cur 
        | movement::movements' ->
            let m = movement.Magnitude
            let (vx,vy) = orientation.UnitDirection
            //expand steps between cur and destination
            let steps = Seq.scan (fun (x',y') _ -> x'+vx, y'+vy) cur [1..m]
            yield! steps |> Seq.take m // yield all but the last element
            yield! walk movements' (steps |> Seq.last) (orientation.Rotate(movement)) }
    walk movements (0,0) N

let part1 = 
    let (x,y) = walk movements |> Seq.last
    Math.Abs(x) + Math.Abs(y)

let part2 =
    let (x,y) = 
        //Yield all coordinates up to and including the first point already visited
        let locations' =
            let locations = walk movements |> Seq.toList
            let rec walk' (cur::locations') visited = seq {
                if visited |> Seq.contains cur then yield! (cur::visited)
                else yield! walk' locations' (cur::visited) }
            walk' locations [] |> Seq.rev
        locations' |> Seq.last
    Math.Abs(x) + Math.Abs(y)