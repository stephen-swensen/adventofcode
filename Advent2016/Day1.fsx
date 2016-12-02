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

let rec walk movements ((cur_x, cur_y) as cur) orientation = seq {
    match movements with
    | [] -> yield cur 
    | movement::movements' ->
        
        let m, (vx,vy), orientation' =
            match movement with
            | R m -> 
                match orientation with
                | N -> m, (1, 0), E 
                | E -> m, (0, -1), S 
                | S -> m, (-1, 0), W 
                | W -> m, (0, 1), N 
            | L m -> 
                match orientation with
                | N -> m, (-1, 0), W 
                | W -> m, (0, -1), S 
                | S -> m, (1, 0), E 
                | E -> m, (0, 1), N 
        
        //expand steps between cur and destination
        let steps = Seq.scan (fun (x',y') _ -> x'+vx, y'+vy) cur [1..m]
        yield! steps |> Seq.take m // yield all but the last element
        yield! walk movements' (steps |> Seq.last) orientation' }

let part1 = 
    let (x,y) = walk movements (0,0) N |> Seq.last
    Math.Abs(x) + Math.Abs(y)

let part2 =
    let (x,y) = 
        let locations' =
            let locations = walk movements (0,0) N |> Seq.toList
            let rec walk' (cur::locations') visited = seq {
                if visited |> Seq.contains cur then yield! (cur::visited)
                else yield! walk' locations' (cur::visited) }
            walk' locations [] |> Seq.rev
        printfn "locations: %A" locations'
        locations' |> Seq.last
    Math.Abs(x) + Math.Abs(y)