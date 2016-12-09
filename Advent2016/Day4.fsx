#load "Prelude.fsx"
open System
open System.Text.RegularExpressions

let parseLine line =
    let m = Regex.Match(line, "(.*)-(.*)\[(.*)\]")
    let room = m.Groups.[1].Value
    let sector = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value
    room,sector,checksum

let input = readLines "input4_part1.txt" |> Seq.map parseLine

let genChecksum room =
    room
    |> Seq.filter (fun c -> c <> '-')
    |> Seq.map string
    |> Seq.countBy id
    |> Seq.sortBy (fun (letter, cnt) -> -cnt, letter)
    |> Seq.map fst
    |> Seq.take 5
    |> String.concat "" 

let part1 =
    input
    |> Seq.sumBy (fun (room, sector, checksum) ->
        let checksum' = genChecksum room
        if checksum' = checksum then sector
        else 0)