#load "Prelude.fsx"
open System
open System.Text.RegularExpressions

let parseLine line =
    let m = Regex.Match(line, "(.*)-(.*)\[(.*)\]")
    let room = m.Groups.[1].Value
    let sector = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value
    room,sector,checksum

let input = readLines "input4_part1.txt" |> Seq.map parseLine |> Seq.toList

let genChecksum room =
    room
    |> Seq.filter (fun c -> c <> '-')
    |> Seq.map string
    |> Seq.countBy id
    |> Seq.sortBy (fun (letter, cnt) -> -cnt, letter)
    |> Seq.map fst
    |> Seq.take 5
    |> String.concat "" 

let realRooms =
    input
    |> Seq.filter (fun (room, sector, checksum) -> genChecksum room = checksum)

let part1 =
    realRooms
    |> Seq.sumBy (fun (room, sector, checksum) ->
        let checksum' = genChecksum room
        if checksum' = checksum then sector
        else 0)

let decryptLetter (c:char) times =
    if c = '-' then ' '
    else ((((c|>int)+times) - 97) % 26 + 97)|>char;;

let decrypt room sector = 
    room 
    |> Seq.map (fun c -> decryptLetter c sector)
    |> Seq.map string
    |> String.concat ""

let part2 =
    realRooms
    |> Seq.pick (fun (room,sector,_) ->
        let room' = decrypt room sector
        if room'.Contains("northpole") then Some(sector)
        else None)