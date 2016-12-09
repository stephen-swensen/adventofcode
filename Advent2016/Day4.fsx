#load "Prelude.fsx"
open System
open System.Text.RegularExpressions

let room = "aaaaa-bbb-zyx" 
let line = "aaaaa-bbb-z-y-x-123[abxyz]"

let parseLine line =
    let m = Regex.Match(line, "(.*)-(.*)\[(.*)\]")
    let room = m.Groups.[1].Value
    let sector = m.Groups.[2].Value |> int
    let checksum = m.Groups.[3].Value
    room,sector,checksum

let genChecksum room =
    room
    |> Seq.filter (fun c -> c <> '-')
    |> Seq.map string
    |> Seq.countBy id
    |> Seq.sortBy (fun (letter, cnt) -> -cnt, letter)
    |> Seq.map fst
    |> Seq.take 5
    |> String.concat "" 

genChecksum "not-a-real-room"