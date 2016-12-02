[<AutoOpen>]
module Prelude

open System
open System.IO

do System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let readText path =
    File.ReadAllText path

let readLines path =
    File.ReadAllLines path

let split (delimiter:string) (input:string) =
    let result = input.Split([|delimiter|], StringSplitOptions.None)
    if result = [|input|] then Seq.empty //nothing was split
    else upcast result

///Try parse string to int
let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

///Try parse char array to int
let (|Int'|_|) chars =
   chars |> Seq.toArray |> String |> (|Int|_|)