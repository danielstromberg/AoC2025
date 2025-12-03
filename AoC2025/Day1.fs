module AoC2025.Day1

open System

type Rotation =
    | Left of int
    | Right of int

let rem n m = ((n % m) + m) % m

let parse (input : string array)  =
    input
    |> Seq.map (fun s ->
        let r = Convert.ToInt32(s.Substring 1)
        match s with
        | s when s.StartsWith "L" -> Left r
        | _ -> Right r)
    
let partOne input =
    input
    |> parse
    |> Seq.scan (fun acc rotation ->
        match rotation with
        | Left r -> rem (acc - r) 100
        | Right r -> rem (acc + r) 100) 50
    |> Seq.where (fun n -> n = 0)
    |> Seq.length
    |> _.ToString()
    
let partTwo input =
    input
    |> parse
    |> Seq.fold (fun (angle, count) rotation -> 
        match rotation with
        | Left r -> (rem (angle - r) 100, count + (r + 100 - angle) / 100 - (100 - angle) / 100)
        | Right r -> (rem (angle + r) 100, count + (angle + r) / 100)) (50, 0)
    |> snd
    |> _.ToString()