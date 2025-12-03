module AoC2025.Day2

open System

let parse (input : string array)  =
    input[0]
    |> _.Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map _.Split("-")
    |> Seq.map (fun x -> [| int64(x[0]) .. int64(x[1]) |])
    
let isRepeatingTwice num =
    let c = string num
    let l = String.length c / 2
    c[0..l - 1] = c[l..]

let isRepeating num =
    let c = string num
    [| 2 .. String.length c |]
    |> Seq.filter (fun d -> String.length c % d = 0)
    |> Seq.exists (fun d ->
        c
        |> Seq.splitInto d
        |> Seq.distinct
        |> Seq.length
        |> (=) 1)
    
let solve input fn = 
    input
    |> parse
    |> Seq.concat
    |> Seq.filter fn
    |> Seq.sum
    |> string
    
let partOne input = solve input isRepeatingTwice
        
let partTwo input = solve input isRepeating