module AoC2025.Day6

open System

type Problem =
    | Mult of int64 seq
    | Add of int64 seq

let parseVertical (input : string array) =
    input
    |> Seq.rev
    |> Seq.map _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.transpose
    |> Seq.map (fun l ->
        match Seq.head l with
        | "*" -> Mult (l |> Seq.tail |> Seq.map int64)
        | "+" -> Add (l |> Seq.tail |> Seq.map int64)
        | _ -> failwith "todo")

let parseColumns (input : string array) =
    let operators =
        input
        |> Seq.last
        |> _.ToCharArray()
        |> Seq.indexed
        |> Seq.filter (fun (_, ch) -> ch <> ' ')
    
    input[0 .. Seq.length input - 2]
    |> Seq.map _.ToCharArray()
    |> Seq.transpose
    |> Seq.indexed
    |> Seq.map (fun (index, chars) -> (index, Seq.filter (fun c -> c <> ' ') chars))
    |> Seq.filter (fun (_, chars) -> Seq.length chars <> 0)
    |> Seq.map (fun (index, chars) -> (index, chars |> Seq.toArray |> String |> int64))
    |> Seq.map (fun (index, num) -> (operators |> Seq.findBack (fun (a, _) -> a <= index), num))
    |> Seq.groupBy fst
    |> Seq.map (fun ((_, ch), l) -> (ch, Seq.map snd l))
    |> Seq.map (fun (ch, l) ->
        match ch with
        | '*' -> Mult l
        | '+' -> Add l
        | _ -> failwith "todo")

let solve input parseFun =
    input
    |> parseFun
    |> Seq.sumBy (fun p ->
        match p with
        | Mult l -> Seq.reduce (*) l
        | Add l -> Seq.sum l)
    |> string

let partOne input = solve input parseVertical

let partTwo input = solve input parseColumns