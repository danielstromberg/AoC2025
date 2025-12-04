module AoC2025.Day4

let parse (input : string array) =
    input
    |> Array.map _.ToCharArray()
    |> Array.indexed
    |> Array.map (fun (y, c) ->
        c
        |> Array.indexed
        |> Array.map (fun (x, ch) -> (x, y, ch)))
    |> Array.concat
    |> Array.filter (fun (_, _, c) -> c = '@')
    |> Array.map (fun (x, y, _) -> (x, y))
    |> Set.ofArray

let canRemove map (x, y) =
    ([| x - 1 .. x + 1 |], [| y - 1 .. y + 1 |])
    ||> Seq.allPairs
    |> Seq.except [(x, y)]
    |> Seq.filter (fun p -> Set.contains p map)
    |> Seq.length < 4

let removeRolls map =
    let toRemove = map |> Set.filter (canRemove map)
    Set.difference map toRemove, Set.count toRemove
    
let partOne input =
    let map = parse input
    
    map
    |> Seq.filter (canRemove map)
    |> Seq.length
    |> string

let partTwo input =
    input
    |> parse
    |> Seq.unfold (removeRolls >> function
        | _, 0 -> None
        | newRolls, count -> Some(count, newRolls))
    |> Seq.sum
    |> string