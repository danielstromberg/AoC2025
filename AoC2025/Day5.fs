module AoC2025.Day5

let parse (input : string array) =
    let delim = Seq.findIndex (fun x -> x = "") input
    
    let ranges =
        input[0 .. delim - 1]
        |> Seq.map _.Split("-")
        |> Seq.map (fun x -> (int64(x[0]), int64(x[1])))
        
    let ids =
        input[delim + 1 ..]
        |> Seq.map int64
        
    ranges, ids
    
let partOne input =
    let ranges, ids = parse input
    
    ids
    |> Seq.filter (fun id -> Seq.exists (fun (s, e) -> id >= s && id <= e) ranges)
    |> Seq.length
    |> string

let partTwo input =
    let overlapsOrAdjacent (s1, e1) (s2, e2) = s1 <= e2 + 1L && e1 + 1L >= s2
    
    input
    |> parse
    |> fst
    |> Seq.sortBy fst
    |> Seq.fold (fun acc (s1, e1) ->
        match Seq.tryFindIndex (overlapsOrAdjacent (s1, e1)) acc with
        | Some(index) ->
            let s2, e2 = List.item index acc
            List.updateAt index (min s1 s2, max e1 e2) acc
        | None -> (s1, e1) :: acc) []
    |> Seq.sumBy (fun (a, b) -> b - a + 1L)
    |> string