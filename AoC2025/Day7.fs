module AoC2025.Day7

let parse (input : string array) =
    let map =
        input
        |> Array.map _.ToCharArray()
        |> Array.indexed
        |> Array.collect (fun (y, c) ->
            c
            |> Array.indexed
            |> Array.map (fun (x, ch) -> ((x, y), ch)))
        |> Map.ofSeq 
    
    let beams =
        map
        |> Map.findKey (fun _k v -> v = 'S')
        |> Set.empty.Add
        
    map, beams

let partOne input =
    input
    |> parse
    |> Seq.unfold (fun (map, beams) ->
        let beamsAndSplits =
            beams
            |> Seq.map (fun (x, y) ->
                match Map.tryFind (x, y + 1) map with
                    | Some '^' -> ([(x - 1, y + 1); (x + 1, y + 1)], 1)
                    | Some _ -> ([(x, y + 1)], 0)
                    | None -> ([], 0))
        
        let newBeams = beamsAndSplits |> Seq.map fst |> Seq.concat |> Set.ofSeq
        let splits = beamsAndSplits |> Seq.sumBy snd

        match Set.count newBeams with
            | 0 -> None
            | _ -> Some(splits, (map, newBeams)))
    |> Seq.sum
    |> string

let partTwo input =
    input
    |> parse
    |> (fun (a, b) -> (a, Set.toSeq b |> Seq.map (fun x -> (x, 1L))))
    |> Seq.unfold (fun (map, beamsByCount) ->
        let newBeamsByCount =
            beamsByCount
            |> Seq.collect (fun ((x, y), c) ->
                match Map.tryFind (x, y + 1) map with
                    | Some '^' -> [((x - 1, y + 1), c); ((x + 1, y + 1), c)]
                    | Some _ -> [((x, y + 1), c)]
                    | None -> [])
            |> Seq.groupBy fst
            |> Seq.map (fun ((x, y), b) -> ((x, y), Seq.sumBy snd b))
            
        match Seq.length newBeamsByCount with
            | 0 -> None
            | _ -> Some(Seq.sumBy snd newBeamsByCount, (map, newBeamsByCount)))
    |> Seq.last
    |> string