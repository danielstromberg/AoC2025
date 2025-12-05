open System.IO
open AoC2025

let input = File.ReadAllLines "./input/private/Day05.txt"

Day5.partOne input |> printfn "%s"
Day5.partTwo input |> printfn "%s"