open System.IO
open AoC2025

let input = File.ReadAllLines "./input/private/Day03.txt"

Day3.partOne input |> printfn "%s"
Day3.partTwo input |> printfn "%s"