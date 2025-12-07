open System.IO
open AoC2025

let input = File.ReadAllLines "./input/private/Day07.txt"

Day7.partOne input |> printfn "%s"
Day7.partTwo input |> printfn "%s"