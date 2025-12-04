open System.IO
open AoC2025

let input = File.ReadAllLines "./input/private/Day04.txt"

Day4.partOne input |> printfn "%s"
Day4.partTwo input |> printfn "%s"