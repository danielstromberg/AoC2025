module AoC2025.Day3

open System.Globalization

let parse (input : string array) = input |> Seq.map (Seq.map CharUnicodeInfo.GetDigitValue >> Array.ofSeq) 

let jolt (bank : int array) =
    let index, left =
        bank[0..Array.length bank - 2]
        |> Seq.indexed
        |> Seq.maxBy snd
        
    let right = Seq.max bank[index + 1..]
    int64(left * 10 + right)

let findBattery (bank : int array) slots =
    let index, batteryValue =
        bank[0..Array.length bank - slots - 1]
        |> Seq.indexed
        |> Seq.maxBy snd
    
    bank[index + 1..], batteryValue

let joltDozen (bank : int array) =
    (([], bank), [| 11 .. -1 .. 0 |])
    ||> Seq.fold (fun (acc, bankLeft) slot ->
        let newBank, batteryValue = findBattery bankLeft slot
        batteryValue :: acc, newBank)
    |> fst
    |> Seq.indexed
    |> Seq.sumBy (fun (index, value) -> int64(10.0 ** index) * int64(value))

let solve input joltFun =
    input
    |> parse
    |> Seq.sumBy joltFun
    |> string

let partOne input = solve input jolt

let partTwo input = solve input joltDozen