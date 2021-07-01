module Puzzle2
open System.IO

let escapeCounter count (c: char) =
   match c with
   | '\\' -> count + 2
   | '"' -> count + 2
   | _ -> count + 1

let escapedStringLength = Seq.fold escapeCounter 2

let score (s: string) = escapedStringLength s - s.Length

let run () =
   let lines = File.ReadAllLines "escape.txt"
   let totalScore = Array.sumBy score lines
   printfn "Puzzle1 Answer: %d" totalScore
