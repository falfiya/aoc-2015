module Puzzle1
open System.IO

// this state machine is amazing
type Mode =
   | Normal
   | String
   | Escaped
   | ExpectingHex1
   | ExpectingHex2

[<Struct>]
type CounterState = {mode: Mode; count: int}

let inline inRange bottom top c = (bottom <= c) && (c <= top)
let inline isHex (c: char) =
   (inRange '0' '9' c) || (inRange 'a' 'f' c) || (inRange 'A' 'F' c)

let counter (s: CounterState) (c: char) =
   match s.mode with
   | Normal ->
      if c = '"' then
         {s with mode = String}
      else
         failwithf "Expected \" but found %c!" c
   | String ->
      if c = '\\' then
         {s with mode = Escaped}
      elif c = '"' then
         {s with mode = Normal}
      else
         {s with count = s.count + 1}
   | Escaped ->
      if c = '\\' then
         {mode = String; count = s.count + 1}
      elif c = '"' then
         {mode = String; count = s.count + 1}
      elif c = 'x' then
         {s with mode = ExpectingHex1}
      else
         failwithf "Unexpected escape %c!" c
   | ExpectingHex1 ->
      if isHex c then
         {s with mode = ExpectingHex2}
      else
         failwithf "Expected hex1 character but found %c!" c
   | ExpectingHex2 ->
      if isHex c then
         {mode = String; count = s.count + 1}
      else
         failwithf "Expected hex2 character but found %c!" c

let bytesInMemory = Seq.fold counter {mode = Normal; count = 0} >> fun s ->
   match s with
   | {mode = Normal; count = count} -> count
   | _ -> failwithf "Invalid counting state '%A'" s.mode

let score (s: string) = s.Length - bytesInMemory s

let run () =
   let lines = File.ReadAllLines "escape.txt"
   let totalScore = Array.sumBy score lines
   printfn "Puzzle1 Answer: %d" totalScore
