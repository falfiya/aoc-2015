module Puzzle1
open Shared

let map: bool [,] = Array2D.zeroCreate 1000 1000

let True _ = true

let turnOn (sX, sY) (fX, fY) =
   map.[sX..fX, sY..fY] <- (map.[sX..fX, sY..fY] |> Array2D.map True)

let False _ = false

let turnOff (sX, sY) (fX, fY) =
   map.[sX..fX, sY..fY] <- (map.[sX..fX, sY..fY] |> Array2D.map False)

let XOR = (<>) true

let toggle (sX, sY) (fX, fY) =
   map.[sX..fX, sY..fY] <- (map.[sX..fX, sY..fY] |> Array2D.map XOR)

for inst in instructions do
   let fn =
      match inst.Code with
      | Op.Code.On     -> turnOn
      | Op.Code.Off    -> turnOff
      | Op.Code.Toggle -> toggle
   fn inst.Start inst.Finish

let run() =
   map
   |> Array2D.flat
   |> Array.fillet id
   |> printf "Puzzle1: There are %d lights turned on \n"
