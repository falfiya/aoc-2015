module Puzzle1
open Shared

let map: bool [,] = Array2D.zeroCreate 1000 1000

let True _ = true
let False _ = false
let XOR = (<>) true

for inst in instructions do
   let lightMapping =
      match inst.Code with
      | Op.Code.On     -> True
      | Op.Code.Off    -> False
      | Op.Code.Toggle -> XOR
   let (sX, sY) = inst.Start
   let (fX, fY) = inst.Finish
   map.[sX..fX, sY..fY] <- (map.[sX..fX, sY..fY] |> Array2D.map lightMapping)

let run() =
   map
   |> Array2D.flat
   |> Array.fillet id
   |> printf "Puzzle1: There are %d lights turned on \n"
