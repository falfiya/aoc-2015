module Puzzle2
open Shared

let map: int [,] = Array2D.zeroCreate 1000 1000

let raise = (+) 1
let lower ll = if ll > 0 then ll - 1 else 0

for inst in instructions do
   let lightMapping =
      match inst.Code with
      | Op.Code.On     -> raise
      | Op.Code.Off    -> lower
      | Op.Code.Toggle -> raise >> raise
   let (sX, sY) = inst.Start
   let (fX, fY) = inst.Finish
   map.[sX..fX, sY..fY] <- (map.[sX..fX, sY..fY] |> Array2D.map lightMapping)

let run() =
   map
   |> Array2D.flat
   |> Array.sum
   |> printf "Puzzle2: The light level is at %d\n"
