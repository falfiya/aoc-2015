module Shared
open System

module Array =
   let fillet pred = pred |> Array.filter >> Array.length

module Array2D =
   // http://www.fssnip.net/oq/title/Array2D-to-one-dimension
   let flat matrix =
      let maxX = Array2D.length1 matrix - 1
      let maxY = Array2D.length2 matrix - 1
      [|for x in 0..maxX do
         for y in 0..maxY do
            yield matrix.[x, y]|]

module Op =
   type Code = On | Off | Toggle

   type Op = { Code: Code; Start: (int * int); Finish: (int * int) }

   let locFromString (s: string) =
      match s.Split(",") with
      | [| x; y |] -> (int x, int y)
      | _ -> raise(Exception("Invalid loc"))

   let create code start finish = {
      Op.Code = code
      Start   = locFromString start
      Finish  = locFromString finish
   }

   let fromString (s: string) =
      match s.Split(" ") with
      | [| _; start; _; finish |]        -> create Toggle start finish
      | [| _; "on"; start; _; finish |]  -> create On     start finish
      | [| _; "off"; start; _; finish |] -> create Off    start finish
      | _ -> raise(Exception("Invalid instruction"))

let filename = __SOURCE_DIRECTORY__ + "/instructions.txt"
let instructions =
   filename
   |> IO.File.ReadAllLines
   |> Array.map Op.fromString
