module Shared
open System

module Op =
   type OpCode = On | Off | Toggle

   type Op = { Code: OpCode; Start: (int * int); Finish: (int * int) }

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
