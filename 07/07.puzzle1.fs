module Puzzle1
open System.Collections.Generic
open Shared
open Shared.Gate

let wires     = new Dictionary<Wire, Circuit<DataOrWire>>()
let feeds     = new Dictionary<Wire, Wire List>()
let evaluable = new Queue<Wire>()
let onWire    = new Dictionary<Wire, Data>()

let run() =
   for gate in gates do
      wires.[gate.Output] <- gate.Inputs

      let reqs = gate.Inputs |> Circuit.requires

      if reqs.Length = 0 then
         // which implies evaluable
         evaluable.Enqueue gate.Output

      for req in reqs do
         if not (feeds.ContainsKey req) then
            feeds.[req] <- new List<Wire>()
         feeds.[req].Add gate.Output

   while evaluable.Count <> 0 do
      let w = evaluable.Dequeue()

      let res =
         match Circuit.toEvaluable wires.[w] with
         | None -> failwith "Logic Error, haha yes."
         | Some e ->
            match e with
            | Direct a      -> a
            | Not    a      -> ~~~a
            | And    (a, b) -> a &&& b
            | Or     (a, b) -> a ||| b
            | LShift (a, b) -> a <<< int32 b
            | RShift (a, b) -> a >>> int32 b
      onWire.[w] <- res

      for fed in feeds.[w] do
         let newCircuit = {wires.[fed] with // shit }
