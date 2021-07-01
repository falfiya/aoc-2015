module Puzzle1
open System.Collections.Generic
open Shared
open Shared.Gate

let wires     = new Dictionary<Wire, Circuit>()
// what is upstream from the wire
let wants  = new Dictionary<Wire, Wire HashSet>()
// what is downstream from the wire
let gives     = new Dictionary<Wire, Wire List>()
let evaluable = new Queue<Wire>()
let onWire    = new Dictionary<Wire, Data>()

let v (a: DataOrWire) =
   match a with
   | Data data -> data
   | Wire wire -> onWire.[wire]

let run() =
   for gate in gates do
      let w = gate.Output
      wires.[w] <- gate.Inputs

      let reqs = gate.Inputs |> Circuit.requires
      wants.[w] <- new HashSet<Wire>(reqs)

      if wants.[w].Count = 0 then
         // which implies evaluable
         evaluable.Enqueue w

      for want in reqs do
         if not (gives.ContainsKey want) then
            gives.[want] <- new List<Wire>()
         gives.[want].Add w

   while evaluable.Count <> 0 do
      let w = evaluable.Dequeue()

      let res =
         match wires.[w] with
         | Direct a      -> v a
         | Not    a      -> ~~~(v a)
         | And    (a, b) -> (v a) &&& (v b)
         | Or     (a, b) -> (v a) ||| (v b)
         | LShift (a, b) -> (v a) <<< int32 (v b)
         | RShift (a, b) -> (v a) >>> int32 (v b)
      onWire.[w] <- res

      if gives.ContainsKey w then
         for fed in gives.[w] do
            ignore ^| wants.[fed].Remove w
            if wants.[fed].Count = 0 then
               evaluable.Enqueue fed

   for e in onWire do
      printfn "%i is on wire %s" e.Value e.Key
