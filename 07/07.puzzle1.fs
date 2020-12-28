module Puzzle1
open System
open Shared
open Shared.Instructions

type Bindings = Map<Symbol, RValue>

let addInstructionToMap (map: Bindings) (i: Instruction) =
   map.Add(i.LValue, i.RValue)

let getSymbol (table: Bindings) =
   let rec internalGetSymbol (sym: Symbol) (seen: Symbol Set) =
      Console.WriteLine (Set.count seen)
      if Set.contains sym seen then
         failwith "AAAA"
      let inline getExp (e: Exp): Symbol =
         match e with
         | Val v -> v
         | Ref r -> internalGetSymbol r (Set.add sym seen)

      match table.TryFind sym with
      | None -> failwith "Could not get " + sym
      | Some rval ->
         match rval with
         | Init (a)    -> getExp a
         | And  (a, b) -> (getExp a) &&& (getExp b)
         | Or   (a, b) -> (getExp a) ||| (getExp b)
         | Not  (a)    -> ~~~(getExp a)
         | LShift (a, amount) -> (getExp a) <<< (int32 <| getExp amount)
         | RShift (a, amount) -> (getExp a) >>> (int32 <| getExp amount)
   internalGetSymbol

let run() =
   let map = instructions |> Array.fold addInstructionToMap Map.empty
   let wirea = "a" |> parseSymbol |> Option.get
   getSymbol map wirea Set.empty
   |> Console.WriteLine
