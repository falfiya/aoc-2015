module Puzzle1
open Shared
open Shared.Instructions

type Bindings = Map<Symbol, RValue>

let addInstructionToMap (map: Bindings) (i: Instruction) =
   map.Add(i.LValue, i.RValue)

let getSymbol (table: Bindings) =
   let rec internalGetSymbol (sym: Symbol) =
      let inline getExp (e: Exp): uint16 =
         match e with
         | Val v -> v
         | Ref r -> internalGetSymbol r

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

open System
let run() =
   let map = instructions |> Array.fold addInstructionToMap Map.empty
   let wirea = "a" |> parseSymbol |> Option.get
   getSymbol map wirea
   |> Console.WriteLine
