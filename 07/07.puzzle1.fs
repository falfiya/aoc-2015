module Puzzle1
open System
open Shared
open Shared.Instructions

type Bindings = Map<Symbol, RValue>

let addInstructionToMap (map: Bindings) (i: Instruction) =
   map.Add(i.LValue, i.RValue)

let getSymbol (table: Bindings) =
   let mutable workingBindings = table
   let rec internalGetSymbol (i: int) (sym: Symbol) =
      Console.WriteLine i |> ignore
      let inline getExp (e: Exp): Symbol =
         match e with
         | Val v -> v
         | Ref r ->
            let res = internalGetSymbol (i + 1) r
            workingBindings = workingBindings.Remove(sym).Add(sym, Init(Val(res)))
               |> ignore
            res

      match workingBindings.TryFind sym with
      | None -> failwith "Could not get " + sym
      | Some rval ->
         match rval with
         | Init (a)    -> getExp a
         | And  (a, b) -> (getExp a) &&& (getExp b)
         | Or   (a, b) -> (getExp a) ||| (getExp b)
         | Not  (a)    -> ~~~(getExp a)
         | LShift (a, amount) -> (getExp a) <<< (int32 <| getExp amount)
         | RShift (a, amount) -> (getExp a) >>> (int32 <| getExp amount)
   internalGetSymbol 1

let run() =
   let map = instructions |> Array.fold addInstructionToMap Map.empty
   let wirea = "a" |> parseSymbol |> Option.get
   getSymbol map wirea
   |> Console.WriteLine
