module Shared
open System

module Instructions =
   type Symbol = uint16

   // for my sake, an expression is either a reference or a value
   type Exp =
   | Val    of uint
   | Ref    of Symbol

   type RValue =
   | Init   of Exp
   | And    of Exp * Exp
   | Or     of Exp * Exp
   | Not    of Exp
   | LShift of Exp * amount: Exp
   | RShift of Exp * amount: Exp

   let parseUint (s: string) =
      try
         Some <| uint s
      with _ ->
         None

   let parseSymbol (s: string): Symbol Option =
      let chars = s |> Seq.map uint16 |> Seq.toArray
      match chars.Length with
      | 1 -> Some (chars.[0] <<< 8)
      | 2 -> Some (chars.[0] <<< 8 ||| chars.[1])
      | _ -> None

   let Exp (s: string) =
      match parseUint s with
      | Some thing -> Val thing
      | None ->
         match parseSymbol s with
         | Some symbol -> Ref symbol
         | None -> failwith ("No matches found for " + s)

   type Instruction = {
      LValue: Symbol;
      RValue: RValue;
   }

   let fromString (s: string) =
      let revWords = s.Split " " |> Array.rev

      if revWords.Length < 3 then
         failwith "There must be at least 3 words per instruction"

      let symbol =
         match parseSymbol revWords.[0] with
         | Some symbol -> symbol
         | None -> failwith ("Invalid symbol: " + revWords.[0])

      let rhs =
         match revWords.[2..] with
         | [| a |]              -> Init   (Exp a)        // a
         | [| a; "NOT" |]       -> Not    (Exp a)        // NOT a
         | [| b; "AND"; a |]    -> And    (Exp a, Exp b) // a AND b
         | [| b; "OR" ; a |]    -> Or     (Exp a, Exp b) // a OR  b
         | [| b; "LSHIFT"; a |] -> LShift (Exp a, Exp b) // a LSHIFT b
         | [| b; "RSHIFT"; a |] -> RShift (Exp a, Exp b) // a RSHIFT b
         | _ -> failwith "Unknown instruction type"

      { LValue = symbol; RValue = rhs }

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"

let instructions =
   filename
   |> IO.File.ReadAllLines
   |> Array.map Instructions.fromString
