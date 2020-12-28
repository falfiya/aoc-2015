module Shared
open System

module Instructions =
   type Symbol = uint16

   type Exp =
      | Byte   of byte
      | Ref    of Symbol
      | Direct of Exp
      | And    of Exp * Exp
      | Or     of Exp * Exp
      | Not    of Exp
      | LShift of Exp * amount: Exp
      | RShift of Exp * amount: Exp

   let parseByte (s: string) =
      try
         Some <| byte s
      with _ ->
         None

   let parseSymbol (s: string): Symbol Option =
      let chars = s |> Seq.map uint16 |> Seq.toArray
      match chars.Length with
      | 1 -> Some (chars.[0] <<< 8)
      | 2 -> Some (chars.[0] <<< 8 ||| chars.[1])
      | _ -> None

   exception ExpTokenException of string

   let Exp (s: string) =
      match parseByte s with
      | Some byte -> Byte byte
      | None ->
         match parseSymbol s with
         | Some symbol -> Ref symbol
         | None -> failwith <| ("No matches found for " + s)

   type Instruction = {
      LValue: Symbol;
      RValue: Exp;
   }

   exception InvalidInstructionException of string

   let fromString (s: string) =
      let revWords = s.Split " " |> Array.rev

      if revWords.Length < 3 then
         raise <| InvalidInstructionException
            "There must be at least 3 words per instruction"

      let symbol =
         match parseSymbol revWords.[0] with
         | Some symbol -> symbol
         | None -> raise <| InvalidInstructionException ("Invalid symbol: " + revWords.[0])

      let exp =
         match revWords.[2..] with
         | [| a |]              -> Direct (Exp a)        // a
         | [| a; "NOT" |]       -> Not    (Exp a)        // NOT a
         | [| b; "AND"; a |]    -> And    (Exp a, Exp b) // a AND b
         | [| b; "OR" ; a |]    -> Or     (Exp a, Exp b) // a OR  b
         | [| b; "LSHIFT"; a |] -> LShift (Exp a, Exp b) // a LSHIFT b
         | [| b; "RSHIFT"; a |] -> RShift (Exp a, Exp b) // a RSHIFT b
         | _ -> raise <| InvalidInstructionException
                  "Unknown instruction type"

      { LValue = symbol; RValue = exp }

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"

let instructions =
   filename
   |> IO.File.ReadAllLines
   |> Array.map Instructions.fromString
