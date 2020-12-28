module Shared

module Instruction =
   type LValue = uint16
   exception InvalidSymbolException of string

   let stringToSymbol (s: string): LValue =
      let chars = s |> Seq.map uint16 |> Seq.toArray
      match chars.Length with
      | 1 -> chars.[0] <<< 8
      | 2 -> chars.[0] <<< 8 ||| chars.[1]
      | _ -> raise
               <| InvalidSymbolException
                  "String must be either 1 or 2 bytes!"

   type RValue =
      | Direct of byte
      | And    of LValue * LValue
      | Or     of LValue * LValue
      | Not    of LValue
      | Lshift of LValue * amount: byte
      | Rshift of LValue * amount: byte

   type Instruction = {
      LValue: LValue;
      RValue: RValue;
   }

   let make lvalue rvalue = { LValue = lvalue; RValue = rvalue }

   exception InvalidInstructionException of string
   let fromString (s: string) =
      let revWords = s.Split " " |> Array.rev

      if revWords.Length < 3 then
         raise
            <| InvalidInstructionException
               "There must be at least 3 words per instruction!"

      let make = revWords.[0] |> stringToSymbol |> make

      match revWords.[2..] with
      | [| state |]       -> state |> byte |> Direct |> make
      | [| a; "AND"; b |] -> (a, b) |> And |> make

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"
