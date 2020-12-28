module Shared
open System

module Instructions =
   type LValue = string

   type RValue =
      | Direct of byte
      | And    of LValue * LValue
      | Or     of LValue * LValue
      | Not    of LValue
      | LShift of LValue * amount: byte
      | RShift of LValue * amount: byte

   type Instruction = {
      LValue: LValue;
      RValue: RValue;
   }

   let bite s =
      Console.WriteLine ("bite " + s)
      byte s

   exception InvalidInstructionException of string

   let fromString (s: string) =
      let revWords = s.Split " " |> Array.rev

      if revWords.Length < 3 then
         raise <| InvalidInstructionException
            "There must be at least 3 words per instruction"

      let lvalue = revWords.[0]

      let rvalue =
         match revWords.[2..] with
         | [| state |] -> Direct <| bite state
         | [| a; "NOT" |] -> Not a
         | [| b; "AND"; a |] -> And (a, b)
         | [| b; "OR" ; a |] -> Or  (a, b)
         | [| amount; "LSHIFT"; lvalue |] -> LShift (lvalue, byte amount)
         | [| amount; "RSHIFT"; lvalue |] -> RShift (lvalue, byte amount)
         | _ -> raise <| InvalidInstructionException
                  "Unknown instruction type"

      { LValue = lvalue; RValue = rvalue }

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"

let instructions =
   filename
   |> IO.File.ReadAllLines
   |> Array.map Instructions.fromString
