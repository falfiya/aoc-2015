module rec Shared
open System.IO

let inline (^|) a b = a b

module Util =
   let parse parseFn (s: string) =
      try Some (parseFn s)
      with _ -> None

module Option =
   let unwrap (msg: string) (o: 'a Option) =
      match o with
      | Some a -> a
      | None -> failwith msg

[<Struct>]
type Gate<'a> = {Inputs: Gate.Circuit<'a>; Output: Gate.Wire}
module Gate =
   type Data = uint32
   module Data =
      let fromString = Util.parse Data.Parse

   type Wire = string

   type DataOrWire =
   | Data of Data
   | Wire of Wire
   module DataOrWire =
      let fromString (s: string): DataOrWire =
         match Data.fromString s with
         | Some data -> Data data
         | None -> Wire s

      let getWireType (i: DataOrWire) =
         match i with
         | Wire w -> Some w
         | Data _ -> None

   type Circuit<'a> =
   | Direct of 'a
   | Not    of 'a
   | And    of 'a * 'a
   | Or     of 'a * 'a
   | LShift of 'a * 'a
   | RShift of 'a * 'a
   module Circuit =
      let inputs (c: Circuit<DataOrWire>) =
         match c with
         | Direct a      -> [|a|]
         | Not    a      -> [|a|]
         | And    (a, b) -> [|a; b|]
         | Or     (a, b) -> [|a; b|]
         | LShift (a, b) -> [|a; b|]
         | RShift (a, b) -> [|a; b|]

      let requires (c: Circuit<DataOrWire>) =
         c |> inputs |> Array.map DataOrWire.getWireType |> Array.choose id

      let toEvaluable (c: Circuit<DataOrWire>) =
         match c with
         | Direct (Data a)         -> Some ^| Direct (a)
         | Not    (Data a)         -> Some ^| Not    (a)
         | And    (Data a, Data b) -> Some ^| And    (a, b)
         | Or     (Data a, Data b) -> Some ^| Or     (a, b)
         | LShift (Data a, Data b) -> Some ^| LShift (a, b)
         | RShift (Data a, Data b) -> Some ^| RShift (a, b)
         | _ -> None


   let i = DataOrWire.fromString
   let fromString (s: string): Gate<DataOrWire> =
      let words = s.Split " "
      let last  = words.Length - 1
      let circuit =
         match words.[..last - 2] with
         | [|a|]              -> Direct (i a)
         | [|"NOT"; a|]       -> Not    (i a)
         | [|a; "AND"; b|]    -> And    (i a, i b)
         | [|a; "OR" ; b|]    -> Or     (i a, i b)
         | [|a; "LSHIFT"; b|] -> LShift (i a, i b)
         | [|a; "RSHIFT"; b|] -> RShift (i a, i b)
         | _ -> failwith "Unknown gate type!"

      let output = words.[last]

      {Inputs = circuit; Output = output}

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"

let gates =
   filename
   |> File.ReadAllLines
   |> Array.map Gate.fromString
