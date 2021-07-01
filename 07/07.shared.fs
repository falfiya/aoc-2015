module rec Shared
open System.IO


module Util =
   let parse parseFn (s: string) =
      try Some (parseFn s)
      with _ -> None

module Option =
   let unwrap (msg: string) (o: 'a Option) =
      match o with
      | Some a -> a
      | None -> failwith msg

module Gate =
   type DataType = uint16
   module DataType =
      let fromString = Util.parse DataType.Parse

   type WireType = uint16
   module WireType =
      let fromString (s: string): WireType Option =
         let chars = s |> Seq.map uint16 |> Seq.toArray
         match chars.Length with
         | 1 -> Some (chars.[0] <<< 8)
         | 2 -> Some (chars.[0] <<< 8 ||| chars.[1])
         | _ -> None

   type In =
   | Data of DataType
   | Wire of WireType
   module In =
      let fromString (s: string): In =
         match DataType.fromString s with
         | Some data -> Data data
         | None ->
            match WireType.fromString s with
            | Some wire -> Wire wire
            | None -> failwith "Could not parse input!"

   type Circuit =
   | Direct of In
   | Not    of In
   | And    of In * In
   | Or     of In * In
   | LShift of what: In * by: In
   | RShift of what: In * by: In

[<Struct>]
type Gate = {Inputs: Gate.Circuit; Output: Gate.WireType}

module Gate =
   open Gate
   let i = In.fromString
   let fromString (s: string): Gate =
      let words = s.Split ""
      let last  = words.Length - 1
      let circuit =
         match words.[..last - 1] with
         | [|a|]              -> Direct (i a)
         | [|"NOT"; a|]       -> Not    (i a)
         | [|a; "AND"; b|]    -> And    (i a, i b)
         | [|a; "OR" ; b|]    -> Or     (i a, i b)
         | [|a; "LSHIFT"; b|] -> LShift (i a, i b)
         | [|a; "RSHIFT"; b|] -> RShift (i a, i b)
         | _ -> failwith "Unknown wire type!"

      let output =
         words.[last]
         |> WireType.fromString
         |> Option.unwrap "Could not parse output!"

      {Inputs = circuit; Output = output}

let filename = __SOURCE_DIRECTORY__ + "/wires.txt"

let instructions =
   filename
   |> File.ReadAllLines
   |> Array.map Gate.fromString
