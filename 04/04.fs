open System

let secretKey = "iwrupvqb"

let md5 = Security.Cryptography.MD5.Create()

let inline md5Hash (input : string) =
   input
   |> Text.Encoding.ASCII.GetBytes
   |> md5.ComputeHash

let startsWithFiveZeros (bytes: byte[]) =
   bytes.[0] ||| bytes.[1] ||| (bytes.[2] &&& 0xF0uy) = 0uy

let startsWithSixZeros (bytes: byte[]) =
   bytes.[0] ||| bytes.[1] ||| bytes.[2] = 0uy

let inline makeKey (i: uint) = secretKey + string i

let mineWith predicate =
   let rec mine = fun (i: uint) ->
      let key = makeKey i
      if key |> md5Hash |> predicate then
         key
      else
         mine (i + 1u)
   mine

let baToString (ary: byte[]) = BitConverter.ToString(ary).Replace("-", "")

let mine5 = mineWith startsWithFiveZeros
let mine6 = mineWith startsWithSixZeros

let inline (>>>) f0 f1 a b = f1 (f0 a b)
let display = sprintf "%-18s = %s" >>> Console.WriteLine

let key5 = mine5 0u
let hash5 = key5 |> md5Hash |> baToString

display key5 hash5

let key6 = mine6 0u
let hash6 = key6 |> md5Hash |> baToString

display key6 hash6
