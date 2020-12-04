open System

let md5 = Security.Cryptography.MD5.Create()

let md5Hash (input : string) =
   input
   |> Text.Encoding.ASCII.GetBytes
   |> md5.ComputeHash

let startsWithFiveZeros (bytes: byte[]) =
   bytes.[0] ||| bytes.[1] ||| (bytes.[2] &&& 0xF0uy) = 0uy

let startsWithSixZeros (bytes: byte[]) =
   bytes.[0] ||| bytes.[1] ||| (bytes.[2]) = 0uy

let secretKey = "iwrupvqb"

let makeKey (i: uint) = secretKey + string i

let mineWith fn =
   let rec mine = fun (i: uint) ->
      let key = makeKey i
      if
         key
         |> md5Hash
         |> fn
      then
         key
      else
         mine (i + 1u)
   mine

let flip f a b = f b a
let (++) a = flip (+) a

let baToString (ary: byte[]) = BitConverter.ToString(ary).Replace("-", "")

let mine5 = mineWith startsWithFiveZeros
let mine6 = mineWith startsWithSixZeros

let key5 = mine5 0u
let hash5 = key5 |> md5Hash |> baToString

Console.WriteLine (sprintf "%-18s = %s" key5 hash5)

let key6 = mine6 0u
let hash6 = key6 |> md5Hash |> baToString

Console.WriteLine (sprintf "%-18s = %s" key6 hash6)
