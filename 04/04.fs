open System

let md5 = Security.Cryptography.MD5.Create()

let md5Hash (input : string) =
   input
   |> Text.Encoding.ASCII.GetBytes
   |> md5.ComputeHash

let startsWithFiveZeros (bytes: byte[]) =
   (bytes.[0] ||| bytes.[1] ||| (bytes.[2] &&& 0xF0uy)) = 0uy

let secretKey = "iwrupvqb"

let makeKey (i: uint) = secretKey + string i

let rec mine (i: uint) =
   if
      makeKey i
      |> md5Hash
      |> startsWithFiveZeros
   then
      i
   else
      mine (i + 1u)

let flip f a b = f b a
let (++) a = flip (+) a

let baToString (ary: byte[]) = BitConverter.ToString(ary)

let keyEnd = mine 0u
let key = makeKey keyEnd
let hash = md5Hash key

Console.WriteLine (sprintf "%s = %s" key (baToString hash))
