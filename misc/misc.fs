let inline flip f a b = f b a
let inline (++) a = flip (+) a
let inline (>>>) f0 f1 a b = f1 (f0 a b)
let inline (<-->) fn0 fn1 arg = (fn0 arg, fn1 arg)
// let fork: ('T -> 'U)[] -> 'T -> 'U[] = flip Array.map
let inline (|%) f0 arg = f0 arg

// printf "%d" |% Array.reduce (+) [| 1; 2; 3 |]
open System
let displayCat (name: string) =
   name
   |> (+) "The cat's name is "
   |> ((+) >> (|>) "!")
   |> Console.WriteLine

displayCat "Garfield"
