open System

let apply = (|>)
let atLeast = (<=)
let inline flip f a b = f b a

module String =
   let includes = (=) >> String.exists
   let contains (needle: string) (haystack: string) = haystack.Contains(needle)

module Array =
   let includes = (=) >> Array.exists

let vowels = [| 'a'; 'e'; 'i'; 'o'; 'u' |]

let isVowel = Array.includes >> apply vowels

let hasAtLeastThreeVowels =
   isVowel
   |> String.filter
   >> String.length
   >> atLeast 3

let isDouble (a, b) = a = b

let hasDouble: string -> bool =
   List.ofSeq
   >> Seq.pairwise
   >> Seq.exists isDouble

// let's try this one differently
let badWords = [| "ab"; "cd"; "pq"; "xy" |]

let hasBadWords =
   flip String.contains
   >> Array.exists
   >> apply badWords

let hasNoBadWords = hasBadWords >> not

let nicePredicates = [|
   hasAtLeastThreeVowels
   hasDouble
   hasNoBadWords
|]

let isNice =
   apply
   >> Array.forall
   >> apply nicePredicates

IO.File.ReadAllLines "words.txt"
|> Array.filter isNice
|> Array.length
|> string
|> (+) "There are "
|> Console.WriteLine
