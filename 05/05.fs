open System

let apply = (|>)
let atLeast = (<=)
let inline flip f a b = f b a

module String =
   let includes = (=) >> String.exists
   let contains (needle: string) (haystack: string) = haystack.Contains(needle)

module Array =
   let mapOn (ary: 'T[]) (mapping: 'T -> 'U) = Array.map mapping ary

let vowelPredicates = [| 'a'; 'e'; 'i'; 'o'; 'u' |] |> Array.map String.includes

let hasAtLeastThreeVowels =
   apply
   >> Array.mapOn vowelPredicates
   >> Array.filter id
   >> Array.length
   >> atLeast 3

let isDouble (a, b) = a = b

let hasDouble: string -> bool =
   List.ofSeq
   >> Seq.pairwise
   >> Seq.exists isDouble

// let's try this one differently
let badWords = [| "ab"; "cd"; "pq"; "xy" |]

let hasBadword: string -> bool =
   Array.mapOn badWords 
