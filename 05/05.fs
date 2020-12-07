open System

let apply = (|>)
let atLeast = (<=)
let inline flip f a b = f b a

module String =
   let includes = (=) >> String.exists
   let contains (needle: string) (haystack: string) = haystack.Contains(needle)

module Array =
   // stupid type system
   // let includes = (=) >> Array.exists
   let includes (what: 'T) = Array.exists ((=) what)

let lines = IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/words.txt")

module PuzzleOne =
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

   lines
   |> Array.filter isNice
   |> Array.length
   |> string
   |> (+) "There are "
   |> ((+) >> (|>) " nice words.")
   |> Console.WriteLine

// https://stackoverflow.com/a/36570457/
module Seq =
   let skipSafe count source =
      source
      |> Seq.indexed
      |> Seq.filter (fst >> (<=) count)
      |> Seq.map snd


let (%&) fn0 fn1 a = (fn0 a) && (fn1 a)

module PuzzleTwo =
   let hasDouble (s: string) =
      let pairs = s |> Seq.pairwise |> Seq.toArray
      let max = pairs.Length - 2
      let rec loop i =
         i < max && (Array.includes pairs.[i] pairs.[i + 2..] || loop (i + 1))
      loop 0

   let hasOneSurrounded (s: string) =
      let chars = s |> Seq.toArray
      let max = chars.Length - 3
      let rec loop i = i < max && (chars.[i] = chars.[i + 2] || loop (i + 1))
      loop 0

   let isNicer = hasDouble %& hasOneSurrounded

   lines
   |> Array.filter isNicer
   |> Array.length
   |> string
   |> (+) "There are "
   |> ((+) >> (|>) " nicer words.")
   |> Console.WriteLine
