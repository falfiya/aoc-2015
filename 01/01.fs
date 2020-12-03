open System

[<Literal>]
let InputFile = "input.txt"

let parenToInt (p: char) =
   match Convert.ToChar p with
   | '(' -> +1
   | ')' -> -1
   | _   ->  0

let flip f a b = f b a

let append a = flip (+) a

let chars = (
   InputFile
   |> IO.File.ReadAllBytes
   |> Array.map Convert.ToChar
)

let aaaa b a = ((b, a), a)

let main() =
   // part 1
   chars
   |> Array.sumBy parenToInt
   |> Convert.ToString
   |> (+) "Part 1: Santa is on floor "
   |> append " by the end of the route. "
   |> Console.WriteLine

   chars
   |> Array.indexed
   |> Array.mapFold (fun acc (idx, char) -> acc + (parenToInt char) |> aaaa idx) 0
   |> fst
   |> Array.find (fun (_, lvl) -> lvl < 0)
   |> fun (idx, lvl) -> (Convert.ToString idx, Convert.ToString lvl)
   |> fun (idx, lvl) -> "Part 2: Santa enters the basement (" + lvl + ") on INDEX " + idx
   |> Console.WriteLine

main()
