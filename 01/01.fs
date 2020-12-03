open System

[<Literal>]
let InputFile = "input.txt"

let parenToInt (p: byte) =
   match Convert.ToChar p with
   | '(' -> +1
   | ')' -> -1
   | _   ->  0

let main() =
   InputFile
   |> IO.File.ReadAllBytes
   |> Array.sumBy parenToInt
   |> Console.WriteLine

main()
