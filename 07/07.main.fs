module Main
open System

Shared.instructions
|> Array.map Console.WriteLine
|> ignore

// "123 -> x"
// |> Shared.Instructions.fromString
// |> Console.WriteLine
