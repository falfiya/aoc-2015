open System

let flip f a b = f b a
let (++) a = flip (+) a

let file = "input.txt" |> IO.File.ReadAllBytes |> Array.map char

type Location = (int * int)
type SantaMap = Map<Location, uint>

let increment (loc: Location) (map: SantaMap) =
   match map.TryFind loc with
   | Some count -> map.Remove(loc) |> ignore; map.Add(loc, count + 1u)
   | None -> map.Add(loc, 1u)

let walk (santa: Location, map: SantaMap) (arrow: char) =
   let (x, y) = santa;
   let newSanta =
      match arrow with
      | '^' -> (x, y + 1)
      | '>' -> (x + 1, y)
      | 'v' -> (x, y - 1)
      | '<' -> (x - 1, y)
      | _ -> santa
   (newSanta, increment newSanta map)

let startingMap0: SantaMap = Map.empty.Add((0, 0), 1u)

let (finalLoc0, finalMap0) =
   file
   |> Array.fold walk ((0, 0), startingMap0)

finalMap0
|> Map.toSeq
|> Seq.map snd
|> Seq.filter (fun count -> count > 0u)
|> Seq.length
|> string
|> (+) "With just Santa, there were "
|> (++) " houses that had at least one present."
|> Console.WriteLine

let startingMap1 = Map.empty.Add((0, 0), 2u) // since there are 2 santas now

let state1 = (((0, 0), (0, 0)), startingMap1)

let even x = (x % 2) = 0
let odd = even >> not

let filterOnIndex fn =
   Array.indexed
   >> Array.filter (fst >> fn)
   >> Array.map snd

let santaInstructions1 = file |> filterOnIndex even

let robotInstructions1 = file |> filterOnIndex odd

let (_, mapAfterSanta1) =
   santaInstructions1
   |> Array.fold walk ((0, 0), startingMap1)
let (_, mapAfterRobot1) =
   robotInstructions1
   |> Array.fold walk ((0, 0), mapAfterSanta1)

mapAfterRobot1
|> Map.toSeq
|> Seq.map snd
|> Seq.filter (fun count -> count > 0u)
|> Seq.length
|> string
|> (+) "With robot Santa, there are "
|> (++) " houses that had at least one present."
|> Console.WriteLine
