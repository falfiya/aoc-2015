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

let walk (loc: Location, map: SantaMap) (arrow: char) =
   let (x, y) = loc;
   let newloc =
      match arrow with
      | '^' -> (x, y + 1)
      | '>' -> (x + 1, y)
      | 'v' -> (x, y - 1)
      | '<' -> (x - 1, y)
      | _ -> loc
   (newloc, increment newloc map)

let (finalLoc, finalMap) =
   file
   |> Array.fold walk ((0, 0), Map.empty.Add((0, 0), 1u))

finalLoc
|> string
|> (+) "Santa ended his trip at "
|> Console.WriteLine

finalMap
|> Map.toSeq
|> Seq.map snd
|> Seq.filter (fun count -> count > 1u)
|> Seq.length
|> string
|> (+) "There were "
|> (++) " houses that had more than one present"
|> Console.WriteLine
