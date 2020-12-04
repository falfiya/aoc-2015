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

let santaWalk (santa: Location, map: SantaMap) (arrow: char) =
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
   |> Array.fold santaWalk ((0, 0), startingMap0)

(*
finalLoc
|> string
|> (+) "Santa ended his trip at "
|> Console.WriteLine
*)

finalMap0
|> Map.toSeq
|> Seq.map snd
|> Seq.filter (fun count -> count > 0u)
|> Seq.length
|> string
|> (+) "With just Santa, there were "
|> (++) " houses that had at least one present."
|> Console.WriteLine

let robotWalk (robot: Location, map: SantaMap) (arrow: char) =
   let (x, y) = robot;
   let newRobot =
      match arrow with
      | '^' -> (x, y - 1)
      | '>' -> (x - 1, y)
      | 'v' -> (x, y + 1)
      | '<' -> (x + 1, y)
      | _ -> robot
   (newRobot, increment newRobot map)

let bothWalk ((santa: Location, robot: Location), map0: SantaMap) arrow =
   let (newSanta, map1) = santaWalk (santa, map0) arrow
   let (newRobot, map2) = robotWalk (robot, map1) arrow
   ((newSanta, newRobot), map2)

let startingMap1 = Map.empty.Add((0, 0), 2u) // since there are 2 santas now

let state1 = (((0, 0), (0, 0)), startingMap1)

let (finalLoc1, finalMap1) = file |> Array.fold bothWalk state1

finalMap1
|> Map.toSeq
|> Seq.map snd
|> Seq.filter (fun count -> count > 0u)
|> Seq.length
|> string
|> (+) "With robot Santa, there are "
|> (++) " houses that had at least one present."
|> Console.WriteLine
