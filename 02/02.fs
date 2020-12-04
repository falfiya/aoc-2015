open System

let InputFile = "input.txt"

[<Measure>] type Ft

let Ft = 1<Ft>
let Ft0 = 0<Ft>

type Box = struct
   val Width: int<Ft>
   val Height: int<Ft>
   val Length: int<Ft>

   new (width, height, length) = {
      Width = width;
      Height = height;
      Length = length;
   }

   new (s: string) =
      let dim = s.Split 'x' |> Array.map (int >> ( * ) Ft)
      if dim.Length < 3 then
         Box(Ft0, Ft0, Ft0)
      else
         Box(dim.[0], dim.[1], dim.[2])
end

let LWArea (b: Box) = b.Length * b.Width
let WHArea (b: Box) = b.Width  * b.Height
let HLArea (b: Box) = b.Height * b.Length

let WrappingPaperNeeded (b: Box): int<Ft^2> =
   let lwa = LWArea b
   let wha = WHArea b
   let hla = HLArea b
   (2 * lwa) + (2 * wha) + (2 * hla) + Array.min [| lwa; wha; hla |]

let flip f a b = f b a

let append a = flip (+) a

let stringft2 (sqft: int<Ft^2>) = (string sqft) + " ft²"

let boxes = InputFile |> IO.File.ReadAllLines |> Array.map Box

boxes
|> Array.sumBy WrappingPaperNeeded
|> stringft2
|> (+) "Part 1: The elves need "
|> append " of wrapping paper."
|> Console.WriteLine

let Volume (b: Box): int<Ft^3> = b.Width * b.Height * b.Length

let stringft (ft: int<Ft>) = (string ft) + " ft"

let SmallestPerimeter (b: Box): (int<Ft> * int<Ft>) =
   let sides = Array.sort [| b.Width; b.Height; b.Length |]
   (sides.[0], sides.[1])

let RibbonNeeded (b: Box) =
   let (pA, pB) = SmallestPerimeter b
   pA * 2 + pB * 2 + Volume b / 1<Ft^2>

boxes
|> Array.sumBy RibbonNeeded
|> stringft
|> (+) "Part 2: The elves need "
|> append " of ribbon."
|> Console.WriteLine

