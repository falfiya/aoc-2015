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

let WrappingPaperNeeded (b: Box) =
   let lwa = LWArea b
   let wha = WHArea b
   let hla = HLArea b
   (2 * lwa) + (2 * wha) + (2 * hla) + Array.min [| lwa; wha; hla |]

InputFile
|> IO.File.ReadAllLines
|> Array.sumBy (Box >> WrappingPaperNeeded)
|> Console.WriteLine
