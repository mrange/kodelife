open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats

let maxDistance = 48
let limit = 0x80uy
let inline test (img : Image<Gray8>) x y = img.[x, y].PackedValue > limit

let distance x y =
  let x = float x
  let y = float y
  x*x + y*y |> sqrt

let sqrt2 = sqrt 2.0

let rec manhattan img md w h isInside x y off xa ya xx yy =
  let validPos = xx >= 0 && xx < w && yy >= 0 && yy < h
  let md =
    if validPos && (test img xx yy) <> isInside then
      let s = 1.0 - abs ((float img.[x, y].PackedValue - float limit)/128.)
      let d = (distance (xx - x) (yy - y)) - 0.5 - 0.5*s  // Some attempts to smooth the inferred curve
      min md d
    else
      md
  let inline loop off xa ya xx yy = manhattan img md w h isInside x y off xa ya xx yy
  match xa, ya with
  | +1, +1 when x = xx -> loop off  1 -1 (xx + 1) (yy - 1)
  | +1, -1 when y = yy -> loop off -1 -1 (xx - 1) (yy - 1)
  | -1, -1 when x = xx -> loop off -1  1 (xx - 1) (yy + 1)
  | -1, +1 when y = yy -> if off < md * sqrt2 then loop (off + 1.0) 1  1 (xx - 1) y else md
  | _ -> loop off xa ya (xx + xa) (yy + ya)

[<EntryPoint>]
let main argv =
  try
    use img = Image.Load<Gray8> @"c:\temp\mimpulse.png"
    let sz  = img.Size ()
    printfn "Image is %dx%d" sz.Width sz.Height

    use df = new Image<Rgb24> (sz.Width, sz.Height)

    let w = sz.Width
    let h = sz.Height

    for y = 0 to (h - 1) do
      printfn "y: %d/%d" y h
      for x = 0 to (w - 1) do
        let isInside = test img x y
        let d = manhattan img (float maxDistance) w h isInside x y 1.0 1 1 (x - 1) y
        let d = min 1.0 (d / float maxDistance)
        let pd = uint32 (float 0x7FFFFFu * d)
        let id = if isInside then 0x800000u + pd else 0x800000u - pd
        let r = byte (id >>> 16)
        let g = byte (id >>>  8)
        let b = byte (id >>>  0)
        let p = Rgb24 (r, g, b)
        df.[x, y] <- p

    df.Save @"c:\temp\output.png"

    0
  with
  | e -> printfn "Failed with: %s" e.Message; 999
