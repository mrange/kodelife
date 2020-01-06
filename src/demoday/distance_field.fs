open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.PixelFormats

let maxDistance = 50
let limit = 0x80uy
let inline test (img : Gray8[, ]) x y = img.[x, y].PackedValue > limit

let distance x y =
  let x = float x
  let y = float y
  x*x + y*y |> sqrt

let sqrt2 = sqrt 2.0

let rec manhattan img md w h isInside x y off xa ya xx yy: float =
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

let inputPath = @"c:\temp\mimpulse.png"
let outputPath = @"c:\temp\df.png"

let loadImg (): Gray8[, ] =
  printfn "Loading input image: %s" inputPath
  use img = Image.Load<Gray8> inputPath
  let sz  = img.Size ()
  let w = sz.Width
  let h = sz.Height
  printfn "Image is %dx%d" w h
  let ps = Array2D.zeroCreate w h
  for y = 0 to (h - 1) do
    printfn "y: %d/%d" y h
    for x = 0 to (w - 1) do
      ps.[x, y] <- img.[x, y]
  ps


let distanceField (img: Gray8[, ]): float[, ] =
  printfn "Creating distance field..."

  let w = Array2D.length1 img
  let h = Array2D.length2 img
  let df = Array2D.zeroCreate w h

  for y = 0 to (h - 1) do
    printfn "y: %d/%d" y h
    for x = 0 to (w - 1) do
      let isInside = test img x y
      let d = manhattan img (float maxDistance) w h isInside x y 1.0 1 1 (x - 1) y
      let d = min 1.0 (d / float maxDistance)
      let d = if isInside then -d else d
      df.[x, y] <- d
  df

let downSample (df : float[,]): float[,] =
  printfn "Downsample distance field..."

  let w = Array2D.length1 df
  let h = Array2D.length2 df
  let w2 = w/2
  let h2 = h/2
  let df2 = Array2D.zeroCreate w2 h2

  for y = 0 to (h2 - 1) do
    printfn "y: %d/%d" y h2
    for x = 0 to (w2 - 1) do
      let d =
          df.[2*x + 0, 2*y + 0]
        + df.[2*x + 1, 2*y + 0]
        + df.[2*x + 0, 2*y + 1]
        + df.[2*x + 1, 2*y + 1]
      let d = d/4.
      df2.[x, y] <- d
  df2

let saveImg (df : float[,]): unit =
  printfn "Creating final image.."

  let w = Array2D.length1 df
  let h = Array2D.length2 df
  use imgDf = new Image<Rgb24> (w, h)

  for y = 0 to (h - 1) do
    printfn "y: %d/%d" y h
    for x = 0 to (w - 1) do
      let d  = df.[x, y]
      let pd = float 0x7FFFFFu*d
      let id = float 0x800000u - pd |> round
      let id = uint32 id
      let r = byte (id >>> 16)
      let g = byte (id >>>  8)
      let b = byte (id >>>  0)
      let p = Rgb24 (r, g, b)
      imgDf.[x, y] <- p

  printfn "Writing output image: %s" outputPath
  imgDf.Save outputPath

[<EntryPoint>]
let main argv =
  try

    let img = loadImg ()
    let df = distanceField img
    let df2 = downSample df
//    let df2 = downSample df2

    saveImg df2

    0
  with
  | e -> printfn "Failed with: %s" e.Message; 999
