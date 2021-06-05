open System

let scWidth, scHeight = 30, 30

module SharedMath =
  let distance x y =
    let xDist = abs ((float x) - (float scWidth) / 2.0)
    let yDist = abs ((float y) - (float scHeight) / 2.0)

    round (sqrt (xDist**2.0 + yDist**2.0))

module Art =
  let checkerboard x y = 
    if y % 2 = 0 then
      if x % 2 = 0 then 1 else 0
    else
      if x % 2 = 0 then 0 else 1

  let randomSquares x y =
    let rand = Random()
    int (round (rand.NextDouble()))

  // The functions below use math from the polar graphing system,
  // starting simple and getting progressively more complicated
  let filledCircle x y =
    let radius = 12
    if SharedMath.distance x y < (float radius) then 1 else 0

  let outlineCircle x y =
    let radius = 13
    if abs (SharedMath.distance x y - (float radius)) < 1.0 then 1 else 0
  

module Render =
  let generateScreen pattern = [
    for y in 0 .. scHeight ->
      [ for x in 0 .. scWidth -> pattern x y ]
  ]

  let render pattern =
    let screen = generateScreen pattern

    for y in 0 .. scHeight - 1 do
      for x in 0 .. scWidth - 1 do
        let output =
          match screen.[y].[x] with
          | 0 -> "⬛ "
          | 1 -> "⬜ "
          | _ -> "Error, x is neither a 0 nor a 1"
        
        printf "%s" output
      printfn ""
  
  render Art.outlineCircle