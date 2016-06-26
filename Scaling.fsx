#load "Utils.fsx"
open Utils


let pad image (sizeX, sizeY) =
    let x = (sizeX - (Array2D.length2 image)) / 2
    let y = (sizeY - (Array2D.length1 image)) / 2

    printf "sizeX: %d sizeY: %d x: %d y:%d\n" sizeX sizeY x y 

    let padded = Array2D.zeroCreate sizeY sizeX
    Array2D.blit image 0 0 padded y x (Array2D.length1 image) (Array2D.length2 image)
    padded


let image = toGrayscale "images/fragment4.png"
pad image (28,28) |> toImage "images/fragment4padded.png"
