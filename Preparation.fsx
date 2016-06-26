module Preparation


#load "Utils.fsx"
open Utils


let applyThreshold a threshold = 
    Array2D.map (fun x ->
        match (x, threshold) with
        | (x, threshold) when threshold x -> x
        | _ -> 0
    ) a



let test = toGrayscale (buildPath "")
toImage (buildPath "greyscale") test
//applyThreshold test (fun x -> x > 50 && x < 230) |> toImage (buildPath "threshold")



let darkNeighbouhr x y image n  = 
    let ymin = if y-n >= 0 then y-n else y
    let ymax = if y+n < Array2D.length2 image then y+n else y
    let xmin = if x-n >= 0 then x-n else x
    let xmax = if x+n < Array2D.length1 image then x+n else x
    List.fold (fun res x ->
        (List.fold (fun resI y -> if Array2D.get image x y > 50 then resI + 1 else resI) 0 [ymin..ymax])
    ) 0 [xmin..xmax]

let cleanImageFromPoints image =
    Array2D.mapi (fun x y p ->
        match p with
        | p when p < 50 -> 0
        | _ -> if darkNeighbouhr x y test 1 < 3 then 0 else p
    ) image


///clean test image
let preparedImage = cleanImageFromPoints test
toImage (buildPath "test_dave") preparedImage
