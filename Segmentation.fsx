#load "Utils.fsx"
open Utils

type Range = { Start: int; End: int }
type Fragment = { x: Range; y: Range } with static member empty = {x= {Start=0; End=0}; y= {Start=0; End=0}}

// Returns a list of tuples with detected character ranges (xStart, xEnd)
// (only x cordinates, because it scans from left to right)
let scanner image threshold = 
    List.fold (fun (startX, fragments) x ->
        let hasDarkPixels = List.fold (fun s y -> s || Array2D.get image y x > threshold) false [0..(Array2D.length1 image)-1]

        // TODO: Calculate ymin and ymax in this function
        // There is no reason to iterate twice (shrinkBox should become obsolete)

        match startX with
        | 0 when hasDarkPixels -> (x, fragments)
        | 0 when not hasDarkPixels -> (0, fragments)
        | _ when hasDarkPixels -> (startX, fragments)
        | _ -> (0, (startX, x-1)::fragments)

    ) (0, List.empty) [0..(Array2D.length2 image)-1]
    |> snd

// Tries to calculated the smallest box with content for the passed character range (xStart, xEnd)
let shrinkBox (startX, endX) image threshold =
    let fmin x y =
        if x < y then x 
        else y

    let fmax x y = 
        if x > y then x
        else y

    List.fold (fun (yStart, yEnd) x ->
        List.fold (fun (yStart, yEnd) y ->
            if Array2D.get image y x > threshold then (fmin yStart y, fmax yEnd y)
            else (yStart, yEnd)
        ) (yStart, yEnd) [0..(Array2D.length1 image)-1]
    ) ((Array2D.length1 image)-1, 0) [startX..endX]


let shrinkFragments threshold preparedImage fragments =
    List.map (fun (startX, endX) ->
        let (startY, endY) = shrinkBox (startX, endX) preparedImage threshold
        (startX, endX, startY, endY)
    ) fragments

// Run dat shiat
let threshold = 20
let preparedImage = toGrayscale "images/dave2_test_dave.png"
let fragments = scanner preparedImage threshold

let shrinkedFragments = shrinkFragments threshold preparedImage fragments

// Helper: Create and save fragments as images
shrinkedFragments
    |> List.map (getImageFragment preparedImage)
    |> List.iteri (fun i ->
        toImage (String.concat "" ["images/fragment"; (string)i; ".png"])
       )

// TODO: Go on...
