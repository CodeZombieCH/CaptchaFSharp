module Preparation

let convert (p: System.Drawing.Color) =
        int (((float p.R) * 0.3) + ((float p.G) * 0.59) + ((float p.B) * 0.11))

let invert x = 255 - x

let toGrayscale (path: string) =
    let bitmap = new System.Drawing.Bitmap(path)

    let a = Array2D.init bitmap.Height bitmap.Width (fun y x ->
        bitmap.GetPixel(x, y) |> convert |> invert
    )

    bitmap.Dispose()
    a

let toImage (path: string) a =
    let bitmap = new System.Drawing.Bitmap(Array2D.length2 a, Array2D.length1 a)
   
    // Invert values and set 
    Array2D.iteri (fun y x p ->
        bitmap.SetPixel(x, y, System.Drawing.Color.FromArgb(p, p, p))
    ) <| Array2D.map invert a 

    bitmap.Save(path, System.Drawing.Imaging.ImageFormat.Png)
    bitmap.Dispose()

let applyThreshold a threshold = 
    Array2D.map (fun x ->
        match (x, threshold) with
        | (x, threshold) when threshold x -> x
        | _ -> 0
    ) a


// Verification
let buildPath part =
    let basePath = "images/"
    let imageName = "dave2"
    let path =
        if String.length(part) = 0 then String.concat "" [basePath; imageName; ".png"]
        else String.concat "" [basePath; imageName; "_"; part; ".png"]
    printf "%s" path
    path


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
toImage (buildPath "test_dave") (cleanImageFromPoints test)