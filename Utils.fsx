module Utils

let convert (p: System.Drawing.Color) =
        int (((float p.R) * 0.3) + ((float p.G) * 0.59) + ((float p.B) * 0.11))

let invert x = 255 - x

let toGrayscale (path: string) =
    printf "%s" path
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

let getImageFragment image (startX, endX, startY, endY) = 
    Array2D.init (endY-startY+1) (endX-startX+1) (fun y x -> Array2D.get image (startY+y) (startX+x))

// Verification
let buildPath part =
    let basePath = "images/"
    let imageName = "dave2"
    let path =
        if String.length(part) = 0 then String.concat "" [basePath; imageName; ".png"]
        else String.concat "" [basePath; imageName; "_"; part; ".png"]
    printf "%s" path
    path
