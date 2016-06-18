#load "recognition.fsx"
open Recognition

// Test while implementation    
Array.iter print validationData.[0..2]


// Testen Sie Ihre Funktion mit Array.iter print validationData
Array.iter print validationData

////////////////////
// Trainingsfield //
////////////////////


printIndexedData trainingData 200 50

dist (gdai 235) (gdai 231)

let sample = gdai 236
print (gdrai 236)

findNearestN dist 20 sample trainingData |> Array.map (fun x -> print x)





print (gdrai 231)
    



let x = (1, "foo")
let (a',b') = x


let (a,b) = validationData.[0]
b |> Array.chunkBySize 4

let foo = 2

let add x y = x + y

let myList = [1..5]
Array.fold2 (fun currentVal x y -> currentVal + x + y) 0 b


