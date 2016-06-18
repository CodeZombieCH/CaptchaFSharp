#load "Recognition.fsx"
#load "DataImporter.fsx"

open Recognition
open DataImporter



let trainingData = importTrainingData "data/train_kaggle.csv"
let testData = importTestData "data/test_kaggle.csv"


//helper function to guess the data
let guessNumberWithPredefinedFunctions n testDataId =
    guessNumber findNearestN compareNumber n (snd testData.[testDataId]) trainingData


//run
guessNumberWithPredefinedFunctions 100 23


//Write to csv
// open System
// open System.IO

// let dataWriter path =
//     File.WriteAllLines(path, Array.map (fun x -> (string) x) [|1..100|])

// let guessNumberForAllTestData = 
//     guessNumber findNearestN compareNumber
//     Array.mapi (fun id x -> (id, fst x.[0]) 

// dataWriter "/tmp/foo.csv"