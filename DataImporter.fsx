module DataImporter

open System
open System.IO

let dataReader path = 
    File.ReadAllLines path
    |> Array.map (fun x -> x.Split(',')) 
    |> Array.tail
    |> Array.map (Array.map int)

let importTrainingData path =
    dataReader path |> Array.map (fun x -> (x.[0], x.[1..]))

let importTestData path = 
    dataReader path |> Array.map (fun x -> (0, x))