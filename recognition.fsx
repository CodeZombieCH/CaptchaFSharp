module Recognition


open System
open System.IO

let dataReader path = 
    File.ReadAllLines path
    |> Array.map (fun x -> x.Split(',')) 
    |> Array.tail
    |> Array.map (Array.map int)
    |> Array.map (fun x -> (x.[0], x.[1..]))


let trainingData = dataReader <| "data/trainingsample_zhaw.csv" 

let validationData = dataReader <| "data/validationsample_zhaw.csv"


// Jedes Element der einglesenen Daten ist vom Typ int * int [].
// Die Elemente repräsentieren einzelne handschriftliche Ziffern von 0 bis 9 als Graustufenbild, zusammen mit dem entsprechenden Wert als int.
// Die Graustufenbilder sind im Format 28x28 mit Grauwerten von 0 bis 255 als int [] der Länge 784 (= 28 * 28) gegeben.


// Befor wir uns daran machen die Bilder zu klassifizieren, wollen wir sie ausgeben können.
// Implementieren Sie eine Funktion print, die ein Datenelement entgegennimmt und das enthaltene 
// Graustufenbild auf der Konsole (oder sonstwie) 'Zeichnet'. Sie können für die Graustufen die Liste ' '::['░'..'▓'] verwenden.
// Eine Ausgabe der print funktion könnte etwa wie folgt aussehen:
(*                            
                            
              ░▓▒           
              ▓▓▒           
             ▒▓▓            
            ░▓▓░            
            ▓▓▓             
           ▒▓▓░             
           ▓▓▓              
          ▓▓▓░              
          ▓▓▒  ░▒▓▓▓░       
         ▒▓▓░ ░▓▓▓▓▓▒       
         ▓▓▓ ░▓▓▒ ░▓▓░      
        ░▓▓░░▓▓▒    ▒▓      
        ░▓▓░▒▓▓     ░▓░     
        ░▓▓░▓▓      ▒▓▓     
        ░▓▓▒▓▓      ▓▓░     
         ▓▓▓▓▓     ▓▓▓      
         ▒▓▓▓▒   ▒▓▓▓░      
          ▓▓▓▓▓▓▓▓▓▓░       
           ▒▓▓▓▓▓▓▓░        
             ▒▒▒▒           
                            
*)

let print (key, data) =
    let greys = ' '::['░'..'▓']
    data
    |> Array.chunkBySize 28
    |> Array.iter(fun y -> Array.iter(fun x -> printf "%c" greys.[x/64]) y; printfn "")



// Nun geht es darum einen Algorithmus zu entwerfen, der mithilfe von trainingData die Bilder der 
// validationData ihren entsprechenden int werten zuordnen kann.
// Als Klassifizierungsmethode wollen wir einen einfachen k-Nearest-Neighbor-Algorithmus verwenden (de.wikipedia.org/wiki/N%C3%A4chste-Nachbarn-Klassifikation).
 
// Nun geht es darum eine 'Distanzfunktion' auf der Menge der Graustufenbilder zu definieren.
// Die Signatur dieser Funktion sollte (int [] -> int [] -> int) sein. Ähnliche Bilder sollten einen kleinen Wert hervorbringen wohingegen sehr unterschiedliche Bilder
// "weit entfernt" voneinander sind. 
// Schliesslich werden Sie mit verschiedenen "Distanzfunktionen" experimientieren und testen welche die besten Resultate liefern.

let dist a b = 
    Array.fold2 (fun result x y ->
        let diff = x - y
        result + diff * diff) 0 a b


// Diese Funktion soll zu gegebener Distanzfunktion 'metric' und gegebenem n: int und Datenelement 'sample' aus einer Mengen 'collection'
// von Datenelementen die n bezüglich der Distanzfunktion 'metric' nächsten Elemente ausgeben.
// Die Signatur könnte etwa wie folgt aussehen:
// metric:('a -> 'b -> 'c) ->
//    n:int -> 'd * 'b -> collection:('e * 'a) [] -> ('e * 'a) []

let findNearestN metric n sample collection =
    (Array.map(fun x -> 
        let (a, b) = x
        (metric sample b, b)) collection
    |> Array.sort |> Array.sub) 0 n


// Wir setzen nun

let guess metric n sample = findNearestN metric n sample trainingData

// Damit erhalten wir eine Funktion die zu jedem gegebenen Sample (z.B. aus der Validation Set) die bezüglich der angegebenen 
// Metrik die n ähnlichsten Elemente aus trainingData zurückgibt. Damit lässt sich nun einfach ein k-nearest-neighbour algorithmus implementieren.
// Als Mass für die Qualität ihres Algorithmus geben Sie prozentual aus, wieviele Samples aus validationData richtig zugeordnet werden konnten.
// Zusätzlich können Sie z.B. ihren Algorithmus so schreiben, dass nicht erkannte Ziffern auf der Konsole ausgegeben werden. 




//////////////////////
// Helper Functions //
//////////////////////

let printIndexedData data start count =
    Array.fold (fun result x -> 
        let (a, b) = x
        printfn "index: %d - int: %d" result a
        result + 1) start (Array.sub data start count)

let getDataRecordAtIndex data index =
    Array.get data index

let getDataAtIndex data index =
    let (a, b) = getDataRecordAtIndex data index
    b

let gdrai index = getDataRecordAtIndex trainingData index

let gdai index = getDataAtIndex trainingData index

