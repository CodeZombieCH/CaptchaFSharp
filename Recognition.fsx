module Recognition

let guessNumber findFunction compareFunction n sample validationData =
    findFunction compareFunction n sample validationData
    |> Seq.countBy (fun x -> fst (snd x)) |> Seq.toList |> List.map (fun x -> (fst x, (snd x)*100/n))

let findNearestN compareFunction n sample validationData =
    (Array.map(fun x -> (compareFunction sample (snd x), x)) validationData |> Array.sort |> Array.sub) 0 n


let compareNumber a b =
    Array.fold2 (fun result x y -> result + (x-y) * (x-y)) 0 a b


let printNumber (key, data) =
    let greys = ' '::['░'..'▓']
    Array.chunkBySize 28 data |> Array.iter(fun y -> 
        Array.iter(fun x -> printf "%c" greys.[x/64]) y
        printfn ""
    )

