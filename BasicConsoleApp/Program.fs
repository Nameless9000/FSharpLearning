module ConcatenationTest =
    let rec join (x: list<string>) (separator: string): string =
        match x with
        | [] -> ""
        | head :: [] -> head
        | head :: tail -> head + separator + join tail separator

    let sampleTableOfSquares =
        [ for i in 0 .. 99 -> (i, i*i) ]
        |> List.map (fun (a, b) -> sprintf "%d*%d = %d" a a b)

    printfn "%s" (join sampleTableOfSquares "\n")