module IntegersAndNumbers =
    let sampleTableOfSquares = [ for i in 0 .. 99 -> i.ToString() ]

    let rec join (x: list<string>) (separator: string): string =
        match x with
        | [] -> ""
        | head :: [] -> head
        | head :: tail -> head + separator + join tail separator

    printfn "%s" (join sampleTableOfSquares ", ")