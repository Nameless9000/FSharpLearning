module IntegersAndNumbers =
    let sampleTableOfSquares = [ for i in 0 .. 99 -> "hi: " + i.ToString() ]

    let rec join (x: list<string>) (seperator: string): string =
        if x.IsEmpty then
            ""
        else
            x.Head + seperator + join x.Tail seperator

    printfn "%s" (join sampleTableOfSquares "\n")