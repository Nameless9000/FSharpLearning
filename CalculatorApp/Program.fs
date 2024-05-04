module ConcatenationTest =
    let squares_to_25 =
        [ for i in 1 .. 25 do
            let square = i*i
            yield sprintf "%d*%d = %d" i i square ]

    squares_to_25
        |> String.concat "\n"
        |> printf "%s"