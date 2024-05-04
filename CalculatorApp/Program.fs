open System.Text.RegularExpressions

module Calculator =
    type BinOp = Add | Sub

    type Expr =
        | Number of int
        | BinaryOp of BinOp * Expr * Expr

    let rec eval = function
        | Number n -> n
        | BinaryOp (op, e1, e2) ->
            let v1 = eval e1
            let v2 = eval e2
            match op with
            | Add -> v1 + v2
            | Sub -> v1 - v2

    let rec parse_expr (tokens: list<string>) =
        let rec inner (acc: Expr) (tokens: list<string>) =
            match tokens with
            | [] -> acc
            | num :: tail when System.Int32.TryParse(num).Equals(true) ->
                Number(int num)
            | symbol :: tail ->
                let op =
                    match symbol with
                    | "+" -> Add
                    | "-" -> Sub
                    | _ -> failwith "Unknown operator"

                BinaryOp(op, acc, parse_expr tail)

        inner (Number(int (List.head tokens))) (List.tail tokens)


    let tokenize (input: string) =
        let matches = Regex.Matches(input, "(\d+)|([+-])")
        [ for m in matches -> m.Value ]

    let parse (input: string) =
        let tokens = tokenize input
        parse_expr tokens

    [<EntryPoint>]
    let main args =
        printf "Enter the calculation: "
        let calculation = System.Console.ReadLine()
        let ast = parse calculation
        let result = eval ast

        printfn "%A\n" result

        0