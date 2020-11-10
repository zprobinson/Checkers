module ParseCharacterInput

open Checkers.CheckerTypes

let mapCharToDimension dList (cList: char list) (c: char) =
    dList
    |> List.zip cList
    |> Map.ofList
    |> Map.find c
    
let createCell (input: string) : Cell =
    let flag = input.Length = 2
    let mapToRow = mapCharToDimension Row.List ['1'..'8']
    let mapToCol = mapCharToDimension Column.List ['a'..'h']
    if flag then
        try
            let chars = input.ToCharArray()
            let col = mapToCol (chars |> Array.head |> System.Char.ToLower)
            let row = mapToRow (chars |> Array.last)
            { Column = col; Row = row }
        with
            // default bad square to handle error
            | :? System.ArgumentException
            | :? System.Collections.Generic.KeyNotFoundException ->
                { Column = B; Row = One }   
    else raise (System.ArgumentException("Input length much be exactly 2 to create a cell.")) 
    
let splitFullMove (input: string) =
    let result = input.Split(' ')
    match result.Length with
    | 2 -> result
    | _ -> invalidArg "result" "Incorrect number of moves in your input."
    
let createAttemptedMove (input: string) =
    try
        let moves = splitFullMove input
        let start = createCell (moves |> Array.head)
        let target = createCell (moves |> Array.last)
        { FromCell = start; ToCell = target }
    with
    // default bad attempted move to handle error
    | :? System.ArgumentException 
    | :? System.Collections.Generic.KeyNotFoundException ->
        { FromCell = { Column = B; Row = One }; ToCell = { Column = B; Row = One } }   
