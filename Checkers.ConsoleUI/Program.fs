// Learn more about F# at http://fsharp.org

open System
open Checkers
open CheckerTypes
open Implementation
open Initialization
open PrintBoard

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
            let col = mapToCol (chars |> Array.head)
            let row = mapToRow (chars |> Array.last)
            { Column = col; Row = row }
        with
            | :? System.ArgumentException -> 
                { Column = B; Row = One }   // default bad square to handle error
            | :? System.Collections.Generic.KeyNotFoundException ->
                { Column = B; Row = One }   // default bad square to handle error
    else { Column = B; Row = One }  // default bad  square to handle error

let splitFullMove (input: string) =
    let result = input.Split(' ')
    match result.Length with
    | 2 -> result
    | _ -> invalidArg "result" "incorrect number of moves in your input"

let createAttemptedMove (input: string) =
    try
        let moves = splitFullMove input
        let start = createCell (moves |> Array.head)
        let target = createCell (moves |> Array.last)
        { FromCell = start; ToCell = target }
    with
    | :? System.ArgumentException ->
        // default bad attempted move to handle errer
        { FromCell = { Column = B; Row = One }; ToCell = { Column = B; Row = One } }    

let rec renderBoard (gameState : GameState) =
    printfn "%As turn to move.\n" gameState.ColorToMove

    //print checkerboard
    printf "%s" (printBoard gameState.Board)

    //write gameState message
    printfn "%s" gameState.Message

    //prompt and receive input
    printfn "%s" printPrompt
    printf "> "
    let input = Console.ReadLine()

    //parse input to AttemptedMove
    let attempt = createAttemptedMove input

    //validate AttemptedMove and
    //return new GameState object
    let newGameState = updateGame gameState attempt

    //clear console
    Console.Clear()

    //call this method
    renderBoard newGameState


[<EntryPoint>]
let main argv =
    //renderBoard (initNewGame())
    renderBoard (initMultipleCaptureTest())
    //renderBoard (initWinConditionTest())


    0 // return an integer exit code
