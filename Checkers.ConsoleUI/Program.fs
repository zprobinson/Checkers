// Learn more about F# at http://fsharp.org

open System
open Checkers.CheckerTypes
open Checkers.Implementation
open Checkers
open PrintBoard

let mapCharToRow c =
    match c with
    | '1' -> One
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | _ -> failwith "a row input does not exist"
    
let mapCharToCol c =
    match Char.ToLower(c) with
    | 'a' -> A
    | 'b' -> B
    | 'c' -> C
    | 'd' -> D
    | 'e' -> E
    | 'f' -> F
    | 'g' -> G
    | 'h' -> H
    | _ -> failwith "a column input does not exist"

let createCell (input: string) : Cell =
    let flag = input.Length = 2
    if flag then
        let chars = input.ToCharArray()
        let col = mapCharToCol (chars |> Array.head)
        let row = mapCharToRow (chars |> Array.last)
        { Column = col; Row = row }
    else failwith "a move input has more than 2 characters"

let splitFullMove (input: string) =
    let result = input.Split(' ')
    match result.Length with
    | 2 -> result
    | _ -> failwith "incorrect number of moves in your input"

let createAttemptedMove (input: string) =
    let moves = splitFullMove input
    let start = createCell (moves |> Array.head)
    let target = createCell (moves |> Array.last)
    { FromCell = start; ToCell = target }

let rec renderBoard (gameState : GameState) =
    printfn "Checkers in F#!\n"
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
    renderBoard (Implementation.initGame())


    0 // return an integer exit code
