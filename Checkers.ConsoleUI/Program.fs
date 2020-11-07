// Learn more about F# at http://fsharp.org

open System
open Checkers
open CheckerTypes
open Implementation
open Initialization
open PrintBoard
open ParseCharacterInput
        
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
