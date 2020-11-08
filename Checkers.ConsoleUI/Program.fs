open System
open Checkers
open CheckerTypes
open Implementation
open Initialization
open PrintBoard
open ParseCharacterInput
        
let rec renderBoard (gameState : GameState) =
    //print game board and current message
    printfn "%As turn to move.\n" gameState.ColorToMove
    printf "%s" (printBoard gameState.Board)
    printfn "%s" gameState.Message

    //prompt user for input
    printfn "%s" printPrompt
    printf "> "
    let input = Console.ReadLine()

    //parse input to AttemptedMove type
    let attempt = createAttemptedMove input

    //validate AttemptedMove and refresh display using new GameState
    let newGameState = updateGame gameState attempt
    Console.Clear()
    renderBoard newGameState

[<EntryPoint>]
let main argv =
    renderBoard (initNewGame())
    //renderBoard (initMultipleCaptureTest())
    //renderBoard (initWinConditionTest())
    0 
