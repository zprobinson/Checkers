// Learn more about F# at http://fsharp.org

open System
open Checkers.CheckerTypes
open Checkers
open PrintBoard

let rec renderBoard (gameState : GameState) =
    printfn "Checkers in F#!\n"
    //print checkerboard
    printf "%s" printBoard
    //write gameState message
    printfn "%s" gameState.Message
    //prompt and receive input
    printfn "%s" printPrompt
    printf "> "
    //parse input to AttemptedMove

    //validate AttemptedMove

    //return new GameState object

    //clear console
    let input = Console.ReadLine()
    Console.Clear()
    //call this method
    renderBoard gameState


[<EntryPoint>]
let main argv =
    renderBoard (Implementation.initGame())


    0 // return an integer exit code
