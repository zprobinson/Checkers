// Learn more about F# at http://fsharp.org

open System
open Checkers.CheckerTypes
open Checkers

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    printfn ""

    let gameState =
        Implementation.initGame()

    let board=
        gameState.Board

    let printCheckerboard (board: Board) (cell: Cell) = 
        match board.[cell] with
        | Some checker -> 
            match checker with
            | Black, Soldier -> " [x]"
            | Black, King -> " [X]"
            | Red, Soldier -> " [o]"
            | Red, King -> " [O]"
        | None -> " [ ]"

    let printCell = printCheckerboard board

    let checkersGrid = 
        [for row in Row.List do
         for col in Column.List do
         let cell = { Row = row; Column = col }
         match col with
         | H -> printfn " %s" (printCell cell)
         | _ -> printf " %s" (printCell cell) ]







    0 // return an integer exit code
