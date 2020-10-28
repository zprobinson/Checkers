// Learn more about F# at http://fsharp.org

open System
open Checkers.CheckerTypes
open Checkers

[<EntryPoint>]
let main argv =
    printfn "Checkers in F#!\n"

    let initGameState =
        Implementation.initGame()

    let initBoard=
        initGameState.Board

    let getCellValue (board: Board) (cell: Cell) = 
        match board.[cell] with
        | Some checker -> 
            match checker with
            | Black, Soldier -> " [x]"
            | Black, King -> " [X]"
            | Red, Soldier -> " [o]"
            | Red, King -> " [O]"
        | None -> " [ ]"

    let printCell = getCellValue initBoard

    let printRowLabel (row: Row) list =
        sprintf " %d | " ((list |> List.findIndex(fun r -> r = row)) + 1)

    let checkerBoardList = 
        let rowRev = Row.List |> List.rev
        [for row in rowRev do
             printRowLabel row Row.List
             for col in Column.List do
                 let cell = printCell ({ Row = row; Column = col })
                 match col with
                 | H -> sprintf "%s\n" cell
                 | _ -> sprintf "%s"   cell ]

    let checkerBoard =
        checkerBoardList |> List.reduce (+)
    
    let bottomPadding length =
        String.replicate length " "

    let columnBorderBuilder str list = 
        [for _ in list do
            sprintf "%s" str]
        |> List.reduce (+)

    let columnLabelBuilder list =
        [for item in list do
            sprintf " %A  " item]
        |> List.reduce (+)

    let bottomBorder =
        bottomPadding 6 + columnBorderBuilder "___ " Column.List + "\n" +
        bottomPadding 6 + columnLabelBuilder Column.List + "\n\n"

    printf "%s" checkerBoard
    printf "%s" bottomBorder
    printfn "Please enter moves in the following format: A3 B4 (case insensitive)"







    0 // return an integer exit code
