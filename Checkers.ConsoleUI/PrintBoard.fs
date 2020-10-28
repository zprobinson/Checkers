﻿module PrintBoard

open Checkers
open Checkers.CheckerTypes

    let initGameState =
        Implementation.initGame()

    let initBoard =
        initGameState.Board

    let private getCellValue (board: Board) (cell: Cell) = 
        match board.[cell] with
            | Some checker -> 
                match checker with
                | Black, Soldier -> " [x]"
                | Black, King -> " [X]"
                | Red, Soldier -> " [o]"
                | Red, King -> " [O]"
            | None -> " [ ]"

    let private printCell = getCellValue initBoard

    let private printRowLabel (row: Row) list =
        sprintf " %d | " ((list |> List.findIndex(fun r -> r = row)) + 1)

    let private checkerBoardList = 
        let rowRev = Row.List |> List.rev
        [for row in rowRev do
            printRowLabel row Row.List
            for col in Column.List do
                let cell = printCell ({ Row = row; Column = col })
                match col with
                | H -> sprintf "%s\n" cell
                | _ -> sprintf "%s"   cell ]

    let private checkerBoard =
        checkerBoardList |> List.reduce (+)
 
    let private bottomPadding length =
        String.replicate length " "

    let private columnBorderBuilder str list = 
        [for _ in list do
            sprintf "%s" str]
        |> List.reduce (+)

    let private columnLabelBuilder list =
        [for item in list do
            sprintf " %A  " item]
        |> List.reduce (+)

    let private bottomBorder =
        bottomPadding 6 + columnBorderBuilder "___ " Column.List + "\n" +
        bottomPadding 6 + columnLabelBuilder Column.List + "\n\n"

let printBoard =
    sprintf "%s" checkerBoard + 
    sprintf "%s" bottomBorder

let printPrompt = 
    sprintf "Please enter moves in the following format: A3 B4 (case insensitive)"