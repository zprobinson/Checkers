module PrintBoard
open Checkers

let private printCell (board: Board) (cell: Cell) =
    match board.[cell] with
    | Some checker ->
        match checker with
        | Black, Soldier -> " [x]"
        | Black, King -> " [X]"
        | Red, Soldier -> " [o]"
        | Red, King -> " [O]"
    | None -> " [ ]"

let private printRowLabel (row: Row) =
    sprintf " %d | " ((Row.List |> List.findIndex(fun r -> r = row)) + 1)

let private checkerBoardList board =
    let rowRev = Row.List |> List.rev
    [for row in rowRev do
        printRowLabel row
        for col in Column.List do
            let cell = printCell board ({ Row = row; Column = col })
            match col with
            | H -> sprintf "%s\n" cell
            | _ -> sprintf "%s"   cell ]

let private checkerBoard board =
    board
    |> checkerBoardList
    |> String.concat ""

let private bottomPadding length =
    String.replicate length " "

let private columnBorderBuilder str list =
    list
    |> Seq.map (fun _ -> str)
    |> String.concat ""

let private columnLabelBuilder list =
    list
    |> Seq.map (sprintf "%A")
    |> String.concat ""

let private bottomBorder =
    bottomPadding 6 + columnBorderBuilder "___ " Column.List + "\n" +
    bottomPadding 6 + columnLabelBuilder Column.List + "\n\n"

let printBoard (board: Board) =
    sprintf "%s" (checkerBoard board) +
    sprintf "%s" bottomBorder

let printPrompt =
    sprintf "Please enter moves in the following format: A3 B4 (case insensitive)"