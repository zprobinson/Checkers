module PrintBoard
open Checkers

let private getCellText (board: Board) (cell: Cell) =
    match board.[cell] with
    | Some checker ->
        match checker with
        | Black, Soldier -> " [x]"
        | Black, King -> " [X]"
        | Red, Soldier -> " [o]"
        | Red, King -> " [O]"
    | None -> " [ ]"

let private getRowLabel (row: Row) =
    sprintf " %d | " ((Row.List |> List.findIndex(fun r -> r = row)) + 1)

let private checkerBoardList board =
    let rowRev = Row.List |> List.rev
    [ for row in rowRev do
        yield getRowLabel row
        for col in Column.List do
            let cell = getCellText board ({ Row = row; Column = col })
            yield
                match col with
                | H -> sprintf "%s\n" cell
                | _ -> cell ]

let private checkerBoard board =
    board
    |> checkerBoardList
    |> String.concat ""

let private padding length =
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
    padding 6 + columnBorderBuilder "___ " Column.List + "\n" +
    padding 6 + columnLabelBuilder Column.List + "\n\n"

let printBoard (board: Board) =
    (checkerBoard board) + bottomBorder

let printPrompt =
    "Please enter moves in the following format: A3 B4 (case insensitive)"