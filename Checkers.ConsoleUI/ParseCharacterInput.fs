module ParseCharacterInput

open Checkers

let mapCharToDimension dList (cList: char list) (c: char) =

    let mapped =
        dList
        |> List.zip cList
        |> List.tryPick (fun (character, mapping) ->
            if character = c then Some mapping
            else None)
    match mapped with
    | Some mapped ->
        Ok mapped
    | None ->
        c
        |> sprintf "'%c' is not valid."
        |> Error

// no point in recomputing these every time
let mapToRow = mapCharToDimension Row.List ['1'..'8']
let mapToCol = mapCharToDimension Column.List ['a'..'h']

let createCell (input: string) =
    let chars = input.ToCharArray()
    match chars with
    | [| col; row |] ->
        match mapToCol col, mapToRow row with
        | Ok col, Ok row ->
            Ok { Column = col; Row = row }
        | Error err, _ | _, Error err ->
            Error err
    | _ ->
        input
        |> sprintf "'%s' was the incorrect length for input!"
        |> Error

let splitFullMove (input: string) =
    match input.Split(' ') with
    | [| fromCell; toCell |] when fromCell.Length <> 2 || toCell.Length <> 2 ->
        Error "Input length much be exactly 2 to create a cell"
    | [| fromCell; toCell |] ->
        Ok (fromCell, toCell)
    | _ ->
        Error "Incorrect number of moves in your input. Syntax: From To (example: A3 B3)"

let createAttemptedMove (input: string) =
    input
    |> splitFullMove
    |> Result.bind (fun (start, target) ->
        match createCell start, createCell target with
        | Ok start, Ok target ->
            Ok { FromCell = start; ToCell = target }
        | Error err, _ | _, Error err ->
            Error err
    )
