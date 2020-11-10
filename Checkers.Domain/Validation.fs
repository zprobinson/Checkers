module Checkers.Validation

//validate if game is over
let validateEndOfGame gameState =
    let findRemaining color =
        let result =
            gameState.Board
            |> Map.toList
            |> List.filter (fun (_, ch) -> ch = Some (color, Soldier) || ch = Some (color, King))
        result.Length

    let redPieces = findRemaining Red
    let blackPieces = findRemaining Black

    if redPieces = 0 then
        { gameState with
            ColorToMove = Black
            Message = "Black has won the game."
            GameStatus = Completed }
    else if blackPieces = 0 then
        { gameState with
            ColorToMove = Red
            Message = "Red has won the game."
            GameStatus = Completed }
    else gameState

//convert AttemptedMove into Move
let convertToMove gameState (attemptedMove: AttemptedMove) =
    let piece = gameState.Board.[attemptedMove.FromCell]
    match piece with
    | Some piece ->
        Ok {
            Piece = piece;
            FromCell = attemptedMove.FromCell;
            ToCell = attemptedMove.ToCell;
            CaptureType = NoCapture }
    | None ->
        Error "Invalid input.\nNo piece was selected to move."

//player turn
let validatePlayerTurn gameState move =
    let color = fst move.Piece
    let colorToMove = gameState.ColorToMove
    if color = colorToMove then
        Ok move
    else
        Error "Rules error.\nIt's not your turn."

//checkers can only move to an empty board space
let validateMoveToEmptyCell gameState move =
    let cellContent = gameState.Board.Item move.ToCell

    match cellContent with
    | Some _ ->
        Error "Rules error.\nCan't move checker on to an occupied Cell."
    | None ->
        Ok move

//validate if the move follows the rules of checkers
let validMoveShape (move: Move) =

    //refactor attempt on get distance
    let getDistance list dimension1 dimension2  =
        let start =  list |> List.findIndex (fun i -> i = dimension1)
        let target = list |> List.findIndex (fun i -> i = dimension2)
        target - start

    let startCell = move.FromCell
    let endCell = move.ToCell

    let getXDistance = getDistance Column.List
    let getYDistance = getDistance Row.List

    let x = abs (getXDistance startCell.Column endCell.Column)
    let y = getYDistance startCell.Row endCell.Row

    let (color, rank) = move.Piece

    match color with
    | Black ->
        match rank with
        | King ->
            match (x, abs y) with
            | (1, 1) -> Ok move
            | (2, 2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\nBlack King checkers can only move diagonally up/down and to the side. (1 or 2 spaces)"
        | Soldier ->
            match (x, y) with
            | (1, 1) -> Ok move
            | (2, 2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\nBlack checkers can only move diagonally up and to the side. (1 or 2 spaces)"
    | Red ->
        match rank with
        | King ->
            match (x, abs y) with
            | (1, 1) -> Ok move
            | (2, 2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\Red King checkers can only move diagonally up/down and to the side. (1 or 2 spaces)"
        | Soldier ->
            match (x, y) with
            | (1, -1) -> Ok move
            | (2, -2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\nRed checkers can only move diagonally down and to the side. (1 or 2 spaces)"

//when checkers move 2 spaces, the intermediate diagonal space must have a checker on it of opposing color
let validateJumpOverPiece gameState move =
    if move.CaptureType = NoCapture then
        Ok move
    else
        let intermediateCell = (</>) move.FromCell move.ToCell
        let checker = gameState.Board.[intermediateCell]

        match checker with
        | Some checker ->
            if gameState.ColorToMove <> fst checker
            then Ok move
            else Error "Rules error.\nCannot jump over a friendly checker."
        | None -> Error "Rules error.\nIn order to jump 2 spaces, you must capture an opposing piece."

//find 4 Cell options for any cell
let findCellOptions (start: Cell) (piece: Checker) =
    let inline (>=<) num (min, max) = num >= min && num <= max
    let findIndex list item = list |> List.findIndex (fun c -> c = item);
    let getOptions col row =
        [(col + 2, row + 2);
         (col + 2, row - 2);
         (col - 2, row + 2);
         (col - 2, row - 2)]

    let matchRankAndColor startRow endRow =
        let (color, rank) = piece
        match rank with
        | King -> true
        | _ ->
            match color with
            | Red -> startRow > endRow
            | Black -> startRow < endRow

    let startCol = findIndex Column.List start.Column
    let startRow = findIndex Row.List start.Row
    let options = getOptions startCol startRow
    options                                                                                 // take all 4 cell options
    |> List.filter (fun (col, row) -> col >=< (0, 7) && row >=< (0, 7) )                    // remove options that exceed game board boundaries
    |> List.filter (fun (col, endRow) -> matchRankAndColor startRow endRow )                // remove options based on game rules of where checker can jump to
    |> List.map (fun (col, row) -> { Column = Column.List.[col]; Row = Row.List.[row] })    // map remaining options on to a Cell list

//kings a piece
let validatePiecePromotion move =
    let color = fst move.Piece
    match color with
    | Red ->
        match move.ToCell.Row with
        | One -> Ok { move with Piece = (Red, King) }
        | _ -> Ok move
    | Black ->
        match move.ToCell.Row with
        | Eight -> Ok { move with Piece = (Black, King) }
        | _ -> Ok move

//run the attempted move through the validation suite
let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
    attemptedMove
    |> convertToMove gameState
    |> Result.bind (validatePlayerTurn gameState)
    |> Result.bind (validateMoveToEmptyCell gameState)
    |> Result.bind validMoveShape
    |> Result.bind (validateJumpOverPiece gameState)
    |> Result.bind validatePiecePromotion

//check if current piece has any additional options to take a piece
let validateAdditionalCaptures gameState move =
    let validateMoveTest moveResult =
        moveResult
        |> Result.bind (validateMoveToEmptyCell gameState)
        |> Result.bind validMoveShape
        |> Result.bind (validateJumpOverPiece gameState)

    let (color, rank) = move.Piece
    let targetCellOptions = findCellOptions move.ToCell move.Piece

    //failwithf "%A" targetCellOptions

    let resultOptions =
        targetCellOptions
        //map cell list on to attempted moves
        |> List.map (fun cell -> { FromCell = move.ToCell; ToCell = cell })
        //map attempted moves on to Moves
        |> List.map (fun attemptedMove -> { Piece = (color, rank); FromCell = attemptedMove.FromCell; ToCell = attemptedMove.ToCell; CaptureType = Capture })
        //map Moves on to Result<Move, string> using validation
        |> List.map (fun move -> validateMoveTest (Ok move))
        //remove any Moves that are invalid
        |> List.filter (fun result -> match result with | Ok _ -> true | Error _ -> false)

    //failwithf "%A" resultOptions

    //If there are no remaining valid Moves, then false, otherwise true
    match resultOptions.Length with
    | 0 -> false
    | _ -> true
