namespace Checkers
open CheckerTypes

module CheckerValidation = 
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

    //player turn
    let validatePlayerTurn gameState move =
        let (color, rank) = move.Piece
        let colorToMove = gameState.ColorToMove
        if color = colorToMove then 
            Ok move
        else 
            Error "Rules error.\nIt's not your turn."

    //make sure that the checker that is being moved is the correct color
    //let validateCorrectColorTurn gameState (attemptedMove: AttemptedMove) : Result<Move, string> =

    //    let startCell = attemptedMove.FromCell
    //    let targetCell = attemptedMove.ToCell

    //    match gameState.Board.[startCell] with
    //    | Some (checkerColor, checkerRank) ->
    //        if checkerColor = gameState.ColorToMove
    //        then Ok { 
    //            Piece = (checkerColor, checkerRank); 
    //            FromCell = startCell; 
    //            ToCell = targetCell; 
    //            CaptureType = NoCapture }
    //        else Error "It's not your turn."
    //    | None ->
    //        Error "Invalid input.\nNo piece was selected to move."

    //checkers can only move to an empty board space
    let validateMoveToEmptyCell gameState move : Result<Move, string> =
        let targetCell = move.ToCell
        let pieceOnTargetCellOpt = gameState.Board.Item targetCell

        match pieceOnTargetCellOpt with
        | Some _ ->
            Error "Rules error.\nCan't move checker on to an occupied Cell."
        | None ->
            Ok move

    //returns how many cells the move is attempting horizontally
    let getHorizontalDistance startCell targetCell =
        let xStartPos = 
            Column.List
            |> List.findIndex (fun c -> c = startCell.Column)
        let xTargetPos =
            Column.List
            |> List.findIndex (fun c -> c = targetCell.Column)
        xTargetPos - xStartPos

    //returns how many cells the move is attempting vertically
    let getVerticalDistance startCell targetCell =
        let yStartPos = 
            Row.List
            |> List.findIndex (fun r -> r = startCell.Row)
        let yTargetPos =
            Row.List
            |> List.findIndex (fun r -> r = targetCell.Row)
        yTargetPos - yStartPos

    let validMoveShape (move: Move) : Result<Move, string> =
        let (color, rank) = move.Piece

        let x = abs (getHorizontalDistance move.FromCell move.ToCell)
        let y = getVerticalDistance move.FromCell move.ToCell

        match color with
        | Black ->
            match (x, y) with
            | (1, 1) -> Ok move
            | (2, 2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\nBlack checkers can only move diagonally up and to the side. (1 or 2 spaces)"
        | Red ->
            match (x, y) with
            | (1, -1) -> Ok move
            | (2, -2) -> Ok { move with CaptureType = Capture }
            | _ -> Error "Invalid input.\nRed checkers can only move diagonally down and to the side. (1 or 2 spaces)"

    //when checkers move 2 spaces, the intermediate diagonal space must have a checker on it of opposing color
    let validateJumpOverPiece gameState move : Result<Move, string> =
        if move.CaptureType = NoCapture then
            Ok move
        else
            let intermediateCell = (</>) move.FromCell move.ToCell

            match (gameState.Board.[intermediateCell]) with
            | Some (contentColor, _) -> 
                if gameState.ColorToMove = contentColor
                then Error "Rules error.\nCannot jump over a friendly checker."
                else Ok move
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

        let matchRankAndColor startRow endRow (piece: Checker) =
            let (color, rank) = piece
            match rank with
            | King -> true
            | _ ->
                match color with
                | Red -> startRow > endRow
                | Black -> startRow < endRow

        let startColIndex = findIndex Column.List start.Column
        let startRowIndex = findIndex Row.List start.Row
        let options = getOptions startColIndex startRowIndex
        options 
        |> List.filter (fun (c, r) -> c >=< (0, 7) && r >=< (0, 7))
        |> List.filter (fun (c, r) -> matchRankAndColor startColIndex r piece)
        |> List.map (fun (c, r) -> { Column = Column.List.[c]; Row = Row.List.[r] })

    //run the attempted move through the validation suite
    let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
        attemptedMove
        |> convertToMove gameState
        |> Result.bind (validatePlayerTurn gameState)
        |> Result.bind (validateMoveToEmptyCell gameState)
        |> Result.bind validMoveShape
        |> Result.bind (validateJumpOverPiece gameState)

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
            |> List.map (fun cell -> { FromCell = move.ToCell; ToCell = cell })
            |> List.map (fun attemptedMove -> { Piece = (color, rank); FromCell = attemptedMove.FromCell; ToCell = attemptedMove.ToCell; CaptureType = Capture })
            |> List.map (fun move -> validateMoveTest (Ok move))
            |> List.filter (fun result -> match result with | Ok _ -> true | Error _ -> false)

        //failwithf "%A" resultOptions

        match resultOptions.Length with
        | 0 -> false
        | _ -> true
