namespace Checkers
open CheckerTypes

module CheckerValidation = 

    //make sure that the checker that is being moved is the correct color
    let validateCorrectColorTurn gameState (attemptedMove: AttemptedMove) : Result<Move, string> =

        let startCell = attemptedMove.FromCell
        let targetCell = attemptedMove.ToCell

        match gameState.Board.[startCell] with
        | Some (checkerColor, checkerRank) ->
            if checkerColor = gameState.ColorToMove
            then Ok { 
                Piece = (checkerColor, checkerRank); 
                FromCell = startCell; 
                ToCell = targetCell; 
                CaptureType = NoCapture }
            else Error "It's not your turn."
        | None ->
            Error "Invalid input.\nNo piece was selected to move."

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

    //run the attempted move through the validation suite
    let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
        attemptedMove
        |> validateCorrectColorTurn gameState
        |> Result.bind (validateMoveToEmptyCell gameState)
        |> Result.bind validMoveShape
        |> Result.bind (validateJumpOverPiece gameState)
