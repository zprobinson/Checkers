//namespace Checkers
//open CheckerTypes

module CheckerValidation

//    //make sure that the checker that is being moved is the correct color
//    let validateCorrectColorTurn gameState (attemptedMove: AttemptedMove) : Result<Move, string> =

//        let startCell = attemptedMove.FromCell
//        let targetCell = attemptedMove.ToCell

//        match gameState.Board.[startCell] with
//        | Some (checkerColor, checkerRank) ->
//            if checkerColor = gameState.ColorToMove
//            then Ok { 
//                Piece = (checkerColor, checkerRank); 
//                FromCell = startCell; 
//                ToCell = targetCell; 
//                CaptureType = NoCapture }
//            else Error "It's not your turn"
//        | None ->
//            Error "No piece was selected to move"

//    //checkers can only move to an empty board space
//    let validateMoveToEmptyCell gameState move : Result<Move, string> =
//        let targetCell = move.ToCell
//        let pieceOnTargetCellOpt = gameState.Board.Item targetCell

//        match pieceOnTargetCellOpt with
//        | Some _ ->
//            Error "Can't move checker on to an occupied Cell"
//        | None ->
//            Ok move

//    //returns how many cells the move is attempting horizontally
//    let getHorizontalDistance startCell targetCell =
//        let xStartPos = 
//            Column.List
//            |> List.findIndex (fun c -> c = startCell.Column)
//        let xTargetPos =
//            Column.List
//            |> List.findIndex (fun c -> c = targetCell.Column)
//        xTargetPos - xStartPos

//    //returns how many cells the move is attempting vertically
//    let getVerticalDistance startCell targetCell =
//        let yStartPos = 
//            Row.List
//            |> List.findIndex (fun r -> r = startCell.Row)
//        let yTargetPos =
//            Row.List
//            |> List.findIndex (fun r -> r = targetCell.Row)
//        yTargetPos - yStartPos

//    //checkers can only move diagonal 1 space if not capturing
//    let validateNormalMove gameState move : Result<Move, string> =
//        let (checkerColor, checkerRank) = move.Piece
//        let startCell = move.FromCell
//        let targetCell = move.ToCell

//        let xDelta = getHorizontalDistance startCell targetCell
//        let yDelta = getVerticalDistance startCell targetCell

//        match (checkerColor, abs xDelta, yDelta) with
//        | (Black, 1, 1)
//        | (Red, 1, -1) 
//            -> Ok move
//        | _ -> Error "Invalid move.\nCheckers can only move diagonally and forward 1. \n(Black moves up and to the side. Red moves down and to the side.)"

//    //checkers can only capture diagonal 2 spaces when capturing
//    let validateCaptureShape gameState move : Result<Move, string> =
//        let (checkerColor, _) = move.Piece

//        let xDelta = getHorizontalDistance move.FromCell move.ToCell
//        let yDelta = getVerticalDistance move.FromCell move.ToCell

//        match (checkerColor, abs xDelta, yDelta) with
//        | (Black, 2, 2)
//        | (Red, 2, -2)
//            -> Ok move
//        | _ -> Error "Checkers can only capture diagonally and forward 2. (Black moves up and to the side. Red moves down and to the side."

//    //when checkers move 2 spaces, the intermediate diagonal space must have a checker on it of opposing color
//    let validateJumpOverPiece gameState move : Result<Move, string> =
//        let intermediateCell = (</>) move.FromCell move.ToCell

//        match (gameState.Board.[intermediateCell]) with
//        | Some (contentColor, _) -> 
//            if gameState.ColorToMove = contentColor
//            then Error "Cannot jump over a friendly checker."
//            else Ok { Piece = move.Piece; FromCell = move.FromCell; ToCell = move.ToCell; CaptureType = Capture }
//        | None -> Error "In order to jump 2 spaces, you must capture an opposing piece."

//    //run the attempted move through the validation suite
//    let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
//        attemptedMove
//        |> validateCorrectColorTurn gameState
//        |> Result.bind (validateMoveToEmptyCell gameState)
//        |> Result.bind (validateNormalMove gameState)
   