namespace Checkers

module Implementation =
    open Validation

    //updates board by returning new board with updated piece locations
    let updateBoard (board: Board) (move: Move) =
        let isCapture = move.CaptureType
        match isCapture with
        | Capture ->
            board
            |> Map.add (move.FromCell </> move.ToCell) None
        | NoCapture ->
            board
        |> Map.add move.FromCell None
        |> Map.add move.ToCell (Some move.Piece)

    //if there was a capture, keeps the same player's turn.
    let updatePlayerTurn gameState move =
        let currentColor = gameState.ColorToMove
        let changeColor =
            match gameState.ColorToMove with
            | Black -> Red
            | Red -> Black

        match move.CaptureType with
        | Capture ->
            if validateAdditionalCaptures gameState move then
                currentColor
            else
                changeColor
        | NoCapture ->
            changeColor

    //validates the move and returns a new game state
    let updateGame (currentState: GameState) (attemptedMove: Result<AttemptedMove, string>) =
        //check if current gamestate is a completed game
        let validatedGameState = validateEndOfGame currentState
        let validateMoveCurried = validateMove validatedGameState

        match validatedGameState.GameStatus with
        | InProgress ->
            match Result.bind validateMoveCurried attemptedMove with
            | Ok move ->
                //check if last move ended the game and update message
                validateEndOfGame
                    { currentState with
                        Board = updateBoard currentState.Board move
                        ColorToMove = updatePlayerTurn currentState move
                        Message = "" }
            | Error msg ->
                { currentState with Message = msg }
        | Completed ->
            validatedGameState



