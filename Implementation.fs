namespace Checkers

module Implementation = 
    open CheckerTypes
    open CheckerValidation

    //updates board by returning new board with updated piece locations
    let updateBoard (board: Board) (move: Move) =
        let isCapture = move.CaptureType
        match isCapture with
        | Capture ->
            board
                .Add((</>) move.FromCell move.ToCell, None)
                .Add(move.FromCell, None)
                .Add(move.ToCell, Some move.Piece)
        | NoCapture ->
            board
                .Add(move.FromCell, None)
                .Add(move.ToCell, Some move.Piece)

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
    //refactor
    let updateGame (currentState: GameState) (attemptedMove: AttemptedMove) = 
        let validatedState = validateEndOfGame currentState
        match validatedState.GameStatus with
        | InProgress ->
            let validatedMove = validateMove currentState attemptedMove
            match validatedMove with
            | Ok move ->
                let newState = 
                    { currentState with
                            Board = updateBoard currentState.Board move
                            ColorToMove = updatePlayerTurn currentState move
                            Message = "" }
                validateEndOfGame newState
            | Error msg ->
                { currentState with Message = msg }
        | Completed -> 
            validatedState



