namespace Checkers

module Implementation = 
    open CheckerTypes
    open CheckerValidation

    let initGame() = 
        let red_ = Some (Red, Soldier)
        let blk_ = Some (Black, Soldier)

        //takes in Row type and a list of either a checker OR an empty space (option type)  (row: Row) (pieces : (Color * Rank) option list)
        //returns a list of Cell and either checkor OR empty space (option type)            (Cell * (Color * Rank) option) list
        let createRow row pieces =
            let cells = Column.List |> List.map (fun col -> { Row = row; Column = col })
            List.zip cells pieces

        //initialize board state
        let (board: Board) =
            Map (   (createRow Eight    [None; red_; None; red_; None; red_; None; red_]) @
                    (createRow Seven    [red_; None; red_; None; red_; None; red_; None]) @
                    (createRow Six      [None; red_; None; red_; None; red_; None; red_]) @
                    (createRow Five     [None; None; None; None; None; None; None; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [blk_; None; blk_; None; blk_; None; blk_; None]) @
                    (createRow Two      [None; blk_; None; blk_; None; blk_; None; blk_]) @
                    (createRow One      [blk_; None; blk_; None; blk_; None; blk_; None]) )

        { 
            Board = board; 
            ColorToMove= Black; 
            Message = "Lets Play Checkers. Black to move." 
            GameStatus = InProgress }

    let initMultipleCaptureTest() =
        let red_ = Some (Red, Soldier)
        let blk_ = Some (Black, Soldier)

        let createRow row pieces =
            let cells = Column.List |> List.map (fun col -> { Row = row; Column = col })
            List.zip cells pieces

        let (board: Board) =
            Map (   (createRow Eight    [None; None; None; None; None; None; None; None]) @
                    (createRow Seven    [None; None; red_; None; red_; None; None; None]) @
                    (createRow Six      [None; None; None; None; None; None; None; None]) @
                    (createRow Five     [None; None; red_; None; None; None; red_; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [None; None; red_; None; None; None; None; None]) @
                    (createRow Two      [None; blk_; None; None; None; None; None; None]) @
                    (createRow One      [None; None; None; None; None; blk_; None; None]) )

        {
            Board = board;
            ColorToMove = Black;
            Message = "Test Multiple Captures." 
            GameStatus = InProgress }

    let initWinConditionTest() =
        let red_ = Some (Red, Soldier)
        let blk_ = Some (Black, Soldier)

        let createRow row pieces =
            let cells = Column.List |> List.map (fun col -> { Row = row; Column = col })
            List.zip cells pieces
        
        let (board: Board) =
            Map (   (createRow Eight    [None; None; None; None; None; None; None; None]) @
                    (createRow Seven    [None; None; None; None; None; None; None; None]) @
                    (createRow Six      [None; None; None; None; None; None; None; None]) @
                    (createRow Five     [None; None; None; None; None; None; None; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [None; None; None; None; None; None; None; None]) @
                    (createRow Two      [None; None; red_; None; None; None; None; None]) @
                    (createRow One      [None; blk_; None; None; None; None; None; None]) )

        {
            Board = board;
            ColorToMove = Black;
            Message = "Test win condition." 
            GameStatus = InProgress }


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
        let changeColor =
            match gameState.ColorToMove with
            | Black -> Red
            | Red -> Black
        match move.CaptureType with
        | Capture -> 
            if validateAdditionalCaptures gameState move then
                gameState.ColorToMove
            else 
                changeColor
        | NoCapture ->
            changeColor

    //validates the move and returns a new game state
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



