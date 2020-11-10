namespace Checkers

module Initialization =
    let red_ = Some (Red, Soldier)
    let blk_ = Some (Black, Soldier)

    //takes in Row type and a list of either a checker OR an empty space (option type)  (row: Row) (pieces : (Color * Rank) option list)
    //returns a list of Cell and either checkor OR empty space (option type)            (Cell * (Color * Rank) option) list
    let createRow row pieces =
        let cells = Column.List |> List.map (fun col -> { Row = row; Column = col })
        List.zip cells pieces

    let returnGameState color gameStatus board message =
        { Board = board; ColorToMove = color; Message = message; GameStatus = gameStatus }

    let gameStateWithBlackInProgress = returnGameState Black InProgress

    let initNewGame() =
        //initialize board state
        let message = "Lets Play Checkers. Black to move."
        let (board: Board) =
            Map (   (createRow Eight    [None; red_; None; red_; None; red_; None; red_]) @
                    (createRow Seven    [red_; None; red_; None; red_; None; red_; None]) @
                    (createRow Six      [None; red_; None; red_; None; red_; None; red_]) @
                    (createRow Five     [None; None; None; None; None; None; None; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [blk_; None; blk_; None; blk_; None; blk_; None]) @
                    (createRow Two      [None; blk_; None; blk_; None; blk_; None; blk_]) @
                    (createRow One      [blk_; None; blk_; None; blk_; None; blk_; None]) )

        gameStateWithBlackInProgress board message


    let initMultipleCaptureTest() =
        let message = "Text Multiple Captures."
        let (board: Board) =
            Map (   (createRow Eight    [None; None; None; None; None; None; None; None]) @
                    (createRow Seven    [None; None; red_; None; red_; None; None; None]) @
                    (createRow Six      [None; None; None; None; None; None; None; None]) @
                    (createRow Five     [None; None; red_; None; None; None; red_; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [None; None; red_; None; None; None; None; None]) @
                    (createRow Two      [None; blk_; None; None; None; None; None; None]) @
                    (createRow One      [None; None; None; None; None; blk_; None; None]) )

        gameStateWithBlackInProgress board message

    let initWinConditionTest() =
        let message = "Text win condition"
        let (board: Board) =
            Map (   (createRow Eight    [None; None; None; None; None; None; None; None]) @
                    (createRow Seven    [None; None; None; None; None; None; None; None]) @
                    (createRow Six      [None; None; None; None; None; None; None; None]) @
                    (createRow Five     [None; None; None; None; None; None; None; None]) @
                    (createRow Four     [None; None; None; None; None; None; None; None]) @
                    (createRow Three    [None; None; None; None; None; None; None; None]) @
                    (createRow Two      [None; None; None; red_; None; None; None; None]) @
                    (createRow One      [None; None; None; None; blk_; None; None; None]) )

        gameStateWithBlackInProgress board message

