namespace Checkers

module Initialization =
    open CheckerTypes

    let initNewGame() = 
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

