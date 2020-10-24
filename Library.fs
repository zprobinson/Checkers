namespace Checkers

module CheckerTypes =
    //Sets properties of any individual Checker
    type Color = Red | Black    //each checker has a color
    type Rank = Soldier | King  //each checker is either normal or king
    type Checker = Color * Rank

    
    (*  Defines layout of the board
        8 [ ] [X] [ ] [X] [ ] [X] [ ] [X]
        7 [X] [ ] [X] [ ] [X] [ ] [X] [ ]
        6 [ ] [X] [ ] [X] [ ] [X] [ ] [X]
        5 [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ]
        4 [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ]
        3 [O] [ ] [O] [ ] [O] [ ] [O] [ ]
        2 [ ] [O] [ ] [O] [ ] [O] [ ] [O]
        1 [O] [ ] [O] [ ] [O] [ ] [O] [ ]
           A   B   C   D   E   F   G   H
    *)  
    //A column can be any of the following values, and only these values
    type Column = A | B | C | D | E | F | G | H
        with static member List = [A;B;C;D;E;F;G;H]
    //A row can be any of the following values, and only these values
    type Row = One | Two | Three | Four | Five | Six | Seven | Eight
        with static member List = [One; Two; Three; Four; Five; Six; Seven; Eight]
    type Cell = { Column: Column; Row: Row }   //each cell consists of both a row and column

    //the board is a dictionary of every cell and a checker MAYBE (some cells will be open)
    //the Cell is the key, and you will retrieve an option type of either the Checker on that space or None
    type Board = Map<Cell, Checker option>
    type GameState = { Board: Board; ColorToMove: Color; Message: string }

    //for move validation, use an AttemptedMove -> CompletedMove
    type AttemptedMove = { FromCell: Cell; ToCell: Cell }
    type Move = { Piece: Checker; FromCell: Cell; ToCell: Cell }

    // TODO Implement HasCaptured
    type HasCaptured = HasCaptured | HasNotCaptured //checks to see if checker can move again


module Implementation = 
    open CheckerTypes

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

        { Board = board; ColorToMove= Black; Message = "Lets Play Checkers. Black to move" }

    //make sure that the checker that is being moved is the correct color
    let validateCorrectColorTurn gameState (attemptedMove: AttemptedMove) : Result<Move, string> =

        let startCell = attemptedMove.FromCell
        let targetCell = attemptedMove.ToCell
        let checkerToMove = gameState.Board.[startCell]

        match checkerToMove with
        | Some (checkerColor, checkerRank) ->
            if checkerColor = gameState.ColorToMove
            then Ok { Piece = (checkerColor, checkerRank); FromCell = startCell; ToCell = targetCell }
            else Error "It's not your turn"
        | None ->
            Error "No piece was selected to move"

    //checkers can only move to an empty board space
    let validateMoveToEmptyCell gameState move : Result<Move, string> =
        let startCell = move.FromCell
        let targetCell = move.ToCell
        let pieceOnTargetCellOpt = gameState.Board.Item targetCell

        match pieceOnTargetCellOpt with
        | Some _ ->
            Error "Can't move checker on to an occupied Cell"
        | None ->
            Ok { Piece = move.Piece; FromCell = move.FromCell; ToCell = move.ToCell }

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
    
    //checkers can only move diagonal 1 space if not capturing
    let validateNormalMove gameState move : Result<Move, string> =
        let (checkerColor, checkerRank) = move.Piece
        let startCell = move.FromCell
        let targetCell = move.ToCell

        let xDelta = getHorizontalDistance startCell targetCell
        let yDelta = getVerticalDistance startCell targetCell

        match (checkerColor, abs xDelta, yDelta) with
        | (Black, 1, 1)
        | (Red, 1, -1) 
            -> Ok move
        | _ -> Error "Checkers can only move diagonally and forward. (Black moves up and to the side. Red moves down and to the side."

    // TODO Try to validate EITHER a normal move OR a capture move.

