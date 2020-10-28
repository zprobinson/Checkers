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
        with static member (</>) (target: Cell, from: Cell) = 

                let findCellIndex list item1 item2 =
                    let result = 
                        list
                        |> List.zip [0..7]
                        |> List.filter (fun (index, item) -> item = item1 || item = item2)
                        |> List.averageBy (fun (index, item) -> (float)index)
                    (int) result

                let resultRow = findCellIndex Row.List target.Row from.Row
                let resultCol = findCellIndex Column.List target.Column from.Column

                //let resultRow : int =
                //    let result =
                //        Row.List
                //        |> List.zip [0..7]
                //        |> List.filter (fun (index, item) -> item = target.Row || item = from.Row)
                //        |> List.averageBy (fun (index, item) -> (float)index)
                //    (int) result

                //let resultCol : int = 
                //    let result = 
                //        Column.List
                //        |> List.zip [0..7]
                //        |> List.filter (fun (index, item) -> item = target.Column || item = from.Column)
                //        |> List.averageBy (fun (index, item) -> (float)index)
                //    (int) result

                { Column = Column.List.[resultCol]; Row = Row.List.[resultRow] }

    //the board is a dictionary of every cell and a checker MAYBE (some cells will be open)
    //the Cell is the key, and you will retrieve an option type of either the Checker on that space or None
    type Board = Map<Cell, Checker option>
    type GameState = { Board: Board; ColorToMove: Color; Message: string }

    //for move validation, use an AttemptedMove -> CompletedMove
    type AttemptedMove = { FromCell: Cell; ToCell: Cell }
    type PieceCapture = NoCapture | Capture
    type Move = { Piece: Checker; FromCell: Cell; ToCell: Cell; CaptureType: PieceCapture }


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

        { 
            Board = board; 
            ColorToMove= Black; 
            Message = "Lets Play Checkers. Black to move" }

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
            else Error "It's not your turn"
        | None ->
            Error "No piece was selected to move"

    //checkers can only move to an empty board space
    let validateMoveToEmptyCell gameState move : Result<Move, string> =
        let targetCell = move.ToCell
        let pieceOnTargetCellOpt = gameState.Board.Item targetCell

        match pieceOnTargetCellOpt with
        | Some _ ->
            Error "Can't move checker on to an occupied Cell"
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
        | _ -> Error "Checkers can only move diagonally and forward 1. (Black moves up and to the side. Red moves down and to the side."

    //checkers can only capture diagonal 2 spaces when capturing
    let validateCaptureShape gameState move : Result<Move, string> =
        let (checkerColor, _) = move.Piece

        let xDelta = getHorizontalDistance move.FromCell move.ToCell
        let yDelta = getVerticalDistance move.FromCell move.ToCell

        match (checkerColor, abs xDelta, yDelta) with
        | (Black, 2, 2)
        | (Red, 2, -2)
            -> Ok move
        | _ -> Error "Checkers can only capture diagonally and forward 2. (Black moves up and to the side. Red moves down and to the side."

    //when checkers move 2 spaces, the intermediate diagonal space must have a checker on it of opposing color
    let validateJumpOverPiece gameState move : Result<Move, string> =
        let intermediateCell = (</>) move.FromCell move.ToCell

        match (gameState.Board.[intermediateCell]) with
        | Some (contentColor, _) -> 
            if gameState.ColorToMove = contentColor
            then Error "Cannot jump over a friendly checker."
            else Ok { Piece = move.Piece; FromCell = move.FromCell; ToCell = move.ToCell; CaptureType = Capture }
        | None -> Error "In order to jump 2 spaces, you must capture an opposing piece."

    //run the attempted move through the validation suite
    let validateMove (gameState: GameState) (attemptedMove: AttemptedMove) =
        attemptedMove
        |> validateCorrectColorTurn gameState
        |> Result.bind (validateMoveToEmptyCell gameState)
        |> Result.bind (validateNormalMove gameState)
        //|> Result.bind (validateCaptureShape gameState)
        //|> Result.bind (validateJumpOverPiece gameState)
    
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

    let updatedPlayerTurn color =
        match color with
        | Black -> Red
        | Red -> Black
    
    //validates the move and returns a new game state
    let updateGame (currentState: GameState) (attemptedMove: AttemptedMove) = 
        let validatedMove = validateMove currentState attemptedMove
        match validatedMove with
        | Ok move ->
            { currentState with
                    Board = updateBoard currentState.Board move
                    ColorToMove = updatedPlayerTurn currentState.ColorToMove
                    Message = "" }
        | Error msg ->
            { currentState with Message = msg }



