/**
 * valid_move_pawn(+GameState, +Pawn, -NewCoords, -Capture) (Player 1)
 * Validate or return possible NewCoords for a given pawn
 * GameState - current gamestate
 * Pawn - the pawn given
 * NewCoords - new (possible) coordinates for the pawn given
 * Capture - 1 if the move is a capture; 0 if it is a normal move
 */
valid_move_pawn(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol), 0):- % (Player 1)
    is_pawn_green(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty (NORMAL)
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        );
        getValueFromBoard(Board,NewRow,NewCol,greenGoal), % checks if it is greenGoal (NORMAL)
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        )
    ).

valid_move_pawn(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol), 1):- % (Player 1)
    is_pawn_green(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,blue), % if cell is blue (CAPTURE)
        (
                (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
        )
    ).

valid_move_pawn(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol), 0):- % (Player 2)
    is_pawn_blue(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty (NORMAL)
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        );
        getValueFromBoard(Board,NewRow,NewCol,blueGoal), % checks if it is blueGoal (NORMAL)
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        )
    ).

valid_move_pawn(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol), 1):- % (Player 2)
    is_pawn_blue(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,green), % if cell is green (CAPTURE)
        (
                (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
        )
    ).

/**
 * valid_move(+GameState, -Pawn, -NewCoords, -Capture)
 * Return possible a pawn and possible NewCoords for it to move to
 * GameState - current gamestate
 * Pawn - the pawn returned
 * NewCoords - new (possible) coordinates for the pawn returned
 * Capture - 1 if the move is a capture; 0 if it is a normal move
 */
valid_move(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol), 0):- % (Player 1)
    get_green_pawn(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        );
        getValueFromBoard(Board,NewRow,NewCol,greenGoal), % checks if it is greenGoal
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        )
    ).

valid_move(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol), 1):- % (Player 1)
    get_green_pawn(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,blue), % if cell is blue (CAPTURE)
        (
                (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
        )
    ).


valid_move(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol), 0):- % (Player 2)
    get_blue_pawn(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        );
        getValueFromBoard(Board,NewRow,NewCol,blueGoal), % checks if it is blueGoal
        (
                (NewRow =:= Row + 1, NewCol =:= Col); % Right
                (NewRow =:= Row - 1, NewCol =:= Col); % Left
                (NewRow =:= Row, NewCol =:= Col + 1); % Down
                (NewRow =:= Row, NewCol =:= Col - 1)  % Up
        )
    ).

valid_move(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol), 1):- % (Player 2)
    get_blue_pawn(Board, Row, Col),
    length(Board,Max), %get the max boundary of the board
    between(1,Max,NewRow), %checks if new value is within the boundary of the board
    between(1,Max,NewCol), %checks if new value is within the boundary of the board
    (
        getValueFromBoard(Board,NewRow,NewCol,green), % if cell is green (CAPTURE)
        (
                (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
        )
    ).

/**
 * valid_moves_pawn(+GameState, +Pawn, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * GameState - current gamestate
 * Pawn - a given pawn
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves_pawn(GameState, Pawn, ListOfMoves):-
    findall(Capture-NewCoords, valid_move_pawn(GameState, Pawn, NewCoords, Capture), ListOfMoves).

/**
 * valid_moves(+GameState, +Player, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * GameState - current gamestate
 * Player - a player
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves(GameState, Player, ListOfMoves):-
      bagof(Capture-move(Pawn,NewCoords), valid_move(GameState, Pawn, NewCoords, Capture), ListOfMoves).

