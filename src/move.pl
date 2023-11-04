/**
 * move(+GameState, +Move, -NewGameState)
 *
 * Validate and execute a move
 * GameState - current gamestate
 * Move - a given move -> move(Pawn, NewCoords, Capture)
 * Pawn - pawn the player choose to move
 * NewCoords - the new coordinates for the choosen pawn
 * NewGameState - new gamestate
 */
move(GameState, move(Pawn, NewCoords, 0), NewGameState):- % Normal Move
    move_pawn(GameState, Pawn, NewCoords, NewGameState).

move(gamestate(Board, Turn), move(Pawn, NewCoords, 1), NewGameState):- % Capture Move
    dynamic_player(Turn, Player),
    move_pawn(gamestate(Board, Turn), Pawn, NewCoords, MoveGameState),
    manage_capture(MoveGameState, Player, NewGameState).

/**
 * move_pawn(+GameState, +Pawn, +NewCoords, -NewGameState)
 *
 * Moves a pawn to new coordinates.
 * Pawn - the pawn to move
 * GameState - current gamestate
 * NewCoords - new coordinates for the pawn
 * NewGameState - new gamestate
 */
move_pawn(gamestate(Board, _Turn), pawn(Row, Col), coords(NewRow, NewCol), gamestate(NewBoard, _Turn)):-
    getValueFromBoard(Board, Row, Col, Value),
    replaceInBoard(Board, Row, Col, empty, Board1),
    replaceInBoard(Board1, NewRow, NewCol, Value, NewBoard). 
    