/**
 * choose_move(+GameState, +Player, +Level, -Move)
 * Chooses a move for the player to make.
 * GameState - current gamestate
 * Player - the player
 * Level - the level if the player is a computer
 * Move - move for the player to make -> Capture-move(Pawn, NewCoords), Capture indicates if it is a capture move
 */
choose_move(GameState, 'H', _Level, Capture-move(Pawn, NewCoords)):- % (HUMAN)
    choose_pawn(GameState, Pawn),
    valid_moves_pawn(GameState, Pawn, ValidMoves),
    print_moves_pawn(ValidMoves),
    choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords, Capture).

choose_move(GameState, 'C', 1, Capture-Move):- % (COMPUTER - LEVEL 1)
    valid_moves(GameState, _Player, ListOfMoves),
    choose_random_move(GameState, ListOfMoves, Move, Capture).

choose_move(GameState, 'C', 2, Capture-Move):- % (COMPUTER - LEVEL 2)
    valid_moves(GameState, _Player, ListOfMoves),
    best_move(GameState, ListOfMoves, Capture-Move, _Value).


/**
 * choose_pawn(+GameState, -Pawn)
 * Prompts player to choose a pawn to move.
 * GameState - current gamestate
 * Pawn - choosen pawn
 */
choose_pawn(gamestate(Board, 1), pawn(Row,Col)):-  % if it's Player 1 (green)
    repeat,
    write('\nPlease input the coords to the pawn you wish to move.\n'),
    manageRow(Row),
    letter(Row,Letter),
    manageColumn(Col),
    nl, format('Your pawn: (~w, ~d)~n',[Letter, Col]),
    if_then_else(      % then check if a valid green pawn was choosen
        not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
        (write('Invalid choice! Cell is not a green pawn!\n'), fail), % then it is an invalid choice
        nl % else, proceed
    ).

choose_pawn(gamestate(Board, 2), pawn(Row,Col)):-  % if it's Player 2 (blue)
    repeat,
    write('\nPlease input the coords to the pawn you wish to move.\n'),
    manageRow(Row),
    letter(Row,Letter),
    manageColumn(Col),
    nl, format('Your pawn: (~w, ~d)~n',[Letter, Col]),
    if_then_else(      % then check if a valid blue pawn was choosen
        not(is_pawn_blue(Board, Row, Col)), % if pawn is NOT blue
        (write('Invalid choice! Cell is not a blue pawn!\n'), fail), % then it is an invalid choice
        nl % else, proceed
    ).


/**
 * choose_move_pawn(+GameState, +ValidMoves, +Pawn, -NewCoords, -Capture)
 * Prompts the player to choose a move to make from the valid_moves list.
 * GameState - current gamestate
 * ValidMoves - list of valid moves
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn
 * Capture - indicates if it is a capture move
 */
choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords, Capture):-
    length(ValidMoves, L),
    L1 is L - 1,
    repeat,
    write('\n Please input the index for the move you wish to make.\n'),
    write('  > Index:  '), ignore_newlines, read_integer(0, Index),
    Index >= 0, Index =< L1,
    !,
    getValueFromList(ValidMoves, Index, Capture-NewCoords).

/**
 * choose_random_move(+GameState, +ValidMoves, -Move, -Capture)
 *
 * Chooses a random move for the bot to make from the valid_moves list.
 * Pawn - the pawn to move -> pawn(Row, Col)
 * GameState - current gamestate(Board, Turn)
 * ValidMoves - list of valid moves
 * Move - random move
 * Capture - indicates if it is a capture move
 */
choose_random_move(GameState, ValidMoves, Move, Capture):-
    length(ValidMoves, L),
    L1 is L - 1,
    repeat,
    random(0, L, Index),
    Index >= 0, Index =< L1,
    !,
    getValueFromList(ValidMoves, Index, Capture-Move).

/**
 * color(+Turn, -Pawn)
 *
 * Retrieve the color of the oponents' pawns.
 */
color(1,blue). % used in manage_capture
color(2,green). % used in manage_capture

/**
 * manage_capture(+GameState, +Player, -NewGameState)
 *
 * Manages and performs the capture of a pawn.
 * GameState - current gamestate(Board, Turn)
 * Player - the player (Human or Compute)
 * NewGameState - new gamestate(Board, Turn)
 */
manage_capture(gamestate(Board,Turn), 'H', gamestate(NewBoard,Turn)):-
    repeat,
    color(Turn,Color),
    display_game(gamestate(Board,Turn)), nl,
    write('Please input the coords where you want the captured pawn to go.\n'),
    manageRow(Row),
    manageColumn(Col),
    nl, format('Choosen Coords for the captured pawn: (~d, ~d)~n',[Row, Col]),
    if_then_else(
        is_empty_cell(Board,Row,Col),
        replaceInBoard(Board,Row,Col,Color,NewBoard),
        (
            write('Coordinates are not valid!\n'),
            fail
        )
    ).

manage_capture(gamestate(Board,Turn), 'C', gamestate(NewBoard,Turn)):-
    repeat,
    color(Turn,Color),
    display_game(gamestate(Board,Turn)), 
    nl, write('Computer chooses where the captured pawn goes.\n'),
    choose_random_empty(Board,Row,Col),
    replaceInBoard(Board,Row,Col,Color,NewBoard).


/**
 * best_move(+GameState, +ListOfMoves, -Move, -Value)
 * 
 * Choose best move from list.
 * GameState - current gamestate(Board, Turn)
 * ListOfMoves - list of valid moves
 * Move - the best move -> Capture-Move, Capture indicates if it is a capture move
 * Value - the value of the best move
 */
best_move(GameState, ListOfMoves, Capture-Move, Value) :-
    bagof(Value-Capture-Move, (member(Capture-Move, ListOfMoves), get_move_value(GameState, Move, Value)), ListOfMovesPairs),
    min_member(Value-Capture-Move, ListOfMovesPairs).


/**
 * get_move_value(+GameState, +Move, -Value)
 * 
 * Gets value of board after performing Move, and returns a value corresponding to the move made.
 * GameState - current gamestate(Board, Turn)
 * Move - valid move
 * Value - value of the move
 */
get_move_value(gamestate(Board, 1), Move, Value) :- 
    move(gamestate(Board, 1), Move, NewGameState),
    value_green(NewGameState, Value).

get_move_value(gamestate(Board, 2), Move, Value) :- 
    move(gamestate(Board, 2), Move, NewGameState),
    value_blue(NewGameState, Value).


/**
 * choose_random_pawn(+GameState, -Pawn)
 *
 * Chooses a random pawn for the bot to move. 
 * This function was implemented in the early stages of the game for testing purposes.
 * GameState - current gamestate(Board, Turn)
 * Pawn - used to store the choosen pawn
 */
choose_random_pawn(gamestate(Board, Turn), pawn(Row,Col)):- 
    repeat,
    random(1, 10, Row),
    random(1, 10, Col),
    if_then_else(
        Turn == 1,            % if P is Player 1 (green)
        if_then_else(      % then check if a valid green pawn was choosen
            not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
            fail, % then it is an invalid choice
            format('~nComputer has choosen a pawn: (~d, ~d)~n',[Row, Col]) % else, proceed
        ),
        if_then_else(      % else check if a valid blue pawn was choosen
            not(is_pawn_blue(Board, Row, Col)), % if pawn is NOT blue
            fail, %  then it is an invalid choice
            format('~nComputer has choosen a pawn: (~d, ~d)~n',[Row, Col]) % else, proceed
        )
    ).

/**
 * choose_random_empty(+Board, -Row, -Col)
 *
 * Chooses a random (empty) position for the bot to move the captured pawn. 
 * Board - current Board for the game
 * Row - random row
 * Col - random column
 */
choose_random_empty(Board, Row, Col):-
    repeat,
    random(1,10,Row),
    random(1,10,Col),
    letter(Row,Letter),
    if_then_else(
        is_empty_cell(Board,Row,Col),
        format('~nComputer has choosen a cell: (~w, ~d)~n',[Letter,Col]),
        fail
    ).

