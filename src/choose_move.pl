color(1,blue).
color(2,green).

manage_capture(gamestate(Board,P), 'H', _Level, gamestate(NewBoard,P)):-
    repeat,
    color(P,Color),
    display_game(gamestate(Board,P)), nl,
    write('Please input the coords where you want the captured piece to go.\n'),
    manageRow(Row),
    manageColumn(Col),
    nl, format('Choosen Coords: (~d, ~d)~n',[Row, Col]),
    if_then_else(
        is_empty_cell(Board,Row,Col),
        replaceInBoard(Board,Row,Col,Color,NewBoard),
        fail
    ).

/**
 * choose_move(+GameState, +Player, +Level, -Move)
 * Chooses a move for the player to make.
 * GameState - current gamestate
 * Player - the player
 * Move - move for the player to make
 */
choose_move(GameState, 'H', _Level, move(Pawn, NewCoords),Capture):- % (HUMAN)
    choose_pawn(GameState, Pawn),
    valid_moves_pawn(GameState, Pawn, ValidMoves),
    print_moves_pawn(ValidMoves),
    choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords, Capture).

choose_move(GameState, 'C', 1, Move):- % (COMMPUTER - LEVEL 1)
    valid_moves(GameState, _Player, ListOfMoves),
    choose_random_move(GameState, ListOfMoves, Move).

choose_move(GameState, 'C', 2, Move):- % (COMMPUTER - LEVEL 2)
    valid_moves(GameState, _Player, ListOfMoves),
    best_move(GameState, ListOfMoves, Move, _Value).


/**
 * choose_pawn(+GameState, -Pawn)
 * Prompts player to choose a pawn to move.
 * GameState - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, P), pawn(Row,Col)):- 
    repeat,
    write('\nPlease input the coords to the pawn you wish to move.\n'),
    manageRow(Row),
    manageColumn(Col),
    nl, format('Your pawn: (~d, ~d)~n',[Row, Col]),
    if_then_else(
        P == 1,           % if P is Player 1 (green)
        if_then_else(      % then check if a valid green pawn was choosen
                not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
                (write('Invalid choice! Cell is not a green pawn!\n'), fail), % then it is an invalid choice
                nl % else, proceed
        ),
        if_then_else(      % else check if a valid blue pawn was choosen
                not(is_pawn_blue(Board, Row, Col)), % if pawn is NOT blue
                (write('Invalid choice! Cell is not a blue pawn!\n'), fail), %  then it is an invalid choice
                nl % else, proceed
        )
    ).


/**
 * choose_move_pawn(+GameState, +ValidMoves, +Pawn, -NewCoords)
 * Prompts the player to choose a move to make from the valid_moves list.
 * GameState - current gamestate
 * ValidMoves - list of valid moves
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn
 */
choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords,Capture):-
    length(ValidMoves, L),
    L1 is L - 1,
    repeat,
    write('\n Please input the index for the move you wish to make.\n'),
    write('  > Index:  '), read(Index),
    Index >= 0, Index =< L1,
    !,
    getValueFromList(ValidMoves, Index, (NewCoords,Capture)).

/**
 * choose_random_move(+GameState, +ValidMoves, -Move)
 *
 * Chooses a random move for the bot to make from the valid_moves list.
 * Pawn - the pawn to move -> pawn(Row, Col)
 * GameState - current gamestate(Board, Turn)
 * ValidMoves - list of valid moves
 * NewCoords - new coordinates for the pawn
 */
choose_random_move(GameState, ValidMoves, Move):-
    length(ValidMoves, L),
    L1 is L - 1,
    repeat,
    random(0, L, Index),
    Index >= 0, Index =< L1,
    !,
    getValueFromList(ValidMoves, Index, Move).

/**
 * best_move(+GameState, +ListOfMoves, -Move, -Value)
 * 
 * Choose best move from list.
 * GameState - current gamestate(Board, Turn)
 * ListOfMoves - list of valid moves
 * Move - the best move
 * Value - the value of the best move
 */
best_move(GameState, ListOfMoves, Move, Value) :-
    bagof(Value-Move, (member(Move, ListOfMoves), get_move_value(GameState, Move, Value)), ListOfMovesPairs),
    min_member(Value-Move, ListOfMovesPairs).


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

