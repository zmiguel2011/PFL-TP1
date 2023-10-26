/**
 * choose_random_pawn(+Gamestate, -Pawn)
 * Chooses a random pawn for the bot to move
 * Gamestate - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_random_pawn(gamestate(Board, P), pawn(Row,Col)):- 
    repeat,
    random(1, 10, Row),
    random(1, 10, Col),
    if_then_else(
        P == 1,            % if P is Player 1 (green)
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
 * choose_random_move(+Gamestate, +ValidMoves, -Pawn, -NewCoords)
 * Chooses a random move for the bot to make from the valid_moves list.
 * Pawn - the pawn to move -> pawn(Row, Col)
 * Gamestate - current gamestate
 * ValidMoves - list of valid moves
 * NewCoords - new coordinates for the pawn
 */
choose_random_move(Gamestate, ValidMoves, Pawn, NewCoords):-
    length(ValidMoves, L),
    L1 is L - 1,
    repeat,
    random(0, L, Index),
    Index >= 0, Index =< L1,
    !,
    getValueFromList(ValidMoves, Index, Pawn-NewCoords).