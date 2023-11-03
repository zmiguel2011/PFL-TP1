/**
 * print_moves(+ValidMoves)
 *
 * Print each move in the list of all valid moves.
 * ValidMoves - the list of all valid moves
 */
print_moves([]) :-
    write('No valid moves available.').
    
print_moves(ValidMoves) :-
    write('\nAll Valid Moves:\n'),
    print_moves_list(ValidMoves, 0).

/**
 * print_moves_list(+ValidMoves, +Index) 
 *
 * Print each move in the list of all valid moves.
 * This function was implemented in the early stages of the game for testing purposes.
 * ValidMoves - the list of all valid moves 
 * Index - the index (in the list) to the current move being printed
 */
print_moves_list([], _Index).
print_moves_list([_Capture-move( pawn(Row, Col), coords(NewRow, NewCol) )| Rest], Index) :-
    letter(Row, Letter),
    letter(NewRow, NewLetter),
    format(' ~w.  Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~n', [Index, Letter, Col, NewLetter, NewCol]),
    Index1 is Index + 1,
    print_moves_list(Rest, Index1).

/**
 * print_moves_pawn(+ValidMoves)
 *
 * Print each move in the list of valid moves.
 * ValidMoves - the list of valid moves for a pawn given
 */
print_moves_pawn([]) :-
    write('\nNo valid moves available.\n').
    
print_moves_pawn(ValidMoves) :-
    write('\nValid Moves for selected pawn:\n'),
    pawn_print_moves_list(ValidMoves, 0).


/**
 * capture(+Capture, -String)
 * 
 * Associate a capture move to a string
 * Capture - 1 if the move is a capture, 0 otherwise
 * String - the string returned
 */
capture(0,' ').
capture(1,'<- Capture').

/**
 * pawn_print_moves_list(+ValidMoves, +Index)
 *
 * Print each move in the list of valid moves.
 * ValidMoves - the list of valid moves for a pawn given
 * Index - the index (in the list) to the current move being printed
 */
pawn_print_moves_list([], _Index).
pawn_print_moves_list([Capture-coords(Row, Col) | Rest], Index) :-
    letter(Row,Letter),
    capture(Capture,CaptureString),
    format(' ~w. -> Row: ~w | Col: ~w ~w ~n', [Index, Letter, Col,CaptureString]),
    Index1 is Index + 1,
    pawn_print_moves_list(Rest, Index1).

/**
 * print_chosen_move(+Move)
 *
 * Print each move in the of valid moves.
 * Move - the move to print -> move(Pawn, NewCoords)
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn -> coords(Row, Col)
 */
print_chosen_move(move( pawn(Row, Col), coords(NewRow, NewCol) )) :-
    letter(Row, Letter),
    letter(NewRow, NewLetter),
    format('~n > Chosen Move - Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~n', [Letter, Col, NewLetter, NewCol]).

