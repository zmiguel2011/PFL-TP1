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
print_moves_list([move( pawn(Row, Col), coords(NewRow, NewCol), Capture )| Rest], Index) :-
    letter(Row, Letter),
    letter(NewRow, NewLetter),
    capture(Capture,CaptureString),
    format(' ~w.  Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~w ~n', [Index, Letter, Col, NewLetter, NewCol, CaptureString]),
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
pawn_print_moves_list([ move( pawn(_Row, _Col), coords(NewRow, NewCol), Capture ) | Rest], Index) :-
    letter(NewRow,Letter),
    capture(Capture,CaptureString),
    format(' ~w. -> Row: ~w | Col: ~w ~w ~n', [Index, Letter, NewCol,CaptureString]),
    Index1 is Index + 1,
    pawn_print_moves_list(Rest, Index1).

/**
 * print_chosen_move(+Move)
 *
 * Print each move in the of valid moves.
 * Move - the move to print -> move(Pawn, NewCoords, Capture)
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn -> coords(Row, Col)
 */
print_chosen_move(move( pawn(Row, Col), coords(NewRow, NewCol), Capture )) :-
    letter(Row, Letter),
    letter(NewRow, NewLetter),
    capture(Capture,CaptureString),
    format('~n > Chosen Move - Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~w ~n', [Letter, Col, NewLetter, NewCol, CaptureString]).


/**
 * print_capture_coords(+Turn)
 *
 * Print the new coords for the captured pawn.
 */
print_capture_coords(Turn) :-
    dynamic_coords(Row, Col),
    letter(Row, Letter),
    if_then_else(
        dynamic_player(Turn, c),
        write('\nComputer is choosing where the captured pawn goes.\n'),
        true
    ),
    format('~nChoosen Coords for the captured pawn: (~w, ~d)~n',[Letter, Col]),
    retractall(dynamic_coords(_,_)).
