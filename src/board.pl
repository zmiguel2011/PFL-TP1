/**
 * board_create(-Board)
 * 
 * Create board with size 5. 
 */
default_board_create(Board) :- board_create(5, Board).

/**
 * board_create(+N, -Board)
 * 
 * Create board with size N.
 */
board_create(N, Board) :-
    board_create(N, 1, Board).

/**
 * board_create(+N, +R, -Board)
 * 
 * Create board with size N; creates rows after and including index I. 
 */
board_create(N, I, []) :- I > N, !.
board_create(N, I, [Row|Board]) :-
    board_create_row(N, I, Row),
    I1 is I+1,
    board_create(N, I1, Board).


/**
 * board_create_row(+N, +I, -Row)
 * 
 * Create board row I (index) in a board of width N
 */
board_create_row(N, I, Row) :- board_create_row(N, I, 1, Row).

 /**
 * board_create_row(+N, +I, +J, -Row)
 * 
 * Create board row I in a board of width N, starting in column J
 */
board_create_row(N, _I, J, []) :- J > N, !.
board_create_row(N, 1, J, [blue|Board]):-          % #1 condition for blue: same row as greenGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    J =< (N+1)/2,
    J1 is J+1, 
    board_create_row(N, 1, J1, Board),
    !.
board_create_row(N, I, 1, [blue|Board]):-          % #2 condition for blue: same column as greenGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    I =< (N+1)/2,
    board_create_row(N, I, 2, Board),
    !.
board_create_row(N, N, J, [green|Board]):-         % #1 condition for green: same row as blueGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    J >= (N+1)/2,
    J1 is J+1, 
    board_create_row(N, N, J1, Board),
    !.
board_create_row(N, I, N, [green|Board]):-         % #2 condition for green: same column as blueGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    I >= (N+1)/2,
    J1 is N+1, 
    board_create_row(N, I, J1, Board),
    !.
board_create_row(N, I, J, [empty|Board]):-         % condition for empty: if none of the above cases, cell is empty
    J1 is J+1, 
    board_create_row(N, I, J1, Board),
    !.


/**
 * initial_board(-Board)
 * 
 * Get initial board layout
 */
initial_board(Board) :-
    default_board_create(Board).

/**
 * initial_state(+Size, -GameState) 
 * gameState(Board, Turn)
 * 
 * Sets up initial game state.
 */
initial_state(Size, gamestate(Board, _P)) :-
    board_create(Size, Board1),                             % creates board with given Size and fills blue, green and empty cells
    replaceInBoard(Board1, 1, 1, greenGoal, Board2),        % updates greenGoal cell 
    replaceInBoard(Board2, Size, Size, blueGoal, Board3),   % updates blueGoal cell 
    replaceInBoard(Board3, 1, Size, inaccessible, Board4),  % updates inaccessible cell #1
    replaceInBoard(Board4, Size, 1, inaccessible, Board).   % updates inaccessible cell #2
    %initial_board(Board).