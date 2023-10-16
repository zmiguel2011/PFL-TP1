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
 * board_create(+N, +I, -Board)
 * 
 * Create board with size N; creates lines after and including I. 
 */
board_create(N, I, []) :- I > N, !.
board_create(N, I, [Line|Board]) :-
    board_create_line(N, I, Line),
    I1 is I+1,
    board_create(N, I1, Board).


/**
 * board_create_line(+N, +I, -Line)
 * 
 * Create board line I in a board of width N
 */
board_create_line(N, I, Line) :- board_create_line(N, I, 1, Line).

 /**
 * board_create_line(+N, +I, +J, -Line)
 * 
 * Create board line I in a board of width N, starting in column J
 */
board_create_line(N, _I, J, []) :- J > N, !.
board_create_line(N, 1, 1, [greenGoal|Board]):-     % condition for greenGoal: top left corner which is (1,1)
    board_create_line(N, 1, 2, Board),
    !.
board_create_line(N, I, J, [blueGoal|Board]):-      % condition for blueGoal: bottom right corner which is (N,N)
    I == N, J == N,
    J1 is J+1, 
    board_create_line(N, I, J1, Board),
    !.
board_create_line(N, 1, J, [inaccessible|Board]):-  % #1 condition for inaccessible: top right corner which is (1,N)
    J == N,
    J1 is J+1, 
    board_create_line(N, 1, J1, Board),
    !.
board_create_line(N, I, 1, [inaccessible|Board]):-  % #2 condition for inaccessible: top right corner which is (N,1)
    I == N,
    board_create_line(N, I, 2, Board),
    !.
board_create_line(N, 1, J, [blue|Board]):-          % #1 condition for blue: same row as greenGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    J =< (N+1)/2,
    J1 is J+1, 
    board_create_line(N, 1, J1, Board),
    !.
board_create_line(N, I, 1, [blue|Board]):-          % #2 condition for blue: same column as greenGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    I =< (N+1)/2,
    board_create_line(N, I, 2, Board),
    !.
board_create_line(N, I, J, [green|Board]):-         % #1 condition for green: same row as blueGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    I == N, J >= (N+1)/2,
    J1 is J+1, 
    board_create_line(N, I, J1, Board),
    !.
board_create_line(N, I, J, [green|Board]):-         % #2 condition for green: same column as blueGoal and only occupying X cells (for any N, X is (N+1)/2 - 1)
    J == N, I >= (N+1)/2,
    J1 is J+1, 
    board_create_line(N, I, J1, Board),
    !.
board_create_line(N, I, J, [empty|Board]):-         % condition for empty: if none of the above cases, cell is empty
    J1 is J+1, 
    board_create_line(N, I, J1, Board),
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
initial_state(Size, gamestate(Board, 1)) :-
    board_create(Size, Board).
    %initial_board(Board).