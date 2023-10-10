initialBoard([
[greenGoal,empty,empty,empty,inaccessible],
[empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty],
[inaccessible,empty,empty,empty,blueGoal]
]).

symbol(greenGoal,S) :- S='G'.
symbol(blueGoal,S) :- S='B'.
symbol(empty,S) :- S='.'.
symbol(blue,S) :- S='b'.
symbol(green,S) :- S='g'.
symbol(inaccessible,S) :- S='-'.

letter(1, L) :- L='A'.
letter(2, L) :- L='B'.
letter(3, L) :- L='C'.
letter(4, L) :- L='D'.
letter(5, L) :- L='E'.

/**
 * printBoard(+List, N)
 * 
 * Prints the board. 
 */
printBoard(List) :-
    nl,
    write('   | 1 | 2 | 3 | 4 | 5 |\n'),
    write('---|---|---|---|---|---|\n'),
    printMatrix(List, 1).

printMatrix([], 6). % Base Case -> N is 6.
/**
 * printMatrix(+List, N)
 * 
 * Prints a matrix with N cells of length. 
 */
printMatrix([Head|Tail], N) :-
    letter(N, L),
    write(' '),
    write(L),
    N1 is N + 1,
    write(' | '),
    printRow(Head),
    write('\n---|---|---|---|---|---|\n'),
    printMatrix(Tail, N1).

printRow([]).  % Base Case
/**
 * printRow(+List)
 * 
 * Prints a row of the board.
 */
printRow([Head|Tail]) :-
    symbol(Head, S),
    write(S),
    write(' | '),
    printRow(Tail).