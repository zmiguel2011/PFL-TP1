/**
initialBoard([
[greenGoal,blue,blue,empty,inaccessible],
[blue,empty,empty,empty,empty],
[blue,empty,empty,empty,green],
[empty,empty,empty,empty,green],
[inaccessible,empty,green,green,blueGoal]
]).

*/

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
letter(6, L) :- L='F'.
letter(7, L) :- L='G'.
letter(8, L) :- L='H'.
letter(9, L) :- L='I'.
letter(10, L) :- L='J'.


/**
 * display_game(+Size, +gamestate)
 * 
 * Prints the board. 
 */
display_game(gamestate(Board, _P)) :-
    nl,
    length(Board, Size),
    Nmax is Size+1,
    printHeader(Nmax),
    printMatrix(Board, 1, Nmax).

printHeader(Nmax):-
    write('   |'),
    printHeader1(Nmax, 1), nl,
    printDivider(Nmax, 0), nl.

printHeader1(Nmax, Nmax). % Base Case -> Nmax == N.
printHeader1(Nmax, N):-
    Nmax > N,
    write(' '),
    write(N),
    write(' |'),
    N1 is N+1,
    printHeader1(Nmax, N1).

printDivider(Nmax, Nmax). % Base Case -> Nmax == N.
printDivider(Nmax, N):-
    Nmax > N,
    write('---|'),
    N1 is N+1,
    printDivider(Nmax, N1).


printMatrix([], Nmax, Nmax). % Base Case -> N == Nmax=Size+1.
/**
 * printMatrix(+List, N, Nmax)
 * 
 * Prints a matrix with N cells of length. 
 */
printMatrix([Head|Tail], N, Nmax) :-
    letter(N, L),
    write(' '),
    write(L),
    N1 is N + 1,
    write(' | '),
    printRow(Head),
    nl, printDivider(Nmax, 0), nl,
    printMatrix(Tail, N1, Nmax).

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