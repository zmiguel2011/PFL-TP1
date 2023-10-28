/**
 * not(+X)
 * 
 * Negates X.
 */
not(X):- X, !, fail.
not(_X).

/**
 * absolute(+X, -Y)
 * 
 * Stores in Y the aboslute value of X.
 */
absolute(X,X) :- X >= 0, !.
absolute(X,Y) :- Y is -X.

/**
 * if_then_else(If, Then, _Else)
 * 
 * Conditional execution predicate.
 */
if_then_else(If, Then, _Else):- If, !, Then. 
if_then_else(_If, _Then, Else):- Else.


/**
 * between(+L, +R, ?I)
 * 
 * If I is binded, it checks if L =< I =< R.
 * If I is not binded, it is successively assigned
 * to the integers between L and R inclusive.
 */
between(L, R, I) :- ground(I), !, L =< I, I =< R.
between(L, L, I) :- I is L, !.
between(L, R, I) :- L < R, I is L.
between(L, R, I) :- L < R, L1 is L+1, between(L1, R, I).

/**
 * getValueFromList(+List, +Index, -Value)
 * 
 * Retrieves value from List at given index.
 */
getValueFromList([Value|_T], 0, Value).
getValueFromList([_H|T], Index, Value) :-
        Index > 0,
        Index1 is Index - 1,
        getValueFromList(T, Index1, Value).

/**
 * getIndexFromRow(+Row, +Index, -Value)
 * 
 * Retrieves value from Row at given index.
 */
getIndexFromRow([Value|_], 1, Value). % We found the value
getIndexFromRow([_|T], Index, Value):-
  getIndexFromRow(T, Index1, Value), % Check in the tail of the row
  Index is Index1 + 1.  % and increment the resulting index


/**
 * getValueFromBoard(+Board, +Row, +Col, -Value)
 * 
 * Retrieves row from Board for a given Value.
 */
getIndexFromBoard([H|_T], 1, Col, Value) :-
        getIndexFromRow(H, Col, Value).

getIndexFromBoard([_Value|T], Row, Col, Value) :-
        getIndexFromBoard(T, Row1, Col, Value),
        Row is Row1 + 1.

/**
 * getValueFromRow(+Row, +Index, -Value)
 * 
 * Retrieves value from Row at given index.
 */

getValueFromRow([Value|_T], 1, Value).
getValueFromRow([_H|T], Index, Value) :-
        Index > 1,
        Index1 is Index - 1,
        getValueFromRow(T, Index1, Value).

/**
 * getValueFromBoard(+Board, +Row, +Col, -Value)
 * 
 * Retrieves value from Board at given position (Row, Col).
 */
getValueFromBoard([H|_T], 1, Col, Value) :-
        getValueFromRow(H, Col, Value).

getValueFromBoard([_H|T], Row, Col, Value) :-
        Row > 1,
        Row1 is Row - 1,
        getValueFromBoard(T, Row1, Col, Value).


/**
 * replaceInRow(+Row, +Index, -NewRow)
 * 
 * Replaces value from Row at given index and returns NewRow.
 */
replaceInRow([_H|T], 1, Value, [Value|T]).
replaceInRow([H|T], Index, Value, [H|TNew]) :-
        Index > 1,
        Index1 is Index - 1,
        replaceInRow(T, Index1, Value, TNew).

/**
 * replaceInBoard(+Board, +Row, +Col, -NewBoard)
 * 
 * Replaces value from Board at given position (Row, Col) and returns NewBoard.
 */
replaceInBoard([H|T], 1, Col,Value, [HNew|T]) :-
        replaceInRow(H, Col, Value, HNew).

replaceInBoard([H|T], Row, Col, Value, [H|TNew]) :-
        Row > 1,
        Row1 is Row - 1,
        replaceInBoard(T, Row1, Col, Value, TNew).

ignore_newlines :-
    repeat,
    peek_char(Char),  % Peek at the next character without consuming it
    ((Char == '\n', get_char(_)) ; (Char == end_of_file ; Char \= '\n')),
    !.


/**
 * orthogonal_distance(+X1, +Y1, +X2, +Y2, -D)
 * 
 * Stores in D the absolute value of the distance between (X1, Y1) and (X2, Y2)
 */
orthogonal_distance(X1, Y1, X2, Y2, D) :-
    D1 is (X1 - X2 + Y1 - Y2),
    absolute(D1, D).