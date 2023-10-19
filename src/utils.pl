/**
 * not(+X)
 * 
 * Negates X;
 */
not(X):- X, !, fail.
not(_X).

/**
 * if_then_else(If, Then, _Else)
 * 
 * Conditional execution predicate.
 */
if_then_else(If, Then, _Else):- If, !, Then. 
if_then_else(_If, _Then, Else):- Else.

getValueFromList([H|_T], 1, Value) :-
        Value = H.

getValueFromList([_H|T], Index, Value) :-
        Index > 1,
        Index1 is Index - 1,
        getValueFromList(T, Index1, Value).

getValueFromMatrix([H|_T], 1, Column, Value) :-
        getValueFromList(H, Column, Value).

getValueFromMatrix([_H|T], Row, Column, Value) :-
        Row > 1,
        Row1 is Row - 1,
        getValueFromMatrix(T, Row1, Column, Value).

ignore_newlines :-
    repeat,
    peek_char(Char),  % Peek at the next character without consuming it
    ((Char == '\n', get_char(_)) ; (Char == end_of_file ; Char \= '\n')),
    !.