% Define the predicates for managing row and column input

% Predicate for managing the row input
manageRow(NewRow) :-
    repeat,
    readRow(Code),
    validateRow(Code, NewRow), !.

% Predicate for managing the column input
manageColumn(NewColumn) :-
    repeat,
    readColumn(Column),
    validateColumn(Column, NewColumn), !.

% Predicate to read the row input as char
readRow(Code) :-
    nl,
    write('  > Row:  '),
    ignore_newlines,
    read_char(Code).

% Predicate to read the column input
readColumn(Column) :-
    nl,
    write('  > Column:  '),
    ignore_newlines,
    read_integer(0, Column).

% Predicate to validate the row input
validateRow(65, 1) :-
    write('You entered character A\n').
validateRow(97, 1) :-
    write('You entered character A\n').

validateRow(66, 2) :-
    write('You entered character B\n').
validateRow(98, 2) :-
    write('You entered character B\n').

validateRow(67, 3) :-
    write('You entered character C\n').
validateRow(99, 3) :-
    write('You entered character C\n').

validateRow(68, 4) :-
    write('You entered character D\n').
validateRow(100, 4) :-
    write('You entered character D\n').

validateRow(69, 5) :-
    write('You entered character E\n').
validateRow(101, 5) :-
    write('You entered character E\n').

validateRow(70, 6) :-
    write('You entered character F\n').
validateRow(102, 6) :-
    write('You entered character F\n').

validateRow(71, 7) :-
    write('You entered character G\n').
validateRow(103, 7) :-
    write('You entered character G\n').

validateRow(72, 8) :-
    write('You entered character H\n').
validateRow(104, 8) :-
    write('You entered character H\n').

validateRow(73, 9) :-
    write('You entered character I\n').
validateRow(105, 9) :-
    write('You entered character I\n').

validateRow(74, 10) :-
    write('You entered character J\n').
validateRow(106, 10) :-
    write('You entered character J\n').

validateRow(_Other, _Row) :-
    write('ERROR: That row is not valid!\n\n'),
    fail.


% Predicate to validate the column input
validateColumn(1, 1).
validateColumn(2, 2).
validateColumn(3, 3).
validateColumn(4, 4).
validateColumn(5, 5).
validateColumn(6, 6).
validateColumn(7, 7).
validateColumn(8, 8).
validateColumn(9, 9).
validateColumn(10, 10).

validateColumn(_, _NewColumn) :-
    write('ERROR: That column is not valid!\n\n'),
    fail.