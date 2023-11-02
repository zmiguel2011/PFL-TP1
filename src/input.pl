/**
 * manageRow(-NewRow)
 *
 * Retrieves a valid row from the player.
 * NewRow - the row
 */
manageRow(NewRow) :-
    repeat,
    readRow(Code),
    validateRow(Code, NewRow), !.

/**
 * manageRow(-NewColumn)
 *
 * Retrieves a valid column from the player.
 * NewColumn - the column
 */
manageColumn(Column) :-
    repeat,
    readColumn(Column),
    validateColumn(Column), !.

/**
 * readRow(-Code)
 *
 * Read a row (letter) from the player.
 * Code - the char code correspondent to the letter row
 */
readRow(Code) :-
    nl,
    write('  > Row:  '),
    ignore_newlines,
    read_char(Code).

/**
 * readColumn(-Column)
 *
 * Read a column (integer) from the player.
 * Column - the column
 */
readColumn(Column) :-
    nl,
    write('  > Column:  '),
    ignore_newlines,
    read_integer(0, Column).

/**
 * validateRow(+RowCode, -RowInt)
 *
 * Validates and converts the row code received into an integer.
 * RowCode - the code correspondent to the ASCII code of the row
 * RowInt - the integer index of the row
 */
validateRow(65, 1) :-
    write('You chose row A\n').
validateRow(97, 1) :-
    write('You chose row A\n').

validateRow(66, 2) :-
    write('You chose row B\n').
validateRow(98, 2) :-
    write('You chose row B\n').

validateRow(67, 3) :-
    write('You chose row C\n').
validateRow(99, 3) :-
    write('You chose row C\n').

validateRow(68, 4) :-
    write('You chose row D\n').
validateRow(100, 4) :-
    write('You chose row D\n').

validateRow(69, 5) :-
    write('You chose row E\n').
validateRow(101, 5) :-
    write('You chose row E\n').

validateRow(70, 6) :-
    write('You chose row F\n').
validateRow(102, 6) :-
    write('You chose row F\n').

validateRow(71, 7) :-
    write('You chose row G\n').
validateRow(103, 7) :-
    write('You chose row G\n').

validateRow(72, 8) :-
    write('You chose row H\n').
validateRow(104, 8) :-
    write('You chose row H\n').

validateRow(73, 9) :-
    write('You chose row I\n').
validateRow(105, 9) :-
    write('You chose row I\n').

validateRow(74, 10) :-
    write('You chose row J\n').
validateRow(106, 10) :-
    write('You chose row J\n').

validateRow(_Other, _Row) :-
    write('ERROR: That row is not valid!\n\n'),
    fail.

/**
 * validateColumn(+Column)
 * 
 * Validadates the column received. 
 */
validateColumn(Column):- Column >= 1, Column =< 10, format('You chose column ~d\n' , Column).
validateColumn(Column):- (Column < 5; Column > 10), !, write('\nERROR: That column is not valid!.\n\n'), fail.