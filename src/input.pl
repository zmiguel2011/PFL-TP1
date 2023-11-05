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
 * Code - the char code correspondant to the letter row
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
 * RowCode - the code correspondant to the ASCII code of the row
 * RowInt - the integer index of the row
 */

validateRow(RowCode, RowInt) :-
    (RowCode >= 65, RowCode =< 74),
    RowInt is (RowCode - 65 + 1),
    format('You chose Row ~c\n' , RowCode).

validateRow(RowCode, RowInt) :-
    (RowCode >= 97, RowCode =< 106),
    RowInt is (RowCode - 97 + 1),
    RowCode1 is (RowCode - 97 + 65),
    format('You chose Row ~c\n' , RowCode1).

validateRow(_Other, _RowInt) :-
    write('ERROR: That row is not valid!\n\n'),
    fail.

/**
 * validateColumn(+Column)
 * 
 * Validates the column received. 
 */
validateColumn(Column):- Column >= 1, Column =< 10, format('You chose column ~d\n' , Column).
validateColumn(Column):- (Column < 5; Column > 10), !, write('\nERROR: That column is not valid!.\n\n'), fail.