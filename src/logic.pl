checkCell(Board, RowIndex, ColIndex, Expected) :-
    (Expected == greenGoal),
    (getValueFromMatrix(Board,RowIndex,ColIndex,Expected),
    write('cell is green\n'));
    (write('cell isnt green\n'),
    askCoords(Board,Expected)).

askCoords(Board, Expected):-
      manageRow(Row),
      manageColumn(Col),
      write('\n'),
      format('Your coords : (~d, ~d)~n',[Row, Col]),
      checkCell(Board,Row,Col,Expected),
      write('IT IS DESIRED PIECE\n').


choosePawn(gamestate(Board, 1)):- %  choosePawn(+gamestate) Choose pawn to move as Player 1
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl,
      format('Your coords : (~d, ~d)~n',[Row, Col]),
      isPawnGreen(Board, Row, Col),
      write('Cell is a green pawn!\n').

choosePawn(gamestate(Board, 2)):- %  choosePawn(+gamestate) Choose pawn to move as Player 2
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl,
      format('Your coords : (~d, ~d)~n',[Row, Col]),
      isPawnBlue(Board, Row, Col),
      write('Cell is a blue pawn!\n').


/* Checks if cell (Row, Column) is green using the predicate: getValueFromMatrix. */
isPawnGreen(Board, Row, Col) :- % isPawnGreen(+Board, +Row, +Col)
      getValueFromMatrix(Board, Row, Col, green).

/* Checks if cell (Row, Column) is blue using the predicate: getValueFromMatrix. */
isPawnBlue(Board, Row, Col) :- % isPawnBlue(+Board, +Row, +Col)
      getValueFromMatrix(Board, Row, Col, blue).

/* Checks if cell (Row, Column) is empty using the predicate: getValueFromMatrix. */
isEmptyCell(Board, Row, Col) :- % isEmptyCell(+Board, +Row, +Col)
      getValueFromMatrix(Board, Row, Col, empty).

startGame(Player1, Player2, Size):-
      initial_state(Size, InitialState),
      display_game(InitialState),
      choosePawn(InitialState).