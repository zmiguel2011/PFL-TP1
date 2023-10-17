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

startGame(Player1, Player2, Size):-
      initial_state(Size, InitialState),
      display_game(InitialState),
      askCoords(InitialBoard, 'green').