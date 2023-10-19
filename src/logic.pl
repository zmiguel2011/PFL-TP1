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

/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 1).
 * gamestate - current gamestate
 * pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, 1), pawn(Row,Col)):- 
      repeat,
      write('\n Please input the coords to the pawn you wish to move. (green)\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your coords : (~d, ~d)~n',[Row, Col]),
      is_pawn_green(Board, Row, Col), !,
      write('Cell is a green pawn!\n').

/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 2).
 * gamestate - current gamestate
 * pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, 2), pawn(Row,Col)):-
      repeat,
      write('\n Please input the coords to the pawn you wish to move. (blue)\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your coords : (~d, ~d)~n',[Row, Col]),
      is_pawn_blue(Board, Row, Col), !,
      write('Cell is a blue pawn!\n').


/**
 * is_pawn_green(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'green' using the predicate getValueFromMatrix (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
is_pawn_green(Board, Row, Col) :-  
      getValueFromMatrix(Board, Row, Col, green).

/**
 * is_pawn_blue(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'blue' using the predicate getValueFromMatrix (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
is_pawn_blue(Board, Row, Col) :- 
      getValueFromMatrix(Board, Row, Col, blue).

/**
 * is_empty_cell(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'empty' using the predicate getValueFromMatrix (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_empty_cell(Board, Row, Col) :- 
      getValueFromMatrix(Board, Row, Col, empty).

/**
 * is_green_goal(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'greenGoal' using the predicate getValueFromMatrix (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_green_goal(Board, Row, Col) :- 
      getValueFromMatrix(Board, Row, Col, greenGoal).

/**
 * is_blue_goal(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'blueGoal' using the predicate getValueFromMatrix (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_blue_goal(Board, Row, Col) :- % 
      getValueFromMatrix(Board, Row, Col, blueGoal).

/**
 * green_player_turn(+Gamestate, -NewGamestate)
 * Handles green player's turn
 * Gamestate - current gamestate
 * NewGamestate - new gamestate
 */
green_player_turn(Gamestate, NewGamestate) :-
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      choose_pawn(Gamestate, GreenPawn),
      % TODO: move_pawn(GreenPawn, Gamestate, NewGamestate),
      % display_game(NewGamestate).
      display_game(Gamestate). % delete after (testing)


/**
 * blue_player_turn(+Gamestate, -NewGamestate)
 * Handles blue player's turn
 * Gamestate - current gamestate
 * NewGamestate - new gamestate
 */
blue_player_turn(Gamestate, NewGamestate) :-
      write('\n------------------ PLAYER 1 (BLUE) -------------------\n\n'),
      choose_pawn(Gamestate, BluePawn),
      % TODO: move_pawn(BluePawn, Gamestate, NewGamestate),
      % display_game(NewGamestate).
      display_game(Gamestate). % delete after (testing)

/* checkGameState(gamestate): checks if the game is over after each move (else, keep playing) */
% TODO: checkGameState(gamestate(Board, P))

/**
 * game_loop(+Gamestate)
 * main game function, waits for player 1 (green) to move and then checks gamestate
 * Gamestate - current gamestate
 */
game_loop(gamestate(Board, 1)) :-
      green_player_turn(gamestate(Board, 1), NewGameState),
      /*(
            checkGameState(NewGameState);
            game_over(NewGameState)
      ),*/
      % game_loop(NewGameState).
      game_loop(gamestate(Board, 2)). % delete after (testing)

/**
 * game_loop(+Gamestate)
 * main game function, waits for player 2 (blue) to move and then checks gamestate
 * Gamestate - current gamestate
 */
game_loop(gamestate(Board, 2)) :-
      blue_player_turn(gamestate(Board, 2), NewGameState),
      /*(
            %checkGameState(NewGameState);
            game_over(NewGameState)
      ),*/
      % game_loop(NewGameState).
      game_loop(gamestate(Board, 1)). % delete after (testing)


start_game(Player1, Player2, Size):-
      initial_state(Size, InitialState),
      display_game(InitialState),
      game_loop(InitialState).