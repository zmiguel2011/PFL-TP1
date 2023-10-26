 /**
 * valid_move_pawn(+Gamestate, +Pawn, -NewCoords) (Player 1)
 * Validate or return possible NewCoords for a given pawn
 * Gamestate - current gamestate
 * Pawn - the pawn given
 * NewCoords - new (possible) coordinates for the pawn given
 */
valid_move_pawn(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol)):-
      is_pawn_green(Board, Row, Col),
      length(Board,Max), %get the max boundary of the board
      between(1,Max,NewRow), %checks if new value is within the boundary of the board
      between(1,Max,NewCol), %checks if new value is within the boundary of the board
      (
            getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            );
            getValueFromBoard(Board,NewRow,NewCol,greenGoal), % checks if it is greenGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            )
      ).

/**
 * valid_move_pawn(+Gamestate, +Pawn, -NewCoords) (Player 2)
 * Validate or return possible NewCoords for a given pawn
 * Gamestate - current gamestate
 * Pawn - the pawn given
 * NewCoords - new (possible) coordinates for the pawn given
 */
valid_move_pawn(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol)):-
      is_pawn_blue(Board, Row, Col),
      length(Board,Max), %get the max boundary of the board
      between(1,Max,NewRow), %checks if new value is within the boundary of the board
      between(1,Max,NewCol), %checks if new value is within the boundary of the board
      (
            getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            );
            getValueFromBoard(Board,NewRow,NewCol,blueGoal), % checks if it is blueGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            )
      ).

 /**
 * valid_move_pawn(+Gamestate, -Pawn, -NewCoords) (Player 1)
 * Return possible a pawn and possible NewCoords for it to move to
 * Gamestate - current gamestate
 * Pawn - the pawn returned
 * NewCoords - new (possible) coordinates for the pawn returned
 */
valid_move(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol)):-
      get_green_pawn(Board, Row, Col),
      length(Board,Max), %get the max boundary of the board
      between(1,Max,NewRow), %checks if new value is within the boundary of the board
      between(1,Max,NewCol), %checks if new value is within the boundary of the board
      (
            getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            );
            getValueFromBoard(Board,NewRow,NewCol,greenGoal), % checks if it is greenGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            )
      ).

 /**
 * valid_move_pawn(+Gamestate, -Pawn, -NewCoords) (Player 1)
 * Return possible a pawn and possible NewCoords for it to move to
 * Gamestate - current gamestate
 * Pawn - the pawn returned
 * NewCoords - new (possible) coordinates for the pawn returned
 */
valid_move(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol)):-
      get_blue_pawn(Board, Row, Col),
      length(Board,Max), %get the max boundary of the board
      between(1,Max,NewRow), %checks if new value is within the boundary of the board
      between(1,Max,NewCol), %checks if new value is within the boundary of the board
      (
            getValueFromBoard(Board,NewRow,NewCol,empty), % if cell is empty
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            );
            getValueFromBoard(Board,NewRow,NewCol,blueGoal), % checks if it is blueGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            )
      ).


/**
 * valid_moves_pawn(+Gamestate, +Pawn, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * Gamestate - current gamestate
 * Pawn - a given pawn
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves_pawn(Gamestate, Pawn, ListOfMoves):-
    findall(NewCoords, valid_move_pawn(Gamestate, Pawn, NewCoords), ListOfMoves).

/**
 * valid_moves(+Gamestate, +Player, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * Gamestate - current gamestate
 * Player - a player
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves(gamestate(Board, P), Player, ListOfMoves):-
      bagof(Pawn-NewCoords, valid_move(gamestate(Board, P), Pawn, NewCoords), ListOfMoves).


/**
 * print_moves(+ValidMoves)
 * Print each move in the list of all valid moves.
 * ValidMoves - the list of all valid moves
 */
print_moves([]) :-
    write('No valid moves available.').
    
print_moves(ValidMoves) :-
    write('\nAll Valid Moves:\n'),
    print_moves_list(ValidMoves, 0).

/**
 * print_moves_list(+ValidMoves, +Index)
 * Print each move in the list of all valid moves.
 * ValidMoves - the list of all valid moves 
 * Index - the index (in the list) to the current move being printed
 */
print_moves_list([], _Index).
print_moves_list([pawn(Row, Col)-coords(NewRow, NewCol) | Rest], Index) :-
    letter(Row,Letter),
    letter(NewRow,Letter1),
    format(' ~w.  Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~n', [Index, Letter, Col, Letter1, NewCol]),
    Index1 is Index + 1,
    print_moves_list(Rest, Index1).

/**
 * print_moves_pawn(+ValidMoves)
 * Print each move in the list of valid moves.
 * ValidMoves - the list of valid moves for a pawn given
 */
print_moves_pawn([]) :-
    write('\nNo valid moves available.\n').
    
print_moves_pawn(ValidMoves) :-
    write('\nValid Moves for selected pawn:\n'),
    pawn_print_moves_list(ValidMoves, 0).

/**
 * pawn_print_moves_list(+ValidMoves, +Index)
 * Print each move in the list of valid moves.
 * ValidMoves - the list of valid moves for a pawn given
 * Index - the index (in the list) to the current move being printed
 */
pawn_print_moves_list([], _Index).
pawn_print_moves_list([coords(Row, Col) | Rest], Index) :-
    letter(Row,Letter),
    format(' ~w. -> Row: ~w | Col: ~w ~n', [Index, Letter, Col]),
    Index1 is Index + 1,
    pawn_print_moves_list(Rest, Index1).


/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move.
 * Gamestate - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, P), pawn(Row,Col)):- 
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your pawn: (~d, ~d)~n',[Row, Col]),
      if_then_else(
            P == 1,           % if P is Player 1 (green)
            if_then_else(      % then check if a valid green pawn was choosen
                  not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
                  (write('Invalid choice! Cell is not a green pawn!\n'), fail), % then it is an invalid choice
                  nl % else, proceed
            ),
            if_then_else(      % else check if a valid blue pawn was choosen
                  not(is_pawn_blue(Board, Row, Col)), % if pawn is NOT blue
                  (write('Invalid choice! Cell is not a blue pawn!\n'), fail), %  then it is an invalid choice
                  nl % else, proceed
            )
      ).



/**
 * choose_move(+Gamestate, +Pawn, +ValidMoves, -NewCoords)
 * Prompts the player to choose a move to make from the valid_moves list.
 * Pawn - the pawn to move -> pawn(Row, Col)
 * Gamestate - current gamestate
 * ValidMoves - list of valid moves
 * NewCoords - new coordinates for the pawn
 */
choose_move(Gamestate, Pawn, ValidMoves, NewCoords):-
      length(ValidMoves, L),
      L1 is L - 1,
      repeat,
      write('\n Please input the index for the move you wish to make.\n'),
      write('  > Index:  '), read(Index),
      Index >= 0, Index =< L1,
      !,
      getValueFromList(ValidMoves, Index, NewCoords).

/**
 * move_pawn(+Gamestate, +Pawn, +NewCoords, -NewGamestate)
 * Moves a pawn to new coordinates.
 * Pawn - the pawn to move
 * Gamestate - current gamestate
 * NewCoords - new coordinates for the pawn
 * NewGamestate - new gamestate
 */
move_pawn(gamestate(Board, _P), pawn(Row, Col), coords(NewRow, NewCol), gamestate(NewBoard, _P)):-
      getValueFromBoard(Board, Row, Col, Value),
      replaceInBoard(Board, Row, Col, empty, Board1),
      replaceInBoard(Board1, NewRow, NewCol, Value, NewBoard).


/**
 * move(+Gamestate, +Move, -NewGamestate)
 * Validate and execute a move
 * Gamestate - current gamestate
 * Move - a given move -> move(Pawn, NewCoords)
 * Pawn - pawn the player choose to move
 * NewCoords - the new coordinates for the choosen pawn
 * NewGamestate - new gamestate
 */
move(Gamestate, move(Pawn, NewCoords), NewGamestate):-
      move_pawn(Gamestate, Pawn, NewCoords, NewGamestate).  


/**
 * is_pawn_green(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'green' using the predicate getValueFromBoard (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
is_pawn_green(Board, Row, Col) :-  
      getValueFromBoard(Board, Row, Col, green).

/**
 * is_pawn_blue(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'blue' using the predicate getValueFromBoard (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
is_pawn_blue(Board, Row, Col) :- 
      getValueFromBoard(Board, Row, Col, blue).

/**
 * is_empty_cell(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'empty' using the predicate getValueFromBoard (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_empty_cell(Board, Row, Col) :- 
      getValueFromBoard(Board, Row, Col, empty).

/**
 * is_green_goal(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'greenGoal' using the predicate getValueFromBoard (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_green_goal(Board, Row, Col) :- 
      getValueFromBoard(Board, Row, Col, greenGoal).

/**
 * is_blue_goal(+Board, +Row, +Col)
 * Checks if cell (Row, Column) is 'blueGoal' using the predicate getValueFromBoard (declared in utils.pl)
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
 is_blue_goal(Board, Row, Col) :- % 
      getValueFromBoard(Board, Row, Col, blueGoal).

/**
 * get_green_pawn(+Board, -Row, -Col)
 * Retrieves a green pawn from the board
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
get_green_pawn(Board, Row, Col) :-  
      getIndexFromBoard(Board, Row, Col, green).

/**
 * get_blue_pawn(+Board, -Row, -Col)
 * Retrieves a blue pawn from the board
 * Board - current board
 * Row - row to search for
 * Col - col to search for
 */
get_blue_pawn(Board, Row, Col) :-  
      getIndexFromBoard(Board, Row, Col, blue).

/**
 * green_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles green player's turn (player)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
green_player_turn(Gamestate, 'P', NewGamestate) :-
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(Gamestate),
      valid_moves(Gamestate, 'P', ListOfMoves),
      print_moves(ListOfMoves),
      choose_pawn(Gamestate, GreenPawn),
      valid_moves_pawn(Gamestate, GreenPawn, ValidMoves),
      print_moves_pawn(ValidMoves),
      choose_move(Gamestate, GreenPawn, ValidMoves, NewCoords),
      move(Gamestate, move(GreenPawn, NewCoords), NewGamestate).

/**
 * green_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles green player's turn (computer)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
green_player_turn(Gamestate, 'C', NewGamestate) :-
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(Gamestate),
      valid_moves(Gamestate, 'C', ListOfMoves),
      print_moves(ListOfMoves),
      choose_random_pawn(Gamestate, GreenPawn),
      valid_moves_pawn(Gamestate, GreenPawn, ValidMoves),
      print_moves_pawn(ValidMoves),
      choose_random_move(Gamestate, GreenPawn, ValidMoves, NewCoords),
      move(Gamestate, move(GreenPawn, NewCoords), NewGamestate).


/**
 * blue_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles blue player's turn (player)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
blue_player_turn(Gamestate, 'P', NewGamestate) :-
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(Gamestate),
      valid_moves(Gamestate, 'P', ListOfMoves),
      print_moves(ListOfMoves),
      choose_pawn(Gamestate, BluePawn),
      valid_moves_pawn(Gamestate, BluePawn, ValidMoves),
      print_moves_pawn(ValidMoves),
      choose_move(Gamestate, BluePawn, ValidMoves, NewCoords),
      move(Gamestate, move(BluePawn, NewCoords), NewGamestate).

/**
 * blue_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles blue player's turn (computer)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
blue_player_turn(Gamestate, 'C', NewGamestate) :-
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(Gamestate),
      valid_moves(Gamestate, 'P', ListOfMoves),
      print_moves(ListOfMoves),
      choose_random_pawn(Gamestate, BluePawn),
      valid_moves_pawn(Gamestate, BluePawn, ValidMoves),
      print_moves_pawn(ValidMoves),
      choose_random_move(Gamestate, BluePawn, ValidMoves, NewCoords),
      move(Gamestate, move(BluePawn, NewCoords), NewGamestate).


/**
 * game_over(+Gamestate, -Winner)
 * Checks if the game is over after each move (else, keep playing)
 * Gamestate - current gamestate
 * Winner - winner of the game
 */
game_over(gamestate(Board, Winner), Winner):-
      checkVictory(Board, Winner), !,
      nl, format(' > Congratulations! Player ~w has won the game!', Winner), nl.

/**
 * checkVictory(+Board, +Player)
 * Checks if the player has reached their goal
 * Gamestate - current gamestate
 */
checkVictory(Board, 1):- is_pawn_green(Board, 1, 1).
checkVictory(Board, 2):- length(Board, Size), is_pawn_blue(Board, Size, Size).


/**
 * game_loop(+Gamestate, +Player1, +Player2)
 * Main game function, waits for player 1 (green) to move and then checks gamestate
 * Gamestate - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 1), Player1, Player2) :-
      green_player_turn(gamestate(Board, 1), Player1, gamestate(NewBoard, 1)),
      (
            game_over(gamestate(NewBoard, 1), _Winner);
            game_loop(gamestate(NewBoard, 2), Player1, Player2)
      ).

/**
 * game_loop(+Gamestate, +Player1, +Player2)
 * main game function, waits for player 2 (blue) to move and then checks gamestate
 * Gamestate - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 2), Player1, Player2) :-
      blue_player_turn(gamestate(Board, 2), Player2, gamestate(NewBoard, 2)),
      (
            game_over(gamestate(NewBoard, 2), _Winner);
            game_loop(gamestate(NewBoard, 1), Player1, Player2)
      ).


start_game(Player1, Player2, Size):-
      initial_state(Size, Gamestate),
      game_loop(Gamestate, Player1, Player2).