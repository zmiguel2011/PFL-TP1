 /**
 * valid_move_pawn(+GameState, +Pawn, -NewCoords) (Player 1)
 * Validate or return possible NewCoords for a given pawn
 * GameState - current gamestate
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
 * valid_move_pawn(+GameState, +Pawn, -NewCoords) (Player 2)
 * Validate or return possible NewCoords for a given pawn
 * GameState - current gamestate
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
 * valid_move_pawn(+GameState, -Pawn, -NewCoords) (Player 1)
 * Return possible a pawn and possible NewCoords for it to move to
 * GameState - current gamestate
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
 * valid_move_pawn(+GameState, -Pawn, -NewCoords) (Player 1)
 * Return possible a pawn and possible NewCoords for it to move to
 * GameState - current gamestate
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
 * valid_moves_pawn(+GameState, +Pawn, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * GameState - current gamestate
 * Pawn - a given pawn
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves_pawn(GameState, Pawn, ListOfMoves):-
    findall(NewCoords, valid_move_pawn(GameState, Pawn, NewCoords), ListOfMoves).

/**
 * valid_moves(+GameState, +Player, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * GameState - current gamestate
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
    letter(Row, Letter),
    letter(NewRow, NewLetter),
    format(' ~w.  Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~n', [Index, Letter, Col, NewLetter, NewCol]),
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
 * print_chosen_move(+Pawn, +NewCoords)
 * Print each move in the of valid moves.
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn -> coords(Row, Col)
 */
 print_chosen_move(pawn(Row, Col), coords(NewRow, NewCol)) :-
      letter(Row, Letter),
      letter(NewRow, NewLetter),
      format('~n > Chosen Move - Row: ~w | Col: ~w  ->  NewRow: ~w | NewCol: ~w ~n', [Letter, Col, NewLetter, NewCol]).



/**
 * choose_pawn(+GameState, -Pawn)
 * Prompts player to choose a pawn to move.
 * GameState - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, P), pawn(Row,Col)):- 
      repeat,
      write('\nPlease input the coords to the pawn you wish to move.\n'),
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
 * choose_move(+GameState, +Player, +Level, -Move)
 * Chooses a move for the player to make.
 * GameState - current gamestate
 * Player - the player
 * Move - move for the player to make
 */
choose_move(GameState, 'P', _Level, move(Pawn, NewCoords)):- % (PLAYER)
      choose_pawn(GameState, Pawn),
      valid_moves_pawn(GameState, Pawn, ValidMoves),
      print_moves_pawn(ValidMoves),
      choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords).


/**
 * choose_move(+GameState, +Player, +Level, -Move)
 * Chooses a move for the bot to make based on the level chosen. (LEVEL 1)
 * GameState - current gamestate
 * Player - the player
 * Move - move for the player to make
 */
choose_move(GameState, 'C', 1, move(Pawn, NewCoords)):- % (COMMPUTER)
    valid_moves(GameState, Player, ListOfMoves),
    choose_random_move(GameState, ListOfMoves, Pawn, NewCoords).


/**
 * choose_move(+GameState, +Player, +Level, -Move)
 * Chooses a move for the bot to make based on the level chosen. (LEVEL 2)
 * GameState - current gamestate
 * Player - the player
 * Move - move for the player to make
 */
 /* TODO: BOT LEVEL 2
 choose_move(GameState, Player, 2, move(Pawn, NewCoords)):- % (COMMPUTER)
    valid_moves(GameState, Player, ListOfMoves),
*/


/**
 * choose_move_pawn(+GameState, +ValidMoves, +Pawn, -NewCoords)
 * Prompts the player to choose a move to make from the valid_moves list.
 * GameState - current gamestate
 * ValidMoves - list of valid moves
 * Pawn - the pawn to move -> pawn(Row, Col)
 * NewCoords - new coordinates for the pawn
 */
choose_move_pawn(GameState, ValidMoves, Pawn, NewCoords):-
      length(ValidMoves, L),
      L1 is L - 1,
      repeat,
      write('\n Please input the index for the move you wish to make.\n'),
      write('  > Index:  '), read(Index),
      Index >= 0, Index =< L1,
      !,
      getValueFromList(ValidMoves, Index, NewCoords).

/**
 * move_pawn(+GameState, +Pawn, +NewCoords, -NewGameState)
 * Moves a pawn to new coordinates.
 * Pawn - the pawn to move
 * GameState - current gamestate
 * NewCoords - new coordinates for the pawn
 * NewGameState - new gamestate
 */
move_pawn(gamestate(Board, _P), pawn(Row, Col), coords(NewRow, NewCol), gamestate(NewBoard, _P)):-
      getValueFromBoard(Board, Row, Col, Value),
      replaceInBoard(Board, Row, Col, empty, Board1),
      replaceInBoard(Board1, NewRow, NewCol, Value, NewBoard).


/**
 * move(+GameState, +Move, -NewGameState)
 * Validate and execute a move
 * GameState - current gamestate
 * Move - a given move -> move(Pawn, NewCoords)
 * Pawn - pawn the player choose to move
 * NewCoords - the new coordinates for the choosen pawn
 * NewGameState - new gamestate
 */
move(GameState, move(Pawn, NewCoords), NewGameState):-
      move_pawn(GameState, Pawn, NewCoords, NewGameState).  


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
 * green_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles green player's turn (player)
 * GameState - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
green_player_turn(GameState, 'P', Level, NewGameState) :- % (PLAYER)
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(GameState),
      choose_move(GameState, 'P', _Level, move(Pawn, NewCoords)),
      print_chosen_move(Pawn, NewCoords),
      move(GameState, move(Pawn, NewCoords), NewGameState).

/**
 * green_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles green player's turn (computer)
 * GameState - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
green_player_turn(GameState, 'C', Level, NewGameState) :- % (COMMPUTER)
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(GameState),
      valid_moves(GameState, 'C', ListOfMoves),
      choose_move(GameState, 'C', Level, move(Pawn, NewCoords)),
      print_chosen_move(Pawn, NewCoords),
      move(GameState, move(Pawn, NewCoords), NewGameState).


/**
 * blue_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles blue player's turn (player)
 * GameState - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
blue_player_turn(GameState, 'P', Level, NewGameState) :- % (PLAYER)
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(GameState),
      choose_move(GameState, 'P', _Level, move(Pawn, NewCoords)),
      print_chosen_move(Pawn, NewCoords),
      move(GameState, move(Pawn, NewCoords), NewGameState).

/**
 * blue_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles blue player's turn (computer)
 * GameState - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
blue_player_turn(GameState, 'C', Level, NewGameState) :- % (COMMPUTER)
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(GameState),
      valid_moves(GameState, 'C', ListOfMoves),
      choose_move(GameState, 'C', Level, move(Pawn, NewCoords)),
      print_chosen_move(Pawn, NewCoords),
      move(GameState, move(Pawn, NewCoords), NewGameState).


/**
 * game_over(+GameState, -Winner)
 * Checks if the game is over after each move (else, keep playing)
 * GameState - current gamestate
 * Winner - winner of the game
 */
game_over(gamestate(Board, Winner), Winner):-
      checkVictory(Board, Winner), !,
      write('\n------------------ GAME OVER -------------------\n\n'),
      nl, format(' > Congratulations! Player ~w has won the game!', Winner), nl.

/**
 * checkVictory(+Board, +Player)
 * Checks if the player has reached their goal
 * GameState - current gamestate
 */
checkVictory(Board, 1):- is_pawn_green(Board, 1, 1).
checkVictory(Board, 2):- length(Board, Size), is_pawn_blue(Board, Size, Size).


/**
 * game_loop(+GameState, +Player1, +Player2)
 * Main game function, waits for player 1 (green) to move and then checks gamestate
 * GameState - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 1), Player1, Player2, Level) :-
      green_player_turn(gamestate(Board, 1), Player1, Level, gamestate(NewBoard, 1)),
      (
            game_over(gamestate(NewBoard, 1), _Winner);
            game_loop(gamestate(NewBoard, 2), Player1, Player2, Level)
      ).

/**
 * game_loop(+GameState, +Player1, +Player2)
 * main game function, waits for player 2 (blue) to move and then checks gamestate
 * GameState - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 2), Player1, Player2, Level) :-
      blue_player_turn(gamestate(Board, 2), Player2, Level, gamestate(NewBoard, 2)),
      (
            game_over(gamestate(NewBoard, 2), _Winner);
            game_loop(gamestate(NewBoard, 1), Player1, Player2, Level)
      ).


start_game(Player1, Player2, Size, Level):-
      initial_state(Size, GameState),
      game_loop(GameState, Player1, Player2, Level).