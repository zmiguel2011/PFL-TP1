valid_move(gamestate(Board,Player),pawn(Row,Col),coords(NewRow,NewCol)):-
      length(Board,Max), %get the max boundary of the board
      between(1,Max,NewRow), %checks if new value is within the boundary of the board
      between(1,Max,NewCol), %checks if new value is within the boundary of the board
      getValueFromBoard(Board,NewRow,NewCol,empty),
      (
        (NewRow =:= Row + 1, NewCol =:= Col); % Right
        (NewRow =:= Row - 1, NewCol =:= Col); % Left
        (NewRow =:= Row, NewCol =:= Col + 1); % Down
        (NewRow =:= Row, NewCol =:= Col - 1)  % Up
      ).

% Define a predicate to format and print the list of moves.
print_moves([]) :-
    write('No valid moves available.').
    
print_moves(Moves) :-
    write('Valid Moves:'),
    nl,
    print_moves_list(Moves, 0).

% Define a helper predicate to print each move in the list.
print_moves_list([], _Index).

print_moves_list([coords(Row, Col) | Rest], Index) :-
    letter(Row,Letter),
    format(' > ~w. Row: ~w | Col: ~w', [Index, Letter, Col]), nl,
    Index1 is Index + 1,
    print_moves_list(Rest, Index1).


/**
 * valid_moves(+Gamestate, +Pawn, -ValidMoves)
 * Returns a list of valid moves gor a given pawn.
 * Gamestate - current gamestate
 * Pawn - a given pawn
 * ValidMoves - the list of valid moves for the pawn given
 */
valid_moves(Gamestate,Pawn,ValidMoves):-
    findall(NewCoords,valid_move(Gamestate,Pawn,NewCoords),ValidMoves),
    print_moves(ValidMoves).

/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 1).
 * Gamestate - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, 1), pawn(Row,Col)):- 
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your pawn: (~d, ~d)~n',[Row, Col]),
      if_then_else(
            not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
            (write('Invalid choice! Cell is not a green pawn!\n'), fail), % then it is an invalid choice
            nl % else, proceed
      ).
/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 2).
 * Gamestate - current gamestate
 * Pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, 2), pawn(Row,Col)):-
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your coords : (~d, ~d)~n',[Row, Col]),
      if_then_else(
            not(is_pawn_blue(Board, Row, Col)), % if pawn is NOT blue
            (write('Invalid choice! Cell is not a blue pawn!\n'), fail), %  then it is an invalid choice
            nl % else, proceed
      ).


/**
 * make_move(+Gamestate, +Pawn, +ValidMoves, -NewGamestate)
 * Prompts the player to choose a move to make.
 * Pawn - the pawn to movepawn(Row, Col)
 * Gamestate - current gamestate
 * ValidMoves - list of valid moves
 * NewGamestate - new gamestate
 */
make_move(Gamestate, Pawn, ValidMoves, NewGamestate):-
      repeat,
      write('\n Please input the index for the move you wish to make.\n'),
      length(ValidMoves, L),
      L1 is L - 1,
      read(Index),
      Index >= 0, Index =< L1,
      !,
      getValueFromList(ValidMoves, Index, Coords),
      move_pawn(Gamestate, Pawn, Coords, NewGamestate).    


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
 * green_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles green player's turn (player)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
green_player_turn(Gamestate, 'P', NewGamestate) :-
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(Gamestate),
      choose_pawn(Gamestate, GreenPawn),
      valid_moves(Gamestate, GreenPawn, ValidMoves),
      make_move(Gamestate, GreenPawn, ValidMoves, NewGamestate).
      %display_game(Gamestate). % delete after (testing)


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
      choose_pawn(Gamestate, BluePawn),
      valid_moves(Gamestate, BluePawn, ValidMoves),
      make_move(Gamestate, BluePawn, ValidMoves, NewGamestate).
      %display_game(Gamestate). % delete after (testing)


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
      initial_state(Size, InitialState),
      display_game(InitialState),
      game_loop(InitialState, Player1, Player2).