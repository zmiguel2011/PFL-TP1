valid_move(gamestate(Board,Player),pawn(Row,Col),pawn(NewRow,NewCol)):-
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
    print_moves_list(Moves).

% Define a helper predicate to print each move in the list.
print_moves_list([]).

print_moves_list([pawn(Row, Col) | Rest]) :-
    letter(Row,Letter),
    format('> Row: ~w | Col: ~w', [Letter, Col]),
    nl,
    print_moves_list(Rest).


valid_moves(Gamestate,Pawn,ValidMoves):-
    findall(NewPawn,valid_move(Gamestate,Pawn,NewPawn),ValidMoves),
    print_moves(ValidMoves).

/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 1).
 * gamestate - current gamestate
 * pawn - used to store the choosen pawn
 */
choose_pawn(gamestate(Board, 1), pawn(Row,Col)):- 
      repeat,
      write('\n Please input the coords to the pawn you wish to move.\n'),
      manageRow(Row),
      manageColumn(Col),
      nl, format('Your coords : (~d, ~d)~n',[Row, Col]),
      if_then_else(
            not(is_pawn_green(Board, Row, Col)), % if pawn is NOT green
            (write('Invalid choice! Cell is not a green pawn!\n'), fail), % then it is an invalid choice
            write('Next player!\n') % else, proceed
      ).
/**
 * choose_pawn(+Gamestate, -Pawn)
 * Prompts player to choose a pawn to move (Player 2).
 * gamestate - current gamestate
 * pawn - used to store the choosen pawn
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
            write('Next player!\n') % else, proceed
      ).


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
      choose_pawn(Gamestate, GreenPawn),
      valid_moves(Gamestate,GreenPawn,ValidMoves),
      % TODO: move_pawn(GreenPawn, Gamestate, NewGamestate),
      % display_game(NewGamestate).
      display_game(Gamestate). % delete after (testing)


/**
 * blue_player_turn(+Gamestate, +Player, -NewGamestate)
 * Handles blue player's turn (player)
 * Gamestate - current gamestate
 * Player - can be either 'P' or 'C', meaning Player and Computer, respectively
 * NewGamestate - new gamestate
 */
blue_player_turn(Gamestate, 'P', NewGamestate) :-
      write('\n------------------ PLAYER 1 (BLUE) -------------------\n\n'),
      choose_pawn(Gamestate, BluePawn),
      % TODO: move_pawn(BluePawn, Gamestate, NewGamestate),
      % display_game(NewGamestate).
      display_game(Gamestate). % delete after (testing)

/* checkGameState(gamestate): checks if the game is over after each move (else, keep playing) */
% TODO: checkGameState(gamestate(Board, P))

/**
 * game_loop(+Gamestate, +Player1, +Player2)
 * main game function, waits for player 1 (green) to move and then checks gamestate
 * Gamestate - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 1), Player1, Player2) :-
      green_player_turn(gamestate(Board, 1), Player1, NewGameState),
      /*(
            checkGameState(NewGameState);
            game_over(NewGameState)
      ),*/
      % game_loop(NewGameState, Player1, Player2).
      game_loop(gamestate(Board, 2), Player1, Player2). % delete after (testing)

/**
 * game_loop(+Gamestate, +Player1, +Player2)
 * main game function, waits for player 2 (blue) to move and then checks gamestate
 * Gamestate - current gamestate
 * Player1 - can be either 'P' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'P' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 2), Player1, Player2) :-
      blue_player_turn(gamestate(Board, 2), Player2, NewGameState),
      /*(
            %checkGameState(NewGameState);
            game_over(NewGameState)
      ),*/
      % game_loop(NewGameState, Player1, Player2).
      game_loop(gamestate(Board, 1), Player1, Player2). % delete after (testing)


start_game(Player1, Player2, Size):-
      initial_state(Size, InitialState),
      display_game(InitialState),
      game_loop(InitialState, Player1, Player2).