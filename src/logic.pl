 /**
 * valid_move_pawn(+GameState, +Pawn, -NewCoords) (Player 1)
 * Validate or return possible NewCoords for a given pawn
 * GameState - current gamestate
 * Pawn - the pawn given
 * NewCoords - new (possible) coordinates for the pawn given
 */
valid_move_pawn(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol),Capture):- % (Player 1)
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
            ),
            Capture is 0;
            getValueFromBoard(Board,NewRow,NewCol,blue), % if cell is blue
            (
                  (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                  (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                  (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                  (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
            ),
            Capture is 1;
            getValueFromBoard(Board,NewRow,NewCol,greenGoal), % checks if it is greenGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            ),
            Capture is 0
      ).

valid_move_pawn(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol),Capture):- % (Player 2)
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
            ),
            Capture = 0;
            getValueFromBoard(Board,NewRow,NewCol,green), % if cell is green
            (
                  (NewRow =:= Row + 1, NewCol =:= Col + 1); % Down Right
                  (NewRow =:= Row - 1, NewCol =:= Col + 1); % Up Right
                  (NewRow =:= Row + 1, NewCol =:= Col - 1); % Down Left
                  (NewRow =:= Row - 1, NewCol =:= Col - 1)  % Up Left
            ),
            Capture = 1;
            getValueFromBoard(Board,NewRow,NewCol,blueGoal), % checks if it is blueGoal
            (
                  (NewRow =:= Row + 1, NewCol =:= Col); % Right
                  (NewRow =:= Row - 1, NewCol =:= Col); % Left
                  (NewRow =:= Row, NewCol =:= Col + 1); % Down
                  (NewRow =:= Row, NewCol =:= Col - 1)  % Up
            ),
            Capture = 0
      ).

 /**
 * valid_move(+GameState, -Pawn, -NewCoords)
 * Return possible a pawn and possible NewCoords for it to move to
 * GameState - current gamestate
 * Pawn - the pawn returned
 * NewCoords - new (possible) coordinates for the pawn returned
 */
valid_move(gamestate(Board,1),pawn(Row,Col),coords(NewRow,NewCol)):- % (Player 1)
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


valid_move(gamestate(Board,2),pawn(Row,Col),coords(NewRow,NewCol)):- % (Player 2)
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
    findall((NewCoords,Capture), valid_move_pawn(GameState, Pawn, NewCoords, Capture), ListOfMoves).

/**
 * valid_moves(+GameState, +Player, -ListOfMoves)
 * Returns a list of valid moves for a given pawn.
 * GameState - current gamestate
 * Player - a player
 * ListOfMoves - the list of valid moves for the pawn given
 */
valid_moves(gamestate(Board, P), Player, ListOfMoves):-
      bagof(move(Pawn,NewCoords), valid_move(gamestate(Board, P), Pawn, NewCoords), ListOfMoves).



/**
 * green_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles green player's turn
 * GameState - current gamestate
 * Player - can be either 'H' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
green_player_turn(GameState, 'H', Level, NewGameState) :- % (HUMAN)
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(GameState),
      choose_move(GameState, 'H', _Level, Move,Capture),
      print_chosen_move(Move),
      move(GameState, Move, MoveGameState),
      if_then_else(
            Capture =:= 1,
            manage_capture(MoveGameState,'H',Level,NewGameState),
            NewGameState = MoveGameState
      ).

green_player_turn(GameState, 'C', Level, NewGameState) :- % (COMMPUTER)
      write('\n------------------ PLAYER 1 (GREEN) -------------------\n\n'),
      display_game(GameState),
      valid_moves(GameState, 'C', ListOfMoves),
      choose_move(GameState, 'C', Level, Move),
      print_chosen_move(Move),
      move(GameState, Move, NewGameState).


/**
 * blue_player_turn(+GameState, +Player, +Level, -NewGameState)
 * Handles blue player's turn
 * GameState - current gamestate
 * Player - can be either 'H' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
blue_player_turn(GameState, 'H', Level, NewGameState) :- % (HUMAN)
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(GameState),
      choose_move(GameState, 'H', _Level, Move,Capture),
      print_chosen_move(Move),
      move(GameState, Move, MoveGameState),
      if_then_else(
            Capture =:= 1,
            manage_capture(MoveGameState,'H',Level,NewGameState),
            NewGameState = MoveGameState
      ).

blue_player_turn(GameState, 'C', Level, NewGameState) :- % (COMMPUTER)
      write('\n------------------ PLAYER 2 (BLUE) -------------------\n\n'),
      display_game(GameState),
      valid_moves(GameState, 'C', ListOfMoves),
      choose_move(GameState, 'C', Level, Move),
      print_chosen_move(Move),
      move(GameState, Move, NewGameState).


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
 * Board - current board
 * Player - current player
 */
checkVictory(Board, 1):- is_pawn_green(Board, 1, 1).
checkVictory(Board, 2):- length(Board, Size), is_pawn_blue(Board, Size, Size).


/**
 * game_loop(+GameState, +Player1, +Player2)
 * Main game function, waits for player to make a move, checks gamestate and changes turn or ends game
 * GameState - current gamestate(Board, Turn)
 * Player1 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'H' or 'C', meaning Player and Computer, respectively
 */
game_loop(gamestate(Board, 1), Player1, Player2, Level) :- % Player 1's Turn
      green_player_turn(gamestate(Board, 1), Player1, Level, gamestate(NewBoard, 1)),
      (
            game_over(gamestate(NewBoard, 1), _Winner); % check is the game is over
            game_loop(gamestate(NewBoard, 2), Player1, Player2, Level) % else continue the game and change turn
      ).

game_loop(gamestate(Board, 2), Player1, Player2, Level) :- % Player 2's Turn
      blue_player_turn(gamestate(Board, 2), Player2, Level, gamestate(NewBoard, 2)),
      (
            game_over(gamestate(NewBoard, 2), _Winner); % check is the game is over
            game_loop(gamestate(NewBoard, 1), Player1, Player2, Level) % else continue the game and change turn
      ).


/**
 * start_game(+Player1, +Player2, +Size, +Level)
 * Starts the game with the initial state and initiates the game loop.
 * Player1 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Size - size of the board (between 5 and 10)
 * Level - level of the computer player (either 1 or 2)
 */
start_game(Player1, Player2, Size, Level):-
      initial_state(Size, GameState),
      game_loop(GameState, Player1, Player2, Level).