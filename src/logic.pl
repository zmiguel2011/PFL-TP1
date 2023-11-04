:- dynamic 
      dynamic_player/2,
      dynamic_coords/2.

/**
 * change_turn(+GameState,-NewGameState)
 * 
 * Change player's turn
 * GameState - current gamestate(Board, Turn)
 * NewGameState - new gamestate(Board, NewTurn)
 */
change_turn(gamestate(_Board, 1), gamestate(_Board, 2)).
change_turn(gamestate(_Board, 2), gamestate(_Board, 1)).

/**
 * turn(+Turn,-Color)
 * 
 * Associate a color to a turn
 * Turn - current turn, 1 or 2
 * Color - the correspondent color, green or blue
 */
turn(1, 'GREEN').
turn(2, 'BLUE').

/**
 * is_capture(+Move)
 * 
 * Check if a move is a capture. (Capture == 1)
 */
is_capture(move(_Pawn, _NewCoords, 1)).

/**
 * green_player_turn(+GameState, +Player, +Level, -NewGameState)
 *
 * Handles green player's turn
 * GameState - current gamestate
 * Player - can be either 'H' or 'C', meaning Player and Computer, respectively
 * NewGameState - new gamestate
 */
handle_turn(gamestate(Board, Turn), Player, Level, NewGameState) :- % (HUMAN)
      turn(Turn, Color),
      format('\n------------------ PLAYER ~d (~w) -------------------\n\n', [Turn, Color]),
      display_game(gamestate(Board, Turn)),
      choose_move(gamestate(Board, Turn), Player, Level, Move),
      print_chosen_move(Move),
      move(gamestate(Board, Turn), Move, NewGameState),
      if_then_else( is_capture(Move), print_capture_coords, true ).
/**
 * game_over(+GameState, -Winner)
 *
 * Checks if the game is over after each move (else, keep playing)
 * GameState - current gamestate
 * Winner - winner of the game
 */
game_over(gamestate(Board, Winner), Winner):-
      checkVictory(Board, Winner), !,
      write('\n------------------ GAME OVER -------------------\n\n'),
      turn(Winner, Color),
      nl, format(' > Congratulations! Player ~d (~w) has won the game!', [Winner, Color]), nl.

/**
 * checkVictory(+Board, +Player)
 *
 * Checks if the player has reached their goal
 * Board - current board
 * Player - current player
 */
checkVictory(Board, 1):- is_pawn_green(Board, 1, 1).
checkVictory(Board, 2):- length(Board, Size), is_pawn_blue(Board, Size, Size).


/**
 * game_loop(+GameState, +Player1, +Player2)
 *
 * Main game function, waits for player to make a move, checks gamestate and changes turn or ends game
 * GameState - current gamestate(Board, Turn)
 * Player1 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'H' or 'C', meaning Player and Computer, respectively
 */
game_loop(GameState, Player1, Player2, Level) :- % Player 1's Turn
      handle_turn(GameState, Player1, Level, SecondGameState),
      (
            game_over(SecondGameState, _Winner); % check if the game is over
            ( 
                  ( change_turn(SecondGameState, ThirdGameState), handle_turn(ThirdGameState, Player2, Level, ForthGameState) ), % else change turn and continue the game
                        (
                              game_over(ForthGameState, _Winner); % check if the game is over
                              ( change_turn(ForthGameState, FinalGameState), game_loop(FinalGameState, Player1, Player2, Level) ) % else change turn and continue the game
                        )
            )
      ).


/**
 * start_game(+Player1, +Player2, +Size, +Level)
 *
 * Starts the game with the initial state and initiates the game loop.
 * Player1 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Player2 - can be either 'H' or 'C', meaning Player and Computer, respectively
 * Size - size of the board (between 5 and 10)
 * Level - level of the computer player (either 1 or 2)
 */
start_game(Player1, Player2, Size, Level):-
      initial_state(Size, GameState),
      retractall(dynamic_player(_,_)),
      assertz(dynamic_player(1, Player1)),
      assertz(dynamic_player(2, Player2)),
      game_loop(GameState, Player1, Player2, Level).