/**
 * main_menu
 * 
 * Presents main menu to the user and manages his actions.
 */
main_menu :-
    print_main_menu,
    choose_menu_option,
    ignore_newlines,
    read_integer(0, Input),
    manage_input(Input), !.

/**
 * manage_input(+Option)
 * 
 * Manages the input received. 
 */
manage_input(1) :-
    repeat,
    choose_board_size,
    ignore_newlines,
    read_integer(0, Size),
    validate_size(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    start_game(h,h, Size, _).

manage_input(2) :-
    repeat,
    choose_board_size,
    ignore_newlines,
    read_integer(0, Size),
    validate_size(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    repeat,
    display_choose_level,
    nl, write(' > Level: '), 
    ignore_newlines,
    read_integer(0, Level),
    validate_level(Level),  % backtrack to repeat
    !,  % when input is valid, cut!, we won't backtrack to repeat anymore
    start_game(h,c, Size, Level).

manage_input(3) :-
    repeat,
    choose_board_size,
    ignore_newlines,
    read_integer(0, Size),
    validate_size(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    repeat,
    display_choose_level,
    nl, write(' > Level: '), 
    ignore_newlines,
    read_integer(0, Level),
    validate_level(Level),  % backtrack to repeat
    !,  % when input is valid, cut!, we won't backtrack to repeat anymore
    start_game(c,h, Size, Level).

manage_input(4) :-
    repeat,
    choose_board_size,
    ignore_newlines,
    read_integer(0, Size),
    validate_size(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    repeat,
    display_choose_level,
    nl, write(' > Level: '), 
    ignore_newlines,
    read_integer(0, Level),
    validate_level(Level),  % backtrack to repeat
    !,  % when input is valid, cut!, we won't backtrack to repeat anymore
    start_game(c,c, Size, Level).

manage_input(5) :-
    print_instructions,
    choose_menu_option,
    ignore_newlines,
    read_char(_Input), !,
    main_menu.

manage_input(0) :-
    write('\nExiting...\n\n').

manage_input(_Option) :-
    write('\nERROR: that option does not exist.\n\n'),
    choose_menu_option,
    ignore_newlines,
    read_integer(0, Input),
    manage_input(Input), !.


/**
 * validate_size(+Size)
 * 
 * Validates the size received. 
 */
validate_size(Size):- Size >= 5, Size =< 10.
validate_size(Size):- (Size < 5; Size > 10), !, write('\nERROR: Size is invalid.\n\n'), fail.


/**
 * validate_level(+Level)
 * 
 * Validates the level received. 
 */
validate_level(Level):- Level >= 1, Level =< 2.
validate_level(Level):- (Level < 1; Level > 2), !, write('\nERROR: Level is invalid.\n\n'), fail.

/**
 * print_main_menu
 * 
 * Prints the main menu.
 */
print_main_menu :-
    nl,nl,
    write(' _______________________________________________________________________ '),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|             _____  _       ___  _   _ _____ ___________ _____         |'),nl,
    write('|            /  __ \\| |     / _ \\| | | /  ___|_   _| ___ \\  _  |        |'),nl,
    write('|            | /  \\/| |    / /_\\ \\ | | \\ `--.  | | | |_/ / | | |        |'),nl,
    write('|            | /  \\/| |    / /_\\ \\ | | \\ `--.  | | | |_/ / | | |        |'),nl,
    write('|            | \\__/\\| |____| | | | |_| /\\__/ / | | | |\\ \\\\ \\_/ /        |'),nl,
    write('|             \\____/\\_____/\\_| |_/\\___/\\____/  \\_/ \\_| \\_|\\___/         |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                           Jose Miguel Isidro                          |'),nl,
    write('|                           Jose Antonio Costa                          |'),nl,
    write('|               -----------------------------------------               |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                          1. Human vs Human                            |'),nl,
    write('|                                                                       |'),nl,
    write('|                          2. Human vs Computer                         |'),nl,
    write('|                                                                       |'),nl,
	write('|                          3. Computer vs Human                         |'),nl,
    write('|                                                                       |'),nl,
	write('|                          4. Computer vs Computer                      |'),nl,
    write('|                                                                       |'),nl,                                                   
    write('|                          5. Instructions                              |'),nl,
    write('|                                                                       |'),nl,
    write('|                          0. Exit                                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write(' _______________________________________________________________________ '),nl,nl,nl.

%  _____  _       ___  _   _ _____ ___________ _____ 
% /  __ \| |     / _ \| | | /  ___|_   _| ___ \  _  |
% | /  \/| |    / /_\ \ | | \ `--.  | | | |_/ / | | |
% | |    | |    |  _  | | | |`--. \ | | |    /| | | |
% | \__/\| |____| | | | |_| /\__/ / | | | |\ \\ \_/ /
%  \____/\_____/\_| |_/\___/\____/  \_/ \_| \_|\___/ 
                                                   
                                                   
/**
 * choose_menu_option
 * 
 * Requests user to choose an option from the menu.
 */
choose_menu_option :-
    write('> Choose an option: ').

/**
 * choose_board_size
 * 
 * Requests user to choose a size for the board.
 */
choose_board_size :-
    write('\n Please input the desired board size for the game. The size must be an integer between 5 and 10. (eg. 5, default is 5x5)\n'),
    write('  > Size: ').


/*
 * display_choose_level
 *
 * Displays computer A.I. levels.
*/
display_choose_level :-
    nl,
    format("1. Level 1~n", []),
    format("2. Level 2~n", []),
    format("0. Back~n~n", []).

/**
 * print_instructions
 * 
 * Prints the instructions of the game.
 */
print_instructions :-
    nl,nl,
    write(' _______________________________________________________________________ '),nl,
    write('|                                                                       |'),nl,
    write('|                Claustro is an abstract two player game,               |'),nl,
    write('|                with super simple rules like many others,              |'),nl,
    write('|               but it has a twist on the capture mechanism             |'),nl,
    write('|               that makes it engaging, thoughtful and fresh.           |'),nl,
    write('|                                                                       |'),nl,                                                   
    write('|                Players alternatively take turns moving one            |'),nl,
    write('|              square orthogonally towards the goal or capture          |'),nl,
    write('|            opponent\'s pawn diagonally (in all four directions),       |'),nl,
    write('|     a captured piece must be immediately placed back on the board     |'),nl,
    write('|                  on any unoccupied square by the                      |'),nl,
    write('|              capturing player before passing the turn.                |'),nl,                                                   
    write('|    The first player that walks a pawn in their goal wins the game.    |'),nl,  
    write('|     Player 1 plays with green pawns and Player 2 with blue pawns.     |'),nl, 
    write('|                                                                       |'),nl,                                                   
    write('|               -----------------------------------------               |'),nl,  
    write('|                              Symbols:                                 |'),nl,      
    write('|               Green pawns: ( g ) --- Blue pawns: ( b )                |'),nl,
    write('|                Green goal: ( G ) --- Blue goal: ( B )                 |'),nl,      
    write('|            Empty cells: (  ) --- inaccessible cells: ( - )            |'),nl,      
    write('|                                                                       |'),nl,                                                   
    write('|               -----------------------------------------               |'),nl,
    write('|                                                                       |'),nl, 
    write('|                     Enter any key to go back.                         |'),nl,                                                  
    write('|                                                                       |'),nl,                                                   
    write(' _______________________________________________________________________ '),nl,nl,nl.




/*   Instructions

Claustro is an abstract two player game, with super simple rules like many others, 
but it has a twist on the capture mechanism that makes it engaging, thoughtful and fresh.

Players alternatively take turns moving one square orthogonally
towards the goal or capture opponent's pawn diagonally (in all four directions),
a captured piece must be immediately placed back on the board 
on any unoccupied square by the capturing player before passing the turn. 
The first player that walks a pawn in their goal wins the game.

*/
