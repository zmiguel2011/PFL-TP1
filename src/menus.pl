mainMenu :-
    printMainMenu,
    askMenuOption,
    read(Input),
    manageInput(Input), !.

manageInput(1) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    startGame('P','P', Size).

manageInput(2) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    startGame('P','C', Size).

manageInput(3) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),   % backtrack to repeat
    !, % when input is valid, cut!, we won't backtrack to repeat anymore
    startGame('C','C', Size).

manageInput(4) :-
    printInstructions,
    askMenuOption,
    read(_Input),
    goBack(_Input).

manageInput(0) :-
    write('\nExiting...\n\n').
/**
 * manageInput(+_Option)
 * 
 * Manages the input received. 
 */
manageInput(_Option) :-
    write('\nERROR: that option does not exist.\n\n'),
    askMenuOption,
    read(Input),
    manageInput(Input).

goBack(_Option) :-
    mainMenu.


/**
 * validateSize(+Size)
 * 
 * Validadates the size received. 
 */
validateSize(Size):- Size >= 5, Size =< 10.

printMainMenu :-
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
    write('|                          1. Player vs Player                          |'),nl,
    write('|                                                                       |'),nl,
    write('|                          2. Player vs Computer                        |'),nl,
    write('|                                                                       |'),nl,
	write('|                          3. Computer vs Computer                      |'),nl,
    write('|                                                                       |'),nl,                                                   
    write('|                          4. Instructions                              |'),nl,
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
                                                   
                                                   


askMenuOption :-
    write('> Choose an option: ').


askBoardSize :-
    write('\n Please input the desired board size for the game. The size must be an integer between 5 and 10. (eg. 5, default is 5x5)\n'),
    write('  > Size: ').


printInstructions :-
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
    write('|            Empty cells: ( . ) --- inaccessible cells: ( - )           |'),nl,      
    write('|                                                                       |'),nl,                                                   
    write('|               -----------------------------------------               |'),nl,
    write('|                                                                       |'),nl, 
    write('|                      Enter ay key to go back.                         |'),nl,                                                  
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
