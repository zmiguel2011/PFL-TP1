mainMenu :-
    printMainMenu,
    askMenuOption,
    read(Input),
    manageInput(Input).

manageInput(1) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),
    startGame('P','P', Size), !.

manageInput(2) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),
    startGame('P','C', Size), !.

manageInput(3) :-
    repeat,
    askBoardSize,
    read(Size),
    validateSize(Size),
    startGame('C','C', Size), !.
    
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
