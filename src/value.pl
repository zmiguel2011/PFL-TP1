/**
 * value(+GameState, +Player, -Value).
 * 
 * Evaluates the GameState for the Player.
 * Value - positive, if the player has the advantage. Otherwise, negative (the other player has the advantage).
 */
value(GameState, 1, Value) :- % GREEN
    value_green(GameState, ValueGreen),
    value_blue(GameState, ValueBlue),
    if_then_else(
        ValueGreen < ValueBlue,
        Value = 1, % green player has the advantage
        Value = -1
    ), !.

value(GameState, 2, Value) :- % BLUE
    value_blue(GameState, ValueBlue),
    value_green(GameState, ValueGreen),
    if_then_else(
        ValueBlue < ValueGreen,
        Value = 1, % blue player has the advantage
        Value = -1
    ), !.



/**
 * value_green(+GameState, -Value).
 * 
 * Evaluates the GameState for the green player. The lower the value, the more advantage they have.
 * MinValue - distance between the closest green pawn and the green goal.
 */
value_green(GameState, MinValue) :-
    aggregate(min(Value), Pawn^value_green_pawn(GameState, Pawn, Value), MinValue).


/**
 * value_blue(+GameState, -Value).
 * 
 * Evaluates the GameState for the blue player. The lower the MinValue, the more advantage they have.
 * MinValue - distance between the closest blue pawn and the blue goal.
 */
value_blue(GameState, MinValue) :-
    aggregate(min(Value), Pawn^value_blue_pawn(GameState, Pawn, Value), MinValue).


/**
 * value_green_pawn(+GameState, +Pawn, -Value).
 * 
 * Evaluates the GameState for the green player. The lower the value, the more advantage they have.
 * Value - distance between the green pawn and the green goal (1,1).
 */
value_green_pawn(gamestate(Board, _Turn), pawn(Row, Col), Value) :-
    get_green_pawn(Board, Row, Col),
    orthogonal_distance(1, 1, Row, Col, Value).


/**
 * value_blue_pawn(+GameState, +Pawn, -Value).
 * 
 * Evaluates the GameState for the blue player. The lower the value, the more advantage they have.
 * Value - distance between the blue pawn and the blue goal (N,N).
 */
value_blue_pawn(gamestate(Board, _Turn), pawn(Row, Col), Value) :-
    get_blue_pawn(Board, Row, Col),
    length(Board, N),
    orthogonal_distance(N, N, Row, Col, Value).

    