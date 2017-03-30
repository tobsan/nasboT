-module(battleship).
-author("Tobias Olausson").
-import(nasbot_utils,[get_command/1,get_message/1,get_sender/1,reply/2]).
-import(lists,[keyfind/3, seq/2]).
-export([handle/3,init/0]).

% Module for playing head to head battleship against each other.

% Initialization, no players
init() -> [].

handle(State,BotState,Msg) -> case get_command(Msg) of
    privmsg -> {ok, battleship, play(State,BotState,Msg)};
    _       -> {ok, battleship, State}
end.

play(State,BotState,Msg) -> 
    {nick,Who} = get_sender(Msg),
    What       = get_message(Msg),
    case keyfind(Who, 1, State) of
        false -> State; % Check for startcommand
        {Who,Board,Opponent} when Opponent == none ->
            % Check if message matches !start <opponent>
            ok;
        {Who,Board,Opponent} ->
            % Check if message is a shot
            ok;
        _ -> State
    end.

% A player gets an empty board and no opponent initially.
init_game(Who) -> {Who, [[ none || _ <- seq(1,10) ] || _ <- seq(1,10) ], none}.
