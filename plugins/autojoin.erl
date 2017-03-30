-module(autojoin).
-author("Tobias Olausson").
-include("../nasbot.hrl").
-import(nasbot_utils,[get_command/1]).
-export([init/2,handle/4]).

% Initialize this modules' state
init(_Config, _BotState) -> self().

% The handle function. If we receive end of MOTD, then it's time to join
% channels
handle(Pid,_BotState, Config, Msg) -> 
  case get_command(Msg) of
    rpl_endofmotd -> joinAll(Pid, Config#config.channels); 
    _ -> ok
  end,
  {ok,autojoin,Pid}.

joinAll(Pid, Cs) -> lists:foreach(fun(C) -> Pid ! {send, "JOIN " ++ C} end, Cs).
