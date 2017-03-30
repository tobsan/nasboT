-module(pingpong).
-author("Tobias Olausson").
-include("../nasbot.hrl").
-import(nasbot_utils,[get_command/1, get_params/1]).
-export([handle/4,init/2]).

-define(INTERVAL,10000).

% This plugin handles ponging pings, and pinging by itself

init(_Config, BotState) -> 
    Bot = BotState#botstate.pid,
    spawn(fun() -> 
        receive after ?INTERVAL -> pinger(0,Bot) 
    end end).

handle(State, _BotState, _Config, Msg) -> case get_command(Msg) of
    pong -> State ! pong;
    _    -> ok
  end,
  {ok,pingpong,State}.

% Makes sure the server on the other end is alive
pinger(Open,Pid) ->
    receive pong -> pinger(Open-1,Pid)
    after ?INTERVAL -> 
        if Open == 3 -> 
            io:format("Too many open PINGs, reconnecting~n",[]),
            Pid ! reconnect;
        true -> case is_process_alive(Pid) of
            true -> Pid ! {send, "PING :nasbot"},
                    pinger(Open+1,Pid);
            _ -> ok % Die
            end
        end
end.
