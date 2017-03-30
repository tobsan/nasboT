-module(mpd).
-author("Tobias Olausson").

-import(nasbot_utils,[get_message/1,is_privmsg/1,reply/2]).
-export([handle/3,init/0]).

init() -> ok.

handle(_State,_BotState,Msg) ->
    case is_privmsg(Msg) of false -> ok; true ->
    Message = get_message(Msg),
    case string:tokens(Message," ") of
        % If we don't spawn, any messages sent to nasbot will be redirected here
        ["!np" | More]  when length(More) =< 2 -> spawn(fun() ->
            H = if More == [] -> "localhost"; true -> hd(More) end,
            C = if length(More) < 2 -> "status"; true -> lists:nth(2,More) end,
            checkHost(Msg,H,C) end);
        _ -> ok
    end end,
    {ok,mpd,null}.

% Takes care of hosts not responding/not running MPD
checkHost(Msg,Host,Command) ->
    P = self(),
    CMD = "mpc -h "++Host++" "++Command++" | head -n 1",
    spawn(fun() -> P ! os:cmd(CMD) end),
    receive
        S -> 
            {ok,R} = re:compile("(error|volume).*"),
            case re:run(S,R) of
                {match,_} -> nasbot ! {send, reply(Msg,"MPD not running")};
                _         -> nasbot ! {send, reply(Msg,"Now playing: "++ S)}
            end
    % Timeout
    after 1000 -> 
        nasbot ! {send, reply(Msg,"MPD at " ++ Host ++ " not responding")}
    end.
