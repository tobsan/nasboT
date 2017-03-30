-module(backa).
-author("Tobias Olausson").
-import(nasbot_utils,[is_privmsg/1,get_message/1,reply/2]).
-import(re,[compile/2,run/2]).
-export([handle/3,init/0]).

init() -> null.

handle(_State, _BotState, Msg) ->
  case is_privmsg(Msg) andalso checkMessage(Msg) of
    true ->
        Reply = lists:nth(random:uniform(length(values())), values()),
        nasbot ! {send, reply(Msg,Reply)};
    _ -> ok
  end,
  {ok,backa,null}.

% TODO: Match swedish chars
checkMessage(Msg) -> 
    M = string:to_lower(get_message(Msg)),
    {ok,R} = compile(".*backa.*r.*!",[unicode]),
    case run(M,R) of
        {match,_} -> true;
        _         -> false
    end.

values() -> ["varför ska ja backa för dig för?!"
           ,"inte kuuken"
           ,"KOM HIT"
           ,"KOM HIT säger ja"
           ,"lugna nerej!"].
