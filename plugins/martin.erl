-module(martin).
-author("Tobias Olausson").
-include("../nasbot.hrl").
-import(nasbot_utils,[is_privmsg/1,get_message/1,get_recipient/1,reply/2]).
-import(re,[compile/2,run/2]).
-export([handle/3,init/0]).

-define(QUOTES, 
    ["but why would anyone do that...I mean 2 plus 2 is...four?"
    ,"but how does bluescreen really work...I mean you can't wear blue?"
    ,"you say it's four but it looks more like 3.75 to me..."]).

init() -> ok.

handle(_State,_BotState,Msg) ->
  case is_privmsg(Msg) of false -> ok; true ->
  {ok,RE} = compile("!martin.*",[unicode]),
  Message = get_message(Msg), % Be agressive, no checking
  case run(Message,RE) of {match,_} -> randomize(Msg); _ -> ok end
  end,
  {ok,martin,null}.

% For convenience
randomize(Msg) -> 
  Reply = lists:nth(random:uniform(length(?QUOTES)), ?QUOTES),
  nasbot ! {send, reply(Msg,Reply)}.
