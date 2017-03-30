-module(negerjam).
-author("Tobias Olausson").
-import(nasbot_utils,[is_privmsg/1,get_message/1,reply/2]).
-export([handle/3,init/0]).

-define(LENNARTSSON,
    "ro raaaaa oooo raaaaaa blåneeger bbrööööö det här är björn "
    "lennartsson, sluta spela, den där förbannade negern som du håller på och "
    "rapar med fram och tillbaks yooo chouuw chouuww chouuw förbannade "
    "negerjävlar som håller på och rapar och bölar som ena jävla kalvar kan du "
    "inte sluta med det där förbannade tramset du ska hålla på i flera år och "
    "du e en jävel o sabotera för svenska folket. Jag vill inte höra någon sån "
    "där jävla dynga, fattar du det FÖR HELVETEEEE!!").

init() -> ok.

handle(_State, _BotState, Msg) ->
  case is_privmsg(Msg) andalso get_message(Msg) == "!lennartsson" of
    true ->
        Start = random:uniform(string:len(?LENNARTSSON) - 100),
        Reply = reply(Msg, string:substr(?LENNARTSSON,Start,100)),
        nasbot ! {send, Reply};
    _ -> ok
  end,
  {ok,lennartsson,null}.

