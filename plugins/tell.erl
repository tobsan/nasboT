-module(tell).
-author("Tobias Olausson").
-import(nasbot_utils,[get_sender/1,is_privmsg/1,get_message/1,reply/2]).
-export([handle/3,init/0]).

init() -> dict:new().

handle(Dict, _BotState, Msg) -> case is_privmsg(Msg) of 
  true -> 
    case get_message(Msg) of
      "!messages"          -> send_messages(Dict,Msg);
      "!tell " ++ _WhoWhat -> add_message(Dict,Msg);
      "!ask " ++  _WhoWhat -> add_message(Dict,Msg);
      _                    -> 
        {nick,Who} = get_sender(Msg),
        case dict:is_key(Who,Dict) of
            true -> nasbot ! {send, reply(Msg,
                    Who ++ ", you have unread messages, type "
                           "/msg nasboT !messages to read")};
            _ -> ok
        end,
        {ok,tell,Dict}
    end; 
  false -> {ok,tell,Dict}
end.

% Sends messages
send_messages(Dict,Msg) ->
    {nick,Who} = get_sender(Msg),
    case catch dict:fetch(Who,Dict) of
        {'EXIT',_Reason} -> 
            nasbot ! {send, reply(Msg,Who ++ ", you have no unread messages")},
            {ok,tell,Dict};
        Messages         ->
            lists:foreach(fun({From,Mess}) ->
                M = "<" ++ From ++ "> " ++ Mess,
                nasbot ! {send, reply(Msg,M)}
            end, Messages),
            {ok,tell,dict:erase(Who,Dict)}
    end.


% Add a message to the messages dictionary.
add_message(Dict,Msg) ->
    {nick,Who} = get_sender(Msg),
    M = get_message(Msg),
    case string:tokens(M," ") of
        [_Command, ToWhom | What] ->
            NewDict = dict:update(
                ToWhom,
                fun(MS) -> [{Who,string:join(What," ")} | MS] end, 
                [{Who,string:join(What," ")}],
                Dict
            ),
            nasbot ! {send, reply(Msg, "Consider it noted")},
            {ok,tell,NewDict};
        _ -> {ok,tell,Dict}
    end.
