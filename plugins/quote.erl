-module(quote).
-author("Tobias Olausson").
-export([init/0,handle/3]).
-import(nasbot_utils,[is_privmsg/1,get_message/1, reply/2]).
-define(QUOTEFILE,"quotes.txt").

-record(quote,{
    date = {date(), time()},
    who  = "Unknown",
    what
}).

% Quote handling module
% The quotes are saved as erlang tuples in QUOTEFILE.
% Actually, they are saved as erlang quote records.
%
% NOTE: This plugin only supports single-line quotes.

% TODO: Make the savefile more human-readable

init() -> case file:consult(?QUOTEFILE) of
    {error,_Reason} -> [];
    {ok,Terms} -> Terms
end.

% Saves all the quotes to file
save(Quotes) ->
    {ok,Dev} = file:open(?QUOTEFILE,[write]),
    lists:foreach(fun(Q) -> io:fwrite(Dev,"~w.~n",[Q]) end, Quotes),
    file:close(Dev).

handle(Quotes,_BotState,Msg) ->
  case is_privmsg(Msg) of false -> {ok,quote,Quotes}; _ ->
  M = get_message(Msg),
  case match(M) of
    {get,Name} when Quotes /= [] ->
        Qs = if Name == any -> Quotes; 
             true ->
                {ok,R} = re:compile(".*"++string:to_lower(Name)++".*",[unicode]),
                lists:filter(fun(Q) -> 
                case re:run(string:to_lower(Q#quote.who),R) of
                {match,_} -> true; _ -> false end end,Quotes)
             end,
        case Qs of
            [] -> nasbot ! {send, reply(Msg,"no quote found")};
            _  -> Q  = lists:nth(random:uniform(length(Qs)),Qs),
                  sendQuote(Q,Msg)
        end,
        {ok, quote, Quotes};
    {add,Who,What} -> % Add a quote
        Q = #quote{ who = Who, what = What },
        % Is the below line neccessary?
        Qs = lists:filter(fun(Q) -> Q#quote.what /= What end,Quotes),
        nasbot ! {send, reply(Msg,"Quote added.")},
        save([Q | Qs]),
        {ok, quote, [Q | Qs]};
    _ -> {ok, quote, Quotes}
  end
  end.
  

match(Msg) -> 
    [Comm | Params] = string:tokens(Msg," "),
    case Comm of
        "!quote" when Params == [] -> {get,any};
        "!quote" -> {get,hd(Params)}; 
        "!addquote" when length(Params) >= 2 ->
            {add,hd(Params),string:join(tl(Params)," ")};
        _ -> nomatch
    end.

sendQuote(Q, Msg) ->
    M = "<" ++ Q#quote.who ++ "> " ++ Q#quote.what,
    nasbot ! {send, reply(Msg,M) },
    void.
