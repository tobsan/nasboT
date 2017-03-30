-module(nasbot_parser).
-author("Tobias Olausson").

-import(string, [tokens/2,chr/2,join/2,strip/2]).
-import(lists, [any/2, all/2,member/2,last/1]).
-include("nasbot.hrl").

-export([parse_config/0,parse_message/1]).

% TODO: Parses the config file config.xml, or something.
parse_config() -> ok.

% Parse the IRC format as given in RFC 1459
parse_message(Msg) ->
  [Head | Tail] = tokens(Msg," "),
  case hd(Head) of
    $: -> % In which case we should have a prefix
      [RawPrefix, RawCommand | RawParams] = [tl(Head) | Tail],
      Prefix = parse_prefix(RawPrefix),
      Command = parse_command(RawCommand),
      Params = parse_params(RawParams),
      #irc{ prefix=Prefix, command=Command, params=Params};
    _ ->
      Command = parse_command(Head),
      Params = parse_params(Tail),
      #irc{ command=Command, params=Params}
  end.

% <prefix>  ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
parse_prefix(Raw) -> 
  case member($!,Raw) of
    true -> % Divide message here
        [N | Rest] = tokens(Raw,"!@"),
        Nick = parse_nick(N),
        User = hd(Rest),
        #prefix{ nick = Nick, user = User, host = case length(Rest) > 1 of
            true -> last(Rest);
            _    -> none
        end };
    _ -> #prefix{host = Raw}
  end.

% <nick> ::= <letter> { <letter> | <number> | <special> }
parse_nick([H | T]) -> case is_alpha(H) andalso 
    all(fun(C) -> is_num(C) orelse is_alpha(C) or is_special(C) end,T) of
        true  -> {nick,[H|T]};
        false -> exit(parse_fail)
  end.

% <command> ::= <letter> { <letter> } | <number> <number> <number>
parse_command(Comm) -> 
  case (length(Comm) == 3) andalso all(fun(C) -> is_num(C) end,Comm) of
    true  -> {numeric, erlang:list_to_integer(Comm)};
    false ->
      case all(fun(Char) -> is_alpha(Char) end, Comm) of
        true  -> {alpha, Comm};
        false -> exit(parse_fail)
      end
  end.

% Is this Char in the span a-z, A-Z
is_alpha(Char) -> ((Char >= 65) and (Char =< 90)) or 
                  ((Char >= 97) and (Char =< 122)).

% Is this Char in the range 0-9
is_num(Char) -> (Char >= 48) and (Char =< 57).

% Below is a list of accepted special chars 
% (superset of that defined in RFC1459)
is_special(Char) -> member(Char,[92, 93, 94, 95, 96, $- , ${, $}]).

% Anything after : is collapsed
parse_params([]) -> [];
parse_params([(":" ++ H) | ParList]) -> [join([H | ParList], " ")];
parse_params([H | ParList])          -> [H | parse_params(ParList) ].
