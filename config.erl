-module(config).
-author("Tobias Olausson").

-import(xmerl_xpath,[string/2]).
-import(lists,[map/2]).

-include("/usr/lib64/erlang/lib/xmerl-1.2.4/include/xmerl.hrl").
-include("nasbot.hrl").

% This module parses the config file.
-compile(export_all).

% Returns: [server]
parse_config() -> 
    {XML,_} = xmerl_scan:file("config.xml"),
    Servers = xmerl_xpath:string("/servers/*",XML),
    lists:map(fun(S) -> xml_to_record(S) end, Servers).

xml_to_record(Server) ->
    Nm      = hd(string("@name",Server)),
    A       = hd(string("address/@value",Server)),
    Address = A#xmlAttribute.value,
    Name    = Nm#xmlAttribute.value,
    Channels = map(fun(C) -> C#xmlAttribute.value end,
                  string("channels/*/@name",Server)),
    Plugins = map(fun(P) -> list_to_atom(P#xmlAttribute.value) end,
                  string("plugins/*/@name",Server)),
    N    = hd(string("nick/@value",Server)),
    Nick = N#xmlAttribute.value,
    P    = hd(string("port/@value",Server)),
    {Port,_} = string:to_integer(P#xmlAttribute.value),
    #config{ name    = Name, address     = Address,
             port    = Port, nick        = Nick,
             plugins = Plugins, channels = Channels }.
