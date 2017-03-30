% 
% nasbot.hrl, contains record definitions used by two or more 
% modules in the bot.
%

% Datastructure of an IRC message
-record(irc,{
    prefix = none, % May be absent
    command,
    params = []
}).

% Prefix of an IRC message
% If the prefix is a server, then host is the only field set.
-record(prefix,{
    nick = none,
    user = none,
    host = none
}).

% This is the configuration record
-record(config,{
    name    = "",
    address = "",
    port = 6667,
    nick = "nasboT",
    plugins  = [pingpong],
    channels = ["#nasbot"]
}).

-record(botstate,{
    pid,
    nick,
    channels  = [],
    plugins   = [],
    admins    = []
}).

-record(channel,{
    topic = "",
    users = [],
    mode  = ""
}).
