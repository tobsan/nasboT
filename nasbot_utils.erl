-module(nasbot_utils).
-include("nasbot.hrl").
-compile(export_all).

%
% Basic functions, use them
%
get_prefix(Msg) when is_record(Msg,irc) -> Msg#irc.prefix;
get_prefix(_Msg) -> exit(not_irc).

get_params(Msg) when is_record(Msg,irc) -> Msg#irc.params;
get_params(_Msg) -> exit(not_irc).

% Who sent this message? Retreives the nick, or server
get_sender(Msg) ->
    Prefix = get_prefix(Msg),
    if Prefix#prefix.nick == none -> Prefix#prefix.host;
    true -> Prefix#prefix.nick
end.

% Retrieve the host
get_host(Msg) -> 
    P = get_prefix(Msg),
    P#prefix.host.

% Convenient for those not bothering to match over all cases
is_nickchange(Msg) -> get_command(Msg) == nick.
is_ping(Msg)    -> get_command(Msg) == ping.
is_quit(Msg)    -> get_command(Msg) == quit.
is_join(Msg)    -> get_command(Msg) == join.
is_part(Msg)    -> get_command(Msg) == part.
is_privmsg(Msg) -> get_command(Msg) == privmsg.

% Who is the recipient of a privmsg
get_recipient(Msg) -> 
  case is_privmsg(Msg) of
    true -> hd(get_params(Msg));
    _    -> exit(not_privmsg)
  end.

% Was this privmsg sent to a channel?
is_to_channel(Msg) -> 
  case hd(get_recipient(Msg)) of
    $# -> true;
    $& -> true;
    _  -> false
  end.

% What was sent in this privmsg?
get_message(Msg) ->
  case is_privmsg(Msg) of
    true -> lists:last(get_params(Msg));
    _    -> exit(not_privmsg)
  end.

% Reply to this privmsg
% If it was to a channel, send to the channel
% If it was in private, send to the sender
reply(Msg, Text) -> 
  case is_privmsg(Msg) of
    true  -> 
      Rep = case is_to_channel(Msg) of
        true  -> get_recipient(Msg);
        false -> {_,S} = get_sender(Msg), S
      end,
      "PRIVMSG " ++ Rep ++ " :" ++ Text;
    false -> exit(not_privmsg)
  end.

% What command was sent?
get_command(Msg) when is_record(Msg,irc) -> case Msg#irc.command of
  {alpha,Comm} -> 
    case Comm of
      "JOIN"    -> join;
      "NICK"    -> nick;
      "MODE"    -> mode;
      "NOTICE"  -> notice;
      "PART"    -> part;
      "PING"    -> ping;
      "PONG"    -> pong;
      "PRIVMSG" -> privmsg;
      "TOPIC"   -> topic;
      "QUIT"    -> quit;
      "ERROR"   -> error
      % More extensive list here please...
    end;
  {numeric,Num} -> 
    case Num of
        % Client-server communication 001-099
        001 -> rpl_welcome;
        002 -> rpl_yourhost;
        003 -> rpl_created;
        004 -> rpl_myinfo;
        005 -> rpl_bounce;
        % Response codes 200-399
        200 -> rpl_tracelink;
        201 -> rpl_traceconnecting;
        202 -> rpl_tracehandshake;
        203 -> rpl_traceunknwn;
        204 -> rpl_traceoperator;
        205 -> rpl_traceuser;
        206 -> rpl_traceserver;
        207 -> rpl_traceservice;
        208 -> rpl_tracenewtype;
        209 -> rpl_traceclass;
        210 -> rpl_tracereconnect; % Unused
        211 -> rpl_statslinkinfo;
        212 -> rpl_statscommands;
        213 -> rpl_statscline; % Reserved
        214 -> rpl_statsnline; % Reserved
        215 -> rpl_statsiline; % Reserved
        216 -> rpl_statskline; % Reserved
        217 -> rpl_statsqline; % Reserved
        218 -> rpl_statsyline; % Reserved
        219 -> rpl_endofstats;
        221 -> rpl_umodeis;
        231 -> rpl_serviceinfo;   % Reserved
        232 -> rpl_endofservices; % Reserved
        233 -> rpl_service;       % Reserved
        234 -> rpl_servlist;
        235 -> rpl_servlistend;
        240 -> rpl_statsvline; % Reserved
        241 -> rpl_statslline; % Reserved
        242 -> rpl_statsuptime;
        243 -> rpl_statsoline;
        244 -> rpl_statshline; % Reserved
        245 -> rpl_statssline; % Reserved
        246 -> rpl_statsping;  % Reserved
        247 -> rpl_statsbline; % Reserved
        250 -> rpl_statsdline; % Reserved
        251 -> rpl_luserclient;
        252 -> rpl_luserop;
        253 -> rpl_luserunknown;
        254 -> rpl_luserchannels;
        255 -> rpl_luserme;
        256 -> rpl_adminme;
        257 -> rpl_adminloc1;
        258 -> rpl_adminloc2;
        259 -> rpl_adminemail;
        261 -> rpl_tracelog;
        262 -> rpl_traceend;
        263 -> rpl_tryagain;
        300 -> rpl_none; % Reserved
        301 -> rpl_away;
        302 -> rpl_userhost;
        303 -> rpl_ison;
        305 -> rpl_unaway;
        306 -> rpl_nowaway;
        311 -> rpl_whoisuser;
        312 -> rpl_whoisserver;
        313 -> rpl_whoisoperator;
        314 -> rpl_whowasuser;
        315 -> rpl_endofwho;
        316 -> rpl_whoischanop; % Reserved
        317 -> rpl_whoisidle;
        318 -> rpl_endofwhois;
        319 -> rpl_whoischannels;
        321 -> rpl_liststart; % Obsolete, Not used
        322 -> rpl_list;
        323 -> rpl_listend;
        324 -> rpl_channelmodeis;
        325 -> rpl_uniqopis;
        331 -> rpl_notopic;
        332 -> rpl_topic;
        341 -> rpl_inviting;
        342 -> rpl_summoning;
        346 -> rpl_invitelist;
        347 -> rpl_endofinvitelist;
        348 -> rpl_exceptlist;
        349 -> rpl_endofexceptlist;
        351 -> rpl_version;
        352 -> rpl_whoreply;
        353 -> rpl_namreply;
        361 -> rpl_killdone; % Reserved
        362 -> rpl_closing;  % Reserved
        363 -> rpl_closeend; % Reserved
        364 -> rpl_links;
        365 -> rpl_endoflinks;
        366 -> rpl_endofnames;
        367 -> rpl_banlist;
        368 -> rpl_endofbanlist;
        369 -> rpl_endofwhowas;
        371 -> rpl_info;
        372 -> rpl_motd;
        373 -> rpl_infostart; % Reserved
        374 -> rpl_endofinfo;
        375 -> rpl_motdstart;
        376 -> rpl_endofmotd;
        381 -> rpl_youreoper;
        382 -> rpl_rehashing;
        383 -> rpl_youreservice;
        384 -> rpl_myportis; % Reserved
        391 -> rpl_time;
        392 -> rpl_userstart;
        393 -> rpl_users;
        394 -> rpl_endofusers;
        395 -> rpl_nousers;
        % Error codes 400+
        401 -> err_nosuchnick;
        402 -> err_nosuchserver;
        403 -> err_nosuchchannel;
        404 -> err_cannotsendtochan;
        405 -> err_toomanychannels;
        406 -> err_wasnosuchnick;
        407 -> err_toomanytargets;
        409 -> err_noorgin;
        411 -> err_norecipient;
        412 -> err_notexttosend;
        413 -> err_notoplevel;
        414 -> err_wildtoplevel;
        415 -> err_badmask;
        421 -> err_unknowncommand;
        422 -> err_nomotd;
        423 -> err_noadmininfo;
        424 -> err_fileerror;
        431 -> err_nonicknamegiven;
        432 -> err_erroneousnickname;
        433 -> err_nicknameinuse;
        436 -> err_nickcollision;
        437 -> err_unavailresource;
        441 -> err_usernotinchannel;
        442 -> err_notonchannel;
        443 -> err_useronchannel;
        444 -> err_nologin;
        445 -> err_summondisabled;
        446 -> err_usersdisabled;
        451 -> err_notregistered;
        461 -> err_needmorepparams;
        462 -> err_alreadyregistered;
        463 -> err_nopermforhost;
        464 -> err_passwdmismatch;
        465 -> err_yourebannedcreep;
        466 -> err_youwillbebanned;
        467 -> err_keyset;
        471 -> err_channelisfull;
        472 -> err_unknownmode;
        473 -> err_inviteonlychan;
        474 -> err_bannedfromchan;
        475 -> err_badchannelkey;
        476 -> err_badchanmask;
        477 -> err_nochanmodes;
        478 -> err_banlistfull;
        481 -> err_noprivileges;
        482 -> err_chanoprivsneeded;
        483 -> err_cantkillserver;
        484 -> err_restricted;
        485 -> err_uniqoprivsneeded;
        491 -> err_nooperhost;
        492 -> err_noservicehost; % Reserved
        501 -> err_umodeunknownflag;
        502 -> err_usersdontmatch;
        _ -> ok
        % See RFC1459
        % See RFC2812
    end
end.

