-module(nasbot).
-author("Tobias Olausson").

% TODO: Rename this module server, and create main module.
-behaviour(gen_server).

-import(gen_tcp,[send/2]).
-import(nasbot_parser,[parse_message/1]).
-import(nasbot_utils,[is_ping/1,get_params/1,get_command/1]).

-include("nasbot.hrl").
-export([main/0,listen/3]). % Subject to change with transition to gen_server
-define(PLUGINDIR,"plugins/").
-define(DELAY,10000). % Delay in ms

g_connect(Config,BotState) -> 
    Name = Config#config.name,
    gen_server:start_link({local,Name},?MODULE,[Config,BotState],[]).

% gen_server module callback functions

% Should only be passed [Config]
init([Config,BotState]) -> 
    Server = Config#config.address,
    Port   = Config#config.port,
    Nick   = Config#config.nick,
    log(info,"nasboT 0.1 connecting to ~s on port ~B",[Server,Port]),
    case gen_tcp:connect(Server,Port,[list,{packet,line},{nodelay,true}]) of
        {error,Why}  -> receive after ?DELAY -> {connect_error,Why} end;
        {ok, Socket} -> 
            send(Socket, "NICK " ++ Nick),
            send(Socket, "USER " ++ Nick ++ " * * :" ++ Nick),
            {ok, {Socket, Config, BotState}}
    end.

% Utility function
send(Socket, Msg) -> gen_tcp:send(Socket,Msg ++ "\r\n").

% TODO: Handle these
%            lists:foreach(fun(P) -> Bot ! {load, P} end, Config#config.plugins),
%            listen(Socket,Config,BotState#botstate{pid = Bot})


% Working as an alias temporarily.
handle_call(Request,From,State) -> handle_cast(Request,State).
handle_cast(Request,State) -> ok.

handle_info(Info,State) -> ok.
% Cleanup
terminate(Reason,{Sock,BotState}) -> ok.
% Update
code_change(OldVsn,State,Extra) -> ok.


% Main function, startup and stuff
% TODO: Move this outside
main() ->
    log(info,"Starting up nasboT 0.1",[]),
    try 
        Servers = config:parse_config(),
        spawn(fun() ->
            process_flag(trap_exit,true),
            Lst = lists:map(fun(S) -> 
                Pid = spawn_link(fun() -> connect(S, #botstate{}) end),
                {Pid,S,S#config.name}
                end, Servers),
            supervise(Lst)
        end)
    catch _:_ -> log(fatal,"Malformed/missing configuration file, exiting.",[])
    end.

% Main running loop
% BotState :: server record
listen(Sock, Config, BotState) -> 
    receive
        {tcp,Sock,Message} ->
            % Remove CRLF
            Msg = lists:filter(fun(C) -> (C /= 10) and (C /= 13) end, Message),
            log(incoming,"~s",[Msg]),
            case catch parse_message(Msg) of
                Result when is_record(Result,irc) -> 
                    NewBotState = runCore(Config,BotState,Result),
                    NewPlugins  = runPlugins(Config,NewBotState,Result),
                    listen(Sock, Config, NewBotState#botstate{ plugins = NewPlugins });
                {'EXIT',parse_fail} -> 
                    log(error,"PARSE FAIL: ~s",[Msg]),
                    listen(Sock,Config,BotState);
                {'EXIT',Reason} -> log(fatal,"Internal parser error ~w",[Reason])
            end;
        {tcp_closed,Sock} -> 
            log(fatal,"TCP connection closed, reconnecting",[]),
            self() ! reconnect,
            listen(Sock,Config,BotState);
        {tcp_error,Sock}  -> 
            log(fatal,"TCP error, reconnecting",[]),
            self() ! reconnect,
            listen(Sock,Config,BotState);
        {send,Msg} ->
            catch log(outgoing,"~s",[Msg]),
            send(Sock,Msg ++ "\r\n"),
            listen(Sock,Config,BotState);
        update -> % TODO: Make this fancier
            log(info,"Updating code",[]),
            ?MODULE:listen(Sock,Config,BotState);
        reconnect -> 
            gen_tcp:close(Sock),
            connect(Config,BotState);
        quit -> ok;
        {unload, Plugin} when is_atom(Plugin) ->
            log(info,"Unloading ~s", [atom_to_list(Plugin)]),
            Plugins    = BotState#botstate.plugins,
            NewPlugins = lists:filter(fun({P,_Ps}) -> P /= Plugin end, Plugins),
            listen(Sock,Config,BotState#botstate{ plugins = NewPlugins});
        {load, Plugin} when is_atom(Plugin) ->
            PStr = atom_to_list(Plugin),
            case catch loadPlugin(Plugin, Config, BotState) of
                {ok, Plugins} -> 
                    log(info,"Loaded ~s",[PStr]),
                    listen(Sock,Config,BotState#botstate{plugins = Plugins});
                _ -> 
                    log(error,"Unable to load plugin ~s",[PStr]),
                    listen(Sock,Config,BotState)
            end
    end.

loadPlugin(Plugin,Config,BotState) ->
    Exports = Plugin:module_info(exports),
    Plugins = BotState#botstate.plugins,
    case lists:member({init,2},Exports) andalso 
         lists:member({handle,4},Exports) andalso 
         not lists:keymember(Plugin,1,Plugins) of
        false -> fail;
        true  -> 
            P = {Plugin, Plugin:init(Config,BotState)},
            {ok, [ P | Plugins]}
    end.

% Utility function
log(LogLevel, Format, Messages) ->
  {H,M,S} = time(),
  [HH,MM,SS] = lists:map(fun(N) ->
    string:right(integer_to_list(N),2,48) end, [H,M,S]), % Prepend 0s if needed
  Logger = case LogLevel of 
    error -> "!! "; 
    fatal -> "!!!! ";
    info -> "** "; 
    incoming -> ">> "; 
    outgoing -> "<< "; 
    _ -> []
  end,
  case catch io:format("~s:~s:~s " ++ Logger ++ Format ++ "~n", [HH,MM,SS | Messages]) of
    {'EXIT',_Reason} -> log(error,"FORMAT ERROR",[]);
    _ -> ok
  end.

% Failsafe way to run plugins and still making sure we do not crash!
runPlugins(Config, BotState,Msg) ->
  lists:map(fun({P,PState}) ->
    case catch P:handle(PState,BotState,Config, Msg) of
        {ok,P,NewState} -> {P,NewState};
        _               -> 
            log(error,"Abnormal module exit: ~s",[atom_to_list(P)]),
            {P,PState}
    end 
  end, BotState#botstate.plugins).

% Updates the bot's internal state if needed.
% Support for !load and !unload aswell?
runCore(_Config, BotState, Msg) ->
    case get_command(Msg) of
        join    -> ok;
        part    -> ok;
        privmsg -> ok;
        ping    -> self() ! {send, "PONG :" ++ get_params(Msg) }; 
        _       -> ok
    end,
    BotState.
