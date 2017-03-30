-module(deal).
-author("Tobias Olausson").
-include("../nasbot.hrl").
-import(nasbot_utils,[get_command/1,get_message/1,get_sender/1,reply/2]).
-import(lists,[keyfind/3,keydelete/3,keyreplace/4,concat/1,map/2,sort/1,unzip/1]).
-export([handle/4,init/2]).

-define(I2L(N),integer_to_string(N)). % integer_to_list(N)).
-define(VALUES,[1,5,10,20,50,100,250,500,1000,2500,5000,10000,25000,50000,75000,
100000,200000,300000,400000,500000,750000,1000000,2500000,5000000]).
-define(STARTCOMMAND,"!deal or no deal").
-define(EXITCOMMAND,"!abort").
-define(STOPTIMES,[17,12,8,5,3,2,1]).
-define(DEAL,"DEAL").
-define(REJECT,"NO").
-define(CURRENCY,"SEK").
-define(STARTMESSAGE,"Deal or no deal! Choose your bag (1-24)!").
-define(REJECTMSG,"Bid rejected").
-define(ABORTMSG,"Game aborted").

-define(BAGCHOSEN(I),"Ok, your bag is "++?I2L(I)++", now choose 6 bags").
-define(DEALMESSAGE(Deal,Own), "Congratulations to " ++ ?I2L(Deal) ++ " " ++ ?CURRENCY ++ "! You had "++ ?I2L(Own) ++ " " ++ ?CURRENCY ++" in your bag").
-define(REJECTEND(Own), "Congratulations to "++ ?I2L(Own) ++ " " ++ ?CURRENCY ++ "!").
-define(LISTBAGS(Nick,Bags),Nick ++ ": There are "++ ?I2L(length(Bags))++ " bags to choose from. Choose one of " ++ concat(map(fun(B) -> ?I2L(B) ++ " " end, lists:sort(Bags)))).
-define(LISTVALUES(Values),"The following values are left: "++ concat(map(fun(V) -> ?I2L(V) ++ " " end, Values))).
-define(DEALOFFER(Nick,Deal),Nick ++ ": The bid from the bank is: "++ ?I2L(Deal) ++ " " ++ ?CURRENCY ++ ". If you accept, type "++?DEAL++", otherwise type "++?REJECT).
-define(CONTAINS(I,Val),"Bag "++ ?I2L(I)++" contains "++ ?I2L(Val) ++ " " ++ ?CURRENCY).
-define(DEALACCEPT(Pid,OwnVal,Left,Msg),Pid ! {send, reply(Msg,?DEALMESSAGE(compute_deal(OwnVal,Left),OwnVal))}).

% Deal or no deal plugin to nasboT
% TODO: Save statistics, better printout of values.


% Initialize, nobody plays
init(_Config, _BotState) -> [].

% Handle requests
handle(State, BotState, _Config, Msg) -> case get_command(Msg) of
    privmsg -> 
        Pid = BotState#botstate.pid,
        {nick,Who}  = get_sender(Msg),
        What = get_message(Msg),
        case keyfind(Who,1,State) of
            false when What == ?STARTCOMMAND ->
                Pid ! {send, reply(Msg,?STARTMESSAGE)},
                {ok, deal, [init_deal(Who) | State]};
            {Who,_} when What == ?EXITCOMMAND ->
                Pid ! {send, reply(Msg,?ABORTMSG)},
                {ok, deal, keydelete(Who,1,State)};
            {Who,DealState} -> 
                NewDealState = run_deal(Pid, Msg,DealState),
                NewState = case NewDealState of
                    ended -> keydelete(Who,1,State);
                    _     -> keyreplace(Who,1,State,{Who,NewDealState})
                end,
                {ok, deal, NewState};
            _ -> {ok, deal, State}
        end;
    _ -> {ok, deal, State}
end.

% This is the case when the player has not yet chosen a bag
run_deal(Pid, Msg,{Own,Left}) when Own == 0 ->
    What = string:strip(get_message(Msg)),
    case string:to_integer(What) of
        {I,_} when is_integer(I) andalso I > 0 andalso I =< 24 -> 
            {nick,Who} = get_sender(Msg),
            NewLeft = keydelete(I,1,Left),
            {Bags,_Vs} = unzip(NewLeft),
            Pid ! {send,reply(Msg,?BAGCHOSEN(I))},
            Pid ! {send,reply(Msg,?LISTBAGS(Who,Bags))},
            {ROBag,ROVal} = keyfind(I,1,Left),
            {{ROBag,ROVal,false},NewLeft};
        _ -> {Own,Left}
    end;

% The general case in this game
run_deal(Pid, Msg,{{OwnBag,OwnVal,DealStatus} = Own,Left}) -> 
    What       = string:to_upper(string:strip(get_message(Msg))),
    {nick,Who} = get_sender(Msg),
    case DealStatus of
        % Expect DEAL or REJECT
        true when What == ?DEAL -> 
            ?DEALACCEPT(Pid,OwnVal,Left,Msg), 
            ended;
        true when What == ?REJECT andalso length(Left) == 1 ->
            Pid ! {send, reply(Msg,?REJECTEND(OwnVal))},
            ended;
        true when What == ?REJECT ->
            {Bags,_Vs} = unzip(Left),
            Pid ! {send, reply(Msg,?REJECTMSG)},
            Pid ! {send, reply(Msg,?LISTBAGS(Who,Bags))},
            {{OwnBag,OwnVal,false},Left};
        true -> {Own,Left};
        % Expect a number
        false -> case string:to_integer(What) of
            {error,_} -> {Own,Left};
            {I,_}     -> case keyfind(I,1,Left) of
                false   -> {Own,Left};
                {I,Val} ->
                    Pid ! {send, reply(Msg,?CONTAINS(I,Val))},
                    NewLeft = keydelete(I,1,Left),
                    case lists:member(length(NewLeft),?STOPTIMES) of
                        true -> % Time to give a deal!
                            {_Bags,Vs} = unzip(NewLeft),
                            io:format("Computing deal~n",[]),
                            Deal       = compute_deal(OwnVal,NewLeft),
                            Values     = sort([OwnVal | Vs]),
                            io:format("Sending text ~n",[]),
                            Pid ! {send, reply(Msg,?DEALOFFER(Who,Deal))},
                            Pid ! {send, reply(Msg,?LISTVALUES(Values))},
                            {{OwnBag,OwnVal,true},NewLeft};
                        _ -> {Own,NewLeft}
                    end
            end
        end
    end.

% Formatting of integers
% Should delimit in thousands.
integer_to_string(N) -> integer_to_list(N).

% Deal is average of all bags - 10%
compute_deal(Val,Left) ->
    {_,Vals} = lists:unzip(Left),
    Avg = lists:sum([Val | Vals]) / length([Val | Vals]),
    erlang:trunc(Avg * 0.9).

% Initializes deal or no deal
% Randomize what values goes in what bag
init_deal(Who) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    Values = lists:sort(fun(_,_) -> random:uniform(2) == 1 end,?VALUES),
    {Who, {0,lists:zip(lists:seq(1,24),Values)}}.
