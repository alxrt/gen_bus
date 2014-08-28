%%%-------------------------------------------------------------------
%%% @author anatolyegorov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Авг. 2014 2:42
%%%-------------------------------------------------------------------
-module(gen_bus).
-author("anatolyegorov").
-include("gen_bus.hrl").

%% Types
-export_type([message/0]).
-type message() :: {gen_bus_msg,Message :: term() }.

%% API
-export([subscribe/1, unsubscribe/1, unsubscribe_all/0, send/2,send_local/2]).


-spec subscribe/1 :: (Tags :: list()| term()) -> ok.
subscribe(Tags) when is_list(Tags) -> gen_server:call(?GEN_BUS, {sub, Tags},infinity);
subscribe(Tag) -> subscribe([Tag]).

-spec unsubscribe/1 :: (Tags :: list()| term()) -> ok.
unsubscribe(Tags) when is_list(Tags) ->    gen_server:call(?GEN_BUS, {unsub, Tags},infinity);
unsubscribe(Tag) -> unsubscribe([Tag]).

-spec unsubscribe_all/0 :: () -> ok.
unsubscribe_all() -> gen_server:call(?GEN_BUS, {unsub_all},infinity).

-spec send/2 :: (Tags:: list()|term(),Message :: term()) -> ok.
send(Tags, Message) when is_list(Tags) -> gen_server:call(?GEN_BUS, {send, Tags, Message},infinity);
send(Tag,Message) -> send([Tag],Message).

-spec send_local/2 :: (Tags:: list()|term(),Message :: term()) -> ok.
send_local(Tags, Message) when is_list(Tags) ->
  Target_TagPids =lists:flatten([ets:lookup(?ETS_TAG2PID,Tag) || Tag <- Tags ]),
  Target_Pids = [TPid || {_,TPid} <- Target_TagPids],
  Pids = sets:to_list(sets:from_list(Target_Pids)),
  [  TPid ! {gen_bus_msg,Message} || TPid <-Pids ],
 ok;
send_local(Tag, Message) -> send_local([Tag],Message).

