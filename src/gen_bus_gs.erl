%%%-------------------------------------------------------------------
%%% @author anatolyegorov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Авг. 2014 2:41
%%%-------------------------------------------------------------------
-module(gen_bus_gs).
-author("anatolyegorov").

-behaviour(gen_server).

-include("../include/gen_bus.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?GEN_BUS).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(?ETS_PID2TAG,[bag,named_table,protected]),
  ets:new(?ETS_TAG2PID,[bag,named_table,protected]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State) ->
  {Pid, _} = From,
  case Request of
    {sub,Tags} ->
      [ begin
          ets:insert(?ETS_PID2TAG,{Pid,Tag}),
          ets:insert(?ETS_TAG2PID,{Tag,Pid})
        end
      || Tag <- Tags];
    {unsub,Tags} ->
      monitor(process,Pid),
      [ begin
          ets:delete_object(?ETS_PID2TAG,{Pid,Tag}),
          ets:delete_object(?ETS_TAG2PID,{Tag,Pid})
        end
        || Tag <- Tags];
    {unsub_all} ->
      demonitor(process,Pid),
      PidTags = ets:lookup(?ETS_PID2TAG,Pid),
      [ begin
          ets:delete_object(?ETS_PID2TAG,{Pid,Tag}),
          ets:delete_object(?ETS_TAG2PID,{Tag,Pid})
        end
        || {_,Tag} <- PidTags];
    {send,Tags,Message} ->
      Target_TagPids =lists:flatten([ets:lookup(?ETS_TAG2PID,Tag) || Tag <- Tags ]),
      Target_Pids = [TPid || {_,TPid} <- Target_TagPids],
      Pids = sets:to_list(sets:from_list(Target_Pids)),
      [  TPid ! {gen_bus_msg,Message} || TPid <-Pids ]
  end,
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
  case Info of
    {'DOWN', _, _, Pid, _} ->
      PidTags = ets:lookup(?ETS_PID2TAG,Pid),
      [ begin
          ets:delete_object(?ETS_PID2TAG,{Pid,Tag}),
          ets:delete_object(?ETS_TAG2PID,{Tag,Pid})
        end
        || {_,Tag} <- PidTags];
    _ -> none
  end,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
