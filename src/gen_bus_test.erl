%%%-------------------------------------------------------------------
%%% @author anatolyegorov
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Авг. 2014 5:02
%%%-------------------------------------------------------------------
-module(gen_bus_test).
-author("anatolyegorov").

%% API
-export([run/0,sender/2,receiver/1]).

run() ->
  gen_bus_app:start(normal,[]),
  gen_bus:subscribe(tag),
  Pids =  [ spawn(?MODULE,sender,[-1,100]) || _ <- lists:seq(1,100) ],
  _RPids = [spawn_link(?MODULE,receiver,[Pid])|| Pid <- Pids, _ <- lists:seq(1,1000)],
  spawn(fun() -> etop:start() end),
  loop().


loop() ->
  receive
    {gen_bus_msg,done} ->
      done;
    {gen_bus_msg,Message} ->
      io:format("~p~n",[Message]),
      loop()
  end.
sender(-1,Max) ->
  timer:sleep(1000),
  sender(0,Max);
sender(Max,Max) ->
  gen_bus:send(tag,done);
sender(N,Max) ->
  gen_bus:send_local(self(),{N,self()}),
  io:format("Sended: ~p ~n",[{N,self()}]),
  timer:sleep(random:uniform(1000)),
  sender(N+1,Max).

receiver(Pid) ->
  gen_bus:subscribe(tag),
  gen_bus:subscribe(Pid),
  receive
    {gen_bus_msg,done} ->
      done;
    {gen_bus_msg,Message} ->
      io:format("[~p] ~p ~n",[self(),Message]),
      receiver(Pid)
    end.