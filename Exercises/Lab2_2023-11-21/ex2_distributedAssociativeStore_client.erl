-module(ex2_distributedAssociativeStore_client).
-export([start/1, store/2, lookup/1]).

start(ServersNum) ->
  register(middleman, spawn(ex2_distributedAssociativeStore_middleman, start_many_servers, [ServersNum])),
  ok.

store(Key, Value) ->
  middleman ! {store, Key, Value},
  ok.

lookup(Key) ->
  middleman ! {lookup, self(), Key},
  receive
    {Key, Result} -> Result;
    Other -> io:format("OTHER C ~p~n", [Other]), error
  end.
