-module(ex5_countingCalls).
-export([start/0, dummy1/0, dummy2/0, tot/0]).

start() ->
  register(server, spawn(fun() -> counter() end)),
  ok.

dummy1() ->
  server ! {call, dummy1},
  ok.

dummy2() ->
  server ! {call, dummy2},
  ok.

tot() ->
  server ! {tot, self()},
  receive
    {list, List} -> List
  end.

counter() ->
  receive
    {call, Service} ->
      increment_dictionary_map(Service),
      counter();
    {tot, From} ->
      increment_dictionary_map(tot),
      From ! {list, get()},
      counter()
  end.

increment_dictionary_map(Service) ->
  CurCount =
    case get(Service) of
      undefined -> 0;
      N -> N
    end,
  put(Service, CurCount+1),
  ok.
