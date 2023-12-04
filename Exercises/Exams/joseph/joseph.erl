-module(joseph).
-export([joseph/2]).

joseph(N, K) ->
  Self = self(),
  Pids = lists:map(
    fun(I) -> spawn(hebrew, init, [I, K, Self]) end,
    lists:seq(1, N)
  ),
  [First | _] = Pids,
  set_nexts(First, Pids),
  start_ring(First, N, K),
  receive
    { joseph, Name } -> io:format("Joseph is the Hebrew in position ~p~n", [Name])
  end.

set_nexts(First, [Last | []]) ->
  Last ! { setNext, First };
set_nexts(First, [H1 | [H2 | T]]) ->
  H1 ! { setNext, H2 },
  set_nexts(First, [H2 | T]).

start_ring(First, N, K) ->
  io:format("In a circle of ~p people, killing number ~p~n", [N, K]),
  First ! { pass, K-1 }.
