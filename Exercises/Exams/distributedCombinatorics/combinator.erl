-module(combinator).
-export([start/2]).

start(N, M) ->
  Self = self(),
  lists:map(
    fun(I) -> spawn(generator, init, [N, M, I, Self]) end,
    lists:seq(1, N)
  ),
  Cols = lists:map(
    fun(El) -> lists:flatten(rec_tuple_to_list(El)) end,
    zip(receiveCols(1, N, []))
  ),
  lists:foreach(
    fun(El) -> print_list(El) end,
    Cols
  ),
  ok.

receiveCols(Current, Max, Acc) when Current =< Max ->
  receive
    { slaveRes, Current, Res } ->
      receiveCols(Current+1, Max, [ Res | Acc ])
  end;
receiveCols(_, _, Acc) -> Acc.

zip([L | []]) -> L;
zip([L1 | [L2 | T]]) ->
  zip([lists:zip(L1, L2) | T]).

rec_tuple_to_list([H | T]) -> [H | T];
rec_tuple_to_list({El1, El2}) ->
  rec_tuple_to_list([
    rec_tuple_to_list(El1),
    rec_tuple_to_list(El2)
  ]);
rec_tuple_to_list(Int) -> Int.

print_list([ H | [] ]) -> io:format("~p~n", [H]);
print_list([ H | T ]) -> io:format("~p, ", [H]), print_list(T).
