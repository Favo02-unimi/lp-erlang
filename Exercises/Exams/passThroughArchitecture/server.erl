-module(server).
-export([init/0]).

init() ->
  group_leader(whereis(user), self()),
  io:format("server nato ~p ~p~n", [self(), node()]),
  loop().

loop() ->
  io:format("in loop~n", []),
  receive
    {plsCheck, Length} ->
      Res = checker(1, Length, first, ""),
      io:format("result ~p~n", [Res]),
      loop()
  end.

checker(Index, Length, _, _) when Index == Length -> true;

checker(Index, Length, first, _) ->
  receive
    {mmReq, {Index, El}} ->
      checker(Index, Length, second, El)
  end;

checker(Index, Length, second, Other) ->
  receive
    {mmReq, {Index, El}} when El == Other ->
      checker(Index+1, Length, first, "");
    {mmReq, {Index, El}} ->
      io:format("fail on ~p != ~p~n", [<<Other>>, <<El>>]), false
  end.
