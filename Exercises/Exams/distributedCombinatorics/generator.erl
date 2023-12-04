-module(generator).
-export([init/4, generate_column/3]).

init(N, M, Col, Master) ->

  GeneratedCol = generate_column(pow(M, N), M, Col),
  % io:format("generated ~p~n", [GeneratedCol]),

  Master ! { slaveRes, Col, GeneratedCol },
  ok.

pow(A, B) -> trunc(math:pow(A,B)).

generate_column(Len, M, Col) ->
  Duplications = pow(M, (Col-1)),
  Part = lists:flatten(
    lists:map(
      fun(I) -> lists:duplicate(Duplications, I) end,
      lists:seq(1, M)
    )
  ),
  lists:flatten(
    lists:duplicate(
      trunc(Len / length(Part)), Part
    )
  ).
