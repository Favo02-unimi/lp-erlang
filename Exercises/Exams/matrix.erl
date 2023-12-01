-module(matrix).
-export([mproduct/2]).

% ----- server -----
mproduct(A,B) ->
  Self = self(),
  BPids = lists:map(
    fun({Index, Col}) -> spawn(fun() -> b_handler(Index, Col, Self) end) end,
    lists:enumerate(B)
  ),
  lists:map(
    fun({Index, Row}) -> spawn(fun() -> a_handler(Index, Row, BPids) end) end,
    lists:enumerate(A)
  ),
  traspose(server_receiver(1,length(B)+1,[])).

% receive results from B actors, in order (Count incrementing)
server_receiver(Count, Tot, Result) when Count < Tot ->
  receive
    {fromB, Count, Row} -> server_receiver(Count+1, Tot, [Row | Result])
  end;
server_receiver(_,_, Result) -> lists:reverse(Result).

% traspose a matrix (B actors will return the columns)
traspose(Matrix) ->
  [[lists:nth(I, C) || C <- Matrix] || I <- lists:seq(1, length(lists:nth(1, Matrix)))].

% ----- a -----
% send own row to all B actors
a_handler(Index, Row, BPids) ->
  lists:foreach(
    fun(Pid) -> Pid ! {fromA, Index, Row} end,
    BPids
  ),
  sent.

% ----- b -----
% receive all rows from A actors, calculates the result and sends to server
b_handler(Index, Col, Parent) ->
  Result = b_receiver(1, length(Col), Col, []),
  Parent ! {fromB, Index, Result},
  sent.

% receive rows from A actors, in order (Count incrementing)
b_receiver(Count, Tot, Col, Result) when Count < Tot ->
  receive
    {fromA, Count, Row} ->
      R = [b_calculator(Col, Row) | Result],
      b_receiver(Count+1, Tot, Col, R)
  end;
b_receiver(_,_,_, Result) -> lists:reverse(Result).

% calculates the value of a single point of the matrix
b_calculator(Col, Row) ->
  lists:sum(
    lists:map(
      fun(I) -> lists:nth(I, Row) * lists:nth(I, Col) end,
      lists:seq(1, length(Col))
    )
  ).

% matrix:mproduct([[1,1,2],[0,1,-3]],[[1,2,0],[1,5,-2],[1,1,1]]).
% matrix:mproduct([[1,2,0],[3,-1,4]],[[1,0,-1]]).
% matrix:mproduct([[5,-1,2],[0,2,3]],[[2,1,0],[0,2,-1],[3,0,0],[-1,2,0]]).
