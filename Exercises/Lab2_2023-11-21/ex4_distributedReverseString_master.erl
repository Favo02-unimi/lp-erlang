-module(ex4_distributedReverseString_master).
-export([long_reversed_string/2]).

long_reversed_string(String, SlavesNum) ->
  Self = self(),
  lists:map(
    fun(I) -> spawn(
      ex4_distributedReverseString_slave,
      reverse_string,
      [divide_string(String, I, length(String) rem SlavesNum, SlavesNum), Self, I]
    ) end,
    lists:seq(1,SlavesNum)
  ),
  receive_and_build(1, SlavesNum, []).

% divide string into tokens,
% the first "Big" of length: len(String) // SlavesNum + 1
divide_string(String, Index, Big, SlavesNum) when Index =< Big ->
  BigLen = trunc(length(String) / SlavesNum) + 1,
  Start = (BigLen * (Index-1)) + 1,
  lists:sublist(String, Start, BigLen);
% the remaining of length: len(String) // SlavesNum
divide_string(String, Index, Big, SlavesNum) ->
  SmallLen = trunc(length(String) / SlavesNum),
  BigLen = SmallLen + 1,
  Start = (BigLen * Big + SmallLen * (Index-Big-1)) + 1,
  lists:sublist(String, Start, SmallLen).

% receive results from slaves, rebuilding string
% using Part as index to rebuild in correct order
receive_and_build(Cur, Stop, Acc) when Cur =< Stop ->
  receive
    {Cur, Part} ->
      receive_and_build(Cur+1, Stop, Part ++ Acc)
  end;
receive_and_build(_, _, Acc) -> Acc.
