-module(ex4_master).
-export([long_reversed_string/2]).

long_reversed_string(String, SlavesNum) ->
  Self = self(),

  % BigNum = length(String) rem SlavesNum,
  % BigSize =
  % Small = SlavesNum - Big,

  lists:map(
    fun(I) -> spawn(
      ex4_slave,
      reverse_string,
      % [divide_string(String, I, Big, Small), Self, I]
      [String, Self, I]
    ) end,
    lists:seq(1,SlavesNum)
  ),
  receive_and_build(1, SlavesNum, []).

% divide_string(String, Index, Big, Small) when Index =< Big ->
%   Start = (Index-1) * Big,
%   End = Start + ((length(String) / (Big+Small))+1),
%   lists:sublist(String, Start, End);
% divide_string(String, Index, Big, Small) ->
%   Start = Big * ((length(String) / (Big+Small))+1) + (Index-1) * Small,
%   End = Start + ((length(String) / (Big+Small))+1),
%   lists:sublist(String, Start, End);
%   .

receive_and_build(Cur, Stop, Acc) when Cur =< Stop ->
  receive
    {Cur, Part} ->
      io:format("~p ~p ~p~n", [Cur, Part, Acc]),
      receive_and_build(Cur+1, Stop, Part ++ Acc)
  end;
receive_and_build(_, _, Acc) -> Acc.
