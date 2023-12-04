-module(hebrew).
-export([init/3]).

init(Name, K, Master) ->
  Next = receive
    { setNext, Next } -> Next
  end,
  % io:format("next settato ~p sono ~p~n", [Next, Name]),
  loop(Name, Next, K, Master).

loop(Name, Next, _, Master) when Next == self() ->
  % io:format("Joseph is the Hebrew in position ~p~n", [Name]),
  Master ! { joseph, Name };
loop(Name, Next, K, Master) ->
  receive
    % send back the next node (to close the gap)
    { die, From } ->
      % io:format("In a circle of ~p people, killing number ~p~n", [K, Name]),
      From ! { myNext, Next };

    % next should die, send die message and wait for new next
    { pass, ToKill } when ToKill =:= 1 ->
      Next ! { die, self() },
      NewNext = receive
        { myNext, NewNext } -> NewNext
      end,
      % io:format("Sono ~p, Inoltro a ~p, con K~p~n", [self(), NewNext, K-1]),
      NewNext ! { pass, K-1 },
      loop(Name, NewNext, K, Master);

    % base case: I shouldnt die and next should die
    { pass, ToKill } ->
      % io:format("Sono ~p, Inoltro a ~p, con K~p~n", [self(), Next, ToKill-1]),
      Next ! { pass, ToKill-1 },
      loop(Name, Next, K, Master)

  end.
