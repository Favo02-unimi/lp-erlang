-module(clock).
-export([start/2, stop/0]).

start(Time, Fun) ->
  ClockPid = spawn(fun() -> tick(Time, Fun) end),
  register(clock, ClockPid).

stop() ->
  clock ! stop.

tick(Time, Fun) ->
  receive
    stop -> void
    after Time -> Fun(), tick(Time, Fun)
  end.

% clock:start(5000, fun() -> io:format("TICK ~p~n",[erlang:now()]) end).
% clock:stop().
