-module(ex4_pingPong_server).
-export([loop/0]).

loop() ->
  receive
    {msg, Msg} ->
      io:format("~p~n", [Msg]),
      loop();
    % {stop, _} -> ok;
    Other ->
      io:format("invalid ~p~n", [Other]),
      loop()
  end.
