-module(ex3_masterAndSlaves_slave).
-export([loop/0]).

loop() ->
  Master = whereis(master),
  receive
    {Master, die} ->
      exit(killed);

    {Master, Msg} ->
      io:format("slave ~p: ~p~n", [self(), Msg]),
      loop()
  end.
