-module(ex2_distributedAssociativeStore_server).
-export([loop/0]).

loop() ->
  receive
    {store, Key, Value} ->
      put(Key, Value),
      loop();
    {lookup, From, Key} ->
      From ! {Key, get(Key)},
      loop()
  end.
