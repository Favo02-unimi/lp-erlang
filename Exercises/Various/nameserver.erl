-module(nameserver).
-export([start/0, store/2, lookup/1]).

% start nameserver
start() ->
  register(server, spawn(fun() -> loop() end)).

% store Key, Value in nameserver
store(Key, Value) ->
  server ! {self(), {store, Key, Value}},
  receive
    {server, Reply} -> Reply
  end.

% check if Key is in nameserver (and return value)
lookup(Key) ->
  server ! {self(), {lookup, Key}},
  receive
    {server, Reply} -> Reply
  end.

% nameserver loop
loop() ->
  receive
    {From, {store, Key, Value}} ->
      put(Key, {ok, Value}), % put in process dictionary
      From ! {server, true},
      loop();
    {From, {lookup, Key}} ->
      From ! {server, get(Key)}, % get from process dictionary
      loop()
  end.
