-module(ex4_pingPong_client).
-export([start/0, print/1, stop/0]).

start () ->
  register(server, spawn_link(ex4_pingPong_server, loop, [])),
  ok.

print(Msg) ->
  server ! {msg, Msg},
  ok.

stop() ->
  exit(stop),
  ok.
