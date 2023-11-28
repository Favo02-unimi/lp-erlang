-module(ex3_masterAndSlaves_client).
-export([start/1, to_slave/2]).

start(N) ->
  register(master, spawn(ex3_masterAndSlaves_master, init_master, [N])),
  ok.

to_slave(Message, N) ->
  master ! {Message, N}.

