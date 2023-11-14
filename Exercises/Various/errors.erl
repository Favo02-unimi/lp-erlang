-module(errors).
-export([start/2]).

start(Bool, M) ->
  A = spawn(fun() -> a() end),
  B = spawn(fun() -> b(A, Bool) end),
  C = spawn(fun() -> c(B, M) end),
  sleep(1000), status(a, A), status(b, B), status(c, C).

% always system process
a() ->
  process_flag(trap_exit, true),
  listen(a).

% linked to A, system process depending on Bool
b(A, Bool) ->
  process_flag(trap_exit, Bool),
  link(A),
  listen(b).

% linked to B, normal process, dies in various ways depending on M
c(B, M) ->
  link(B),
  case M of
    {die, Reason} -> exit(Reason);
    {divide, N} -> _ = 1/N, listen(c);
    normal -> true
  end.

% prints the messages received by Prog
listen(Prog) ->
  receive
    Any -> io:format("Process ~p received ~p~n", [Prog, Any]),
    listen(Prog)
  end.

% pauses esecution for T milliseconds
sleep(T) ->
  receive
    after T -> true
  end.

% prints the status (alive or dead) of a process (Pid)
status(Name, Pid) ->
  case erlang:is_process_alive(Pid) of
    true -> io:format("Process ~p (~p) is alive~n", [Name, Pid]);
    false -> io:format("Process ~p (~p) is dead~n", [Name, Pid])
  end.
