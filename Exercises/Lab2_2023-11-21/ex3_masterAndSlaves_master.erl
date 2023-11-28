-module(ex3_masterAndSlaves_master).
-export([init_master/1]).

init_master(N) ->
  process_flag(trap_exit, true),
  Slaves = lists:map(
    fun(_) -> spawn_link(ex3_masterAndSlaves_slave, loop, []) end,
    lists:seq(1,N)
  ),
  io:format("slaves pids: ~p~n", [Slaves]),
  loop(Slaves).

loop(Slaves) ->
  receive
    {die, N} ->
      lists:nth(N, Slaves) ! {self(), die},
      loop(Slaves);

    {Message, N} ->
      lists:nth(N, Slaves) ! {self(), Message},
      loop(Slaves);

    {'EXIT', Pid, Why} ->
      SlaveIndex = index_of(Pid, Slaves, 1),
      NewSlave = spawn_link(ex3_masterAndSlaves_slave, loop, []),
      NewList = replace(SlaveIndex, NewSlave, Slaves),
      io:format(
        "slave ~p (index ~p) died (reason: ~p), spawned new slave (~p), new slaves: ~p~n",
        [Pid, SlaveIndex, Why, NewSlave, NewList]),
      loop(NewList)
  end.

index_of(_, [], _) -> -1;
index_of(Target, [H|_], Count) when H == Target -> Count;
index_of(Target, [_|T], Count) -> index_of(Target, T, Count+1).

replace(Index, Elem, List) ->
  lists:sublist(List, Index-1) ++ [Elem] ++ lists:sublist(List, Index+1, length(List)).
