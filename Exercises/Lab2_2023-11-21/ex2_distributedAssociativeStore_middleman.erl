-module(ex2_distributedAssociativeStore_middleman).
-export([start_many_servers/1]).

start_many_servers(ServersNum) ->
  Servers = lists:map(
    fun(_) -> spawn(ex2_distributedAssociativeStore_server, loop, []) end,
    lists:seq(1,ServersNum)
  ),
  loop(Servers).

loop(Servers) ->
  receive
    {store, Key, Value} ->
      lists:foreach(
        fun(Pid) -> Pid ! {store, Key, Value} end,
        Servers
      ),
      loop(Servers);

    {lookup, From, Key} ->
      ServerToFetch = rand:uniform(length(Servers)),
      io:format(
        "fetching from server ~p (~p) of ~p servers~n",
        [ServerToFetch, lists:nth(ServerToFetch, Servers), length(Servers)]
      ),
      lists:nth(ServerToFetch, Servers) ! {lookup, self(), Key},
      Result = receive
        {Key, Res} -> Res;
        Other -> io:format("OTHER MM ~p~n", [Other]), error
      end,
      From ! {Key, Result},
      loop(Servers)
  end.
