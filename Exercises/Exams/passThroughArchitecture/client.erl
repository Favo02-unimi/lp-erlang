-module(client).
-export([start/0, close/0, is_palindrome/1]).

start() ->
  group_leader(whereis(user), self()),
  {ok, Hostname} = inet:gethostname(),
  Server = spawn_link(list_to_atom("server@" ++ Hostname), server, init, []),
  Mm1 = spawn_link(list_to_atom("mm1@" ++ Hostname), mm, init, [Server]),
  Mm2 = spawn_link(list_to_atom("mm2@" ++ Hostname), mm, init, [Server]),
  io:format("spawned ~p, ~p, ~p~n", [Mm1, Mm2, Server]),
  register(client, spawn(fun() -> loop(Mm1, Mm2) end)),
  ok.

close() ->
  client ! {stop},
  ok.

is_palindrome(String) ->
  client ! {ispal, String},
  ok.

loop(Mm1, Mm2) ->
  receive
    {ispal, String} ->
      [Part1, Part2] = split_string(String),
      io:format("p1 ~p, p2 ~p~n", [Part1, Part2]),
      Mm1 ! {request, Part1},
      Mm2 ! {request, lists:reverse(Part2)},
      loop(Mm1, Mm2);
    {stop} -> exit("stopping")
  end.

split_string(String) when length(String) rem 2 == 0 -> % even
  L = trunc(length(String)/2),
  [ lists:sublist(String, L), lists:sublist(String, L+1, L) ];
split_string(String) -> % odd
  L = trunc(length(String)/2),
  [ lists:sublist(String, L+1), lists:sublist(String, L+1, L+1) ].
