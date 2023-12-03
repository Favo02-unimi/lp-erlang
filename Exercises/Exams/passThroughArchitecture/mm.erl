-module(mm).
-export([init/1]).

init(Server) ->
  group_leader(whereis(user), self()),
  io:format("mm nato ~p ~p~n", [self(), node()]),
  loop(Server).

loop(Server) ->
  io:format("in loop~n", []),
  receive
    {request, String} ->
      io:format("received ~p~n", [String]),
      Server ! {plsCheck, length(String)},
      lists:foreach(
        fun(Elem) -> Server ! {mmReq, Elem} end,
        lists:enumerate(String)
      ),
      loop(Server)
  end.
