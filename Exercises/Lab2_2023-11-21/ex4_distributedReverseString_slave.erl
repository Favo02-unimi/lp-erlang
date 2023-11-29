-module(ex4_distributedReverseString_slave).
-export([reverse_string/3]).

reverse_string(String, From, Part) ->
  io:format("slave ~p reversing part ~p: ~p~n", [self(), Part, String]),
  From ! {Part, lists:reverse(String)}.
