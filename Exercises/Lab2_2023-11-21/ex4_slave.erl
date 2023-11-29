-module(ex4_slave).
-export([reverse_string/3]).

reverse_string(String, From, Part) ->
  From ! {Part, lists:reverse(String)}.
