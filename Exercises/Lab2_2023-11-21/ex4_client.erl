-module(ex4_client).
-export([reverse/2]).

reverse(String, SlavesNum) ->
  ex4_master:long_reversed_string(String, SlavesNum).
