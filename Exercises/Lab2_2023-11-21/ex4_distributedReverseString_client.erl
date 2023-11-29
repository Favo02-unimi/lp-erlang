-module(ex4_distributedReverseString_client).
-export([reverse/2]).

reverse(String, SlavesNum) ->
  ex4_distributedReverseString_master:long_reversed_string(String, SlavesNum).
