-module(ex1_listComprehensions).
-export([squared_int/1, intersect/2, symmetric_difference/2]).

squared_int(List) ->
  [X*X || X <- List, is_number(X)].

intersect(L1, L2) ->
  [X || X <- L1, lists:member(X, L2)].

symmetric_difference(L1, L2) ->
  [X || X <- L1 ++ L2, not lists:member(X, intersect(L1,L2))].
