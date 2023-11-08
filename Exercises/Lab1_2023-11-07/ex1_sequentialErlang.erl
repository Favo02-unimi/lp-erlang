-module(ex1_sequentialErlang).
-export([is_palindrome/1, is_an_anagram/2, factors/1]).

% remove punctuation and case from a string
remove_punctuation(String) ->
  % trasform to lowercase
  string:lowercase(
    % remove all punctuation. options are:
    %   global: remove all occs not only first
    %   {return, list}: return a normal list and not a iolist
    re:replace(String, "[\s!?_.,:;\"\']", "", [global, {return, list}])
  ).

is_palindrome(String) ->
  Parsed = remove_punctuation(String),
  % parsed string is equal to its reverse
  Parsed == lists:reverse(Parsed).

% return the sum of the characters of a string
sum_string(Str) -> lists:foldr(fun(C, Acc) -> Acc + C end, 0, Str).

is_an_anagram(String, Dict) ->
  PString = remove_punctuation(String),
  Sum = sum_string(PString),
  PDict = lists:map(fun(D) -> remove_punctuation(D) end, Dict),
  length(
    lists:filter(fun(D) -> Sum == sum_string(D) end, PDict)
  ) > 0.

factors(Num) ->
  [X || X <-
    lists:seq(2, trunc(Num/2)+1),
    length([Y || Y <- lists:seq(2, trunc(math:sqrt(X))), X rem Y == 0]) == 0,
    Num rem X == 0
  ].
