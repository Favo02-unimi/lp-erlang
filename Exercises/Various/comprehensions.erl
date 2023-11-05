-module(comprehensions).
-export([primes/1, qsort/2]).

primes(0) -> [];
primes(1) -> [];
primes(N) -> [X || X <-
  lists:seq(2,N),
  length([Y || Y <-
    lists:seq(2, trunc(math:sqrt(X))),
    X rem Y == 0
  ]) == 0
].

qsort(_, []) -> [];
qsort(CMP, [PIVOT|TAIL]) ->
  qsort(CMP, [X || X <-
    TAIL,
    CMP(PIVOT, X) < 0
  ]) ++
  [PIVOT] ++
  qsort(CMP, [X || X <-
    TAIL,
    CMP(PIVOT, X) >= 0
  ]).

% c(comprehensions).
% comprehensions:primes(100).
% comprehensions:qsort(fun(X, Y) -> Y-X end, [10,5,4,1,38,3,9,0]).
