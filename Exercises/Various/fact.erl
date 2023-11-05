-module(fact).
-export([fact/1]).

fact(0) -> 1;
fact(N) -> N * fact(N-1).

% c(fact).
% fact:fact(10).
