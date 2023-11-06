-module(map_filter_reduce).
-export([map/2, filter/2, reduce/2]).

map(_, []) -> [];
map(F, [H|TL]) -> [F(H) | map(F, TL)].

% map_filter_reduce:map(fun(X) -> X*2 end, [2,3,4,5,6]).
% [4,6,8,10,12]

filter(_, []) -> [];
filter(P, [H|TL]) -> filter(P(H), P, H, TL).

filter(true, P, H, L) -> [H|filter(P, L)];
filter(false, P, _, L) -> filter(P, L).

% map_filter_reduce:filter(fun(X) -> X rem 2 == 0 end, [2,3,4,5,6]).
% [2,4,6]

reduce(F, [H|TL]) -> reduce(F, H, TL).

reduce(_, Q, []) -> Q;
reduce(F, Q, [H|TL]) -> reduce(F, F(Q,H), TL).

% map_filter_reduce:reduce(fun(X, ACC) -> ACC + X end, [2,3,4,5,6]).
% 20
