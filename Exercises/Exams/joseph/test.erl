-module(test).
-export([test/0]).

test() ->
  joseph:joseph(30,3), % 29
  joseph:joseph(300,1001), % 226
  joseph:joseph(3000,37), % 1182
  joseph:joseph(26212,2025), % 20593
  joseph:joseph(1000,1000), % 609
  joseph:joseph(2345,26212), % 2896
  joseph:joseph(100000,7). % 27152

% erl -noshell -eval 'test:test()' -s init stop.
