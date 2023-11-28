-module(ex5_countingCalls_server).
-export([loop/0]).

loop() ->
  receive
    {call, From, Service} ->
      increment_dictionary_map(Service),
      Response =
        case Service of
          dummy1 -> dummy1();
          dummy2 -> dummy2();
          Other -> io:format("undefined service: ~p~n", [Other]), error
        end,
      From ! Response,
      loop();

    {tot, From} ->
      increment_dictionary_map(tot),
      From ! get(),
      loop()
  end.

increment_dictionary_map(Service) ->
  CurCount =
    case get(Service) of
      undefined -> 0;
      N -> N
    end,
  put(Service, CurCount+1),
  ok.

% dummy services
dummy1() ->
  ok.

dummy2() ->
  ok.
