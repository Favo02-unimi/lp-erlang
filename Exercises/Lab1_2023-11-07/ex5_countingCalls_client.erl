-module(ex5_countingCalls_client).
-export([start/0, call_service/1, tot/0]).

start() ->
  register(server, spawn(ex5_countingCalls_server, loop, [])),
  ok.

call_service(ServiceName) ->
  server ! {call, self(), ServiceName},
  receive_response().

tot() ->
  server ! {tot, self()},
  receive_response().

receive_response() ->
  receive
    Response -> Response
  end.
