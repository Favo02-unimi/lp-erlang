-module(ex3_processRing_server).
-export([start/3]).

start(MessagesNumber, ProcessesNumber, Message) ->
  % function of each new process spawned
  % not a module function because functions not exported cannot be used as parameters of a spawn
  Process =
    fun Process() -> % name of the "anonymous" function needed to recursive call
      receive
        { message, Message } -> io:format("~p: ~s~n", [self(), Message]), Process(Message);
        quit -> io:format("~p: quit~n", [self()])
      end
    end,

  % spawn <ProcessesNumber> processes, each one running with the "anonymous" function Process
  PidList = lists:map(
    fun(_) -> spawn(Process)
    end, lists:seq(1, ProcessesNumber) % use lists:seq to run the functions <ProcessesNumber> times
  ),

  % send <MessagesNumber> message to all processes (iterate MessagesNumber times)
  lists:map(
    fun(_) ->
      % send <Message> to all processes (saved in PidList)
      lists:map(
        fun(Pid) -> Pid ! { message, Message }
      end, PidList)
    end, lists:seq(1, MessagesNumber)
  ),

  % send quit to all processes (saved in PidList)
  lists:map(
    fun(Pid) ->
      Pid ! quit
    end, PidList
  ),
  serverend.
