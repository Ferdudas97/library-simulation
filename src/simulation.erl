
-module(simulation).

%% API
-export([main/2, interval_milliseconds/0, stop/2, start/2]).

interval_milliseconds() -> 1000.


start(Cpid,Lpid) ->
  library:start(Lpid),
  client:start(Cpid).
stop(Cpid,LPid)  ->
  library:stop(LPid),
  client:stop(Cpid).

main(ClientsNumber, BookNumber) ->
  {ok,LPid} = library:start_link(BookNumber),
  {ok,CPid}= client:start_link(ClientsNumber,LPid),
  {LPid,CPid}.


