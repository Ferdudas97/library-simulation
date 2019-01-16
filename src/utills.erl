
-module(utills).

%% API
-export([get_random_element/1, list_to_map/2, time_delta/2, is_integer/1]).

get_random_element(L) ->
  S= (length(L)),
  if
     S>0->  lists:nth(rand:uniform(length(L)),L);
    true ->  nothing
  end.


list_to_map([],_) ->maps:new();
list_to_map([H|T],F) -> maps:put(F(H),H,list_to_map(T,F)).

time_delta(Time1,Time2) ->
  S1 = calendar:datetime_to_gregorian_seconds(Time1),
  S2 = calendar:datetime_to_gregorian_seconds(Time2),
  S2-S1.

is_integer(S) ->
  try
    _ = list_to_integer(S),
    true
  catch error:badarg ->
    false
  end.