
-module(utills).

%% API
-export([get_random_element/1, list_to_map/2, time_delta/2]).

get_random_element(L) ->
  lists:nth(rand:uniform(length(L)),L).


list_to_map([],_) ->maps:new();
list_to_map([H|T],F) -> maps:put(F(H),H,list_to_map(T,F)).

time_delta(Time1,Time2) ->
  S1 = calendar:datetime_to_gregorian_seconds(Time1),
  S2 = calendar:datetime_to_gregorian_seconds(Time2),
  S2-S1.