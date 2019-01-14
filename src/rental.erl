-module(rental).

-export([get_rentals/1, new_rentals_db/0, update_fines/1, add_new_rental/3, remove_rental/2, make_decision_about_return/2, get_number_of_kept_books/1, update_time/2]).

-record(rental, {bookId, userId, timeOfRent, timeToReturn, toPay}).



new_rentals_db() -> maps:new().
get_rentals(Db) -> maps:values(Db).
add_new_rental(Bid, Cid, Db) ->
  maps:put(Bid, make_rental(Bid, Cid), Db).
remove_rental(Bid, Db) ->
  {maps:remove(Bid, Db), (maps:get(Bid, Db))#rental.toPay}.
update_fines(Db) ->
  maps:map(fun(K, V) -> check_if_should_pay_more(V) end, Db).
update_time(Db, Delta) ->
  io:format("delta ~p \n", [Delta]),
  maps:map(fun(K, V) -> add_time(V, Delta) end, Db).


add_time(Rental, Delta) ->
  T = Rental#rental.timeToReturn,
  UpdatedTime = add_time_to_datetime(T, {0, 0, Delta}),
  Rental#rental{timeToReturn = UpdatedTime}.

is_kept(Rental) -> if
                     Rental#rental.toPay > 0 -> true;
                     true -> false
                   end.
get_number_of_kept_books(Db) ->
  Rentals = get_rentals(Db),
  Kept = lists:filter(fun is_kept/1, Rentals),
  length(Kept).
check_if_should_pay_more(Rental) ->
  IsTime = check_if_is_time_to_pay(calendar:local_time(), Rental#rental.timeToReturn),
  % io:format("is time = ~p \n",[IsTime]),
  if
    IsTime =:= true -> increament_to_pay(Rental);
    IsTime =:= false -> Rental
  end.

increament_to_pay(Rent) -> Rent#rental{toPay = Rent#rental.toPay + 1}.
make_rental(Bid, Uid) ->
  Date = calendar:local_time(),
  Seconds = 7,
  ReturnDate = add_time_to_datetime(Date, {0, 0, Seconds}),  % add 2 weeks
  #rental{bookId = Bid, userId = Uid, timeOfRent = calendar:local_time(), timeToReturn = ReturnDate, toPay = 0}.


make_decision_about_return(BookId, Db) ->
  Rental = maps:get(BookId, Db),
  Delta = utills:time_delta(calendar:local_time(), Rental#rental.timeToReturn) - 1,
  Random = rand:uniform(101) rem 100,
  %% io:format("delat = ~p  random = ~p \n", [Delta, Random]),
  if
    Delta =< 0, Random > 50 -> true;
    Delta > 0, Random > 80 -> true;
    true -> false
  end.


check_if_is_time_to_pay(BorrowDate, ReturnDate) ->
  Delta = utills:time_delta(BorrowDate, ReturnDate),
  if
    Delta < 0 -> true;
    Delta >= 0 -> false
  end.

add_time_to_datetime(Date, {Hour, Min, Sec}) ->
  DateInSeconds = calendar:datetime_to_gregorian_seconds(Date),
  NewDateInSeconds = DateInSeconds + (Hour * 60 * 60) + (Min * 60) + Sec,
  calendar:gregorian_seconds_to_datetime(NewDateInSeconds).