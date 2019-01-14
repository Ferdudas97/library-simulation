-module(library).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/1, get_all_books/1, return_book/2, borrow_book/3, add_new_book/1, handle_info/2, get_all_books_id/1, get_info/1, start/1, stop/1]).
-behavior(gen_server).


-record(state, {booksDb, rentalsDb, earnedMoney = 0,timer, stopTime}).


new_state(Number) ->
  #state{booksDb = book:create_n_new_books(book:new_books(), Number), rentalsDb = rental:new_rentals_db()}.
update_databases(BooksDb, RentalsDb, State) -> State#state{booksDb = BooksDb, rentalsDb = RentalsDb}.
update_state(Money, BooksDb, RentalsDb,State) -> State#state{booksDb = BooksDb, rentalsDb = RentalsDb, earnedMoney = Money}.
update_rentals(Rentals, State) -> State#state{rentalsDb = Rentals}.
update_books(Books, State) -> State#state{booksDb = Books}.
booksDb(State) -> State#state.booksDb.
rentalsDb(State) -> State#state.rentalsDb.
earnedMoney(State) -> State#state.earnedMoney.
set_timer(T,State) -> State#state{timer = T}.
get_timer(State) -> State#state.timer.
get_stop_time(State) -> State#state.stopTime.
set_stop_time(T,State) -> State#state{stopTime = T}.
start_link(Number) -> gen_server:start_link(?MODULE, new_state(Number), []).


init(State) ->
  {ok,T}=timer:send_interval(simulation:interval_milliseconds(), update),
  {ok, set_timer(T,State)}. %% no treatment of info here!


stop(Pid) ->
  gen_server:cast(Pid,stop).
start(Pid) ->
  gen_server:cast(Pid,start).


get_all_books_id(Pid) -> gen_server:call(Pid, get_all_not_rented_id).
get_all_books(Pid) -> gen_server:call(Pid, get_all_books).
return_book(Pid, BookId) ->
  Bool = gen_server:call(Pid, {return, BookId}),
  Bool.

borrow_book(Pid, BookId, ClientId) -> gen_server:call(Pid, {borrow, BookId, ClientId}).
add_new_book(Pid) -> gen_server:cast(Pid, add_book).
get_info(Pid) -> gen_server:call(Pid,get_data).

rent(BookId, ClientId, State) ->
  Books = book:rent_book(BookId, booksDb(State)),
  Rentals = rental:add_new_rental(BookId, ClientId, rentalsDb(State)),
  update_databases(Books, Rentals, State).



handle_call(get_all_not_rented_id, From, State) ->
  {reply, book:get_all_not_rented_id(booksDb(State)), State};
handle_call(get_all_books, From, State) ->
  {reply, book:get_all_books(booksDb(State)), State};

handle_call(get_data, From, State) ->
  {reply,get_data(State), State};


handle_call({borrow, BookId, ClientId}, From, State) ->
  S = book:get_all_not_rented(booksDb(State)),
  if
    S > 0 -> {reply, ok, rent(BookId, ClientId, State)};
    true -> {reply, bad, State}
  end;



handle_call({return, BookId}, From, State) ->
  Decision = rental:make_decision_about_return(BookId, rentalsDb(State)),
  if
    Decision =:= true ->
      %io:format("Oddano ksiązke ~p \n", [BookId]),
      Books = book:return_book(BookId, booksDb(State)),
      {Rentals, Money} = rental:remove_rental(BookId, rentalsDb(State)),
      NS = update_state(Money + earnedMoney(State), Books, Rentals,State),
      {reply, false, NS};
    true ->
      {reply, true, State}
  end.

handle_cast(add_book, State) ->
  New_Books = book:add_new_book(booksDb(State)),
  {noreply, update_books(New_Books,State)};

handle_cast(stop, State) ->
  timer:cancel(get_timer(State)),
  NS = set_timer(undefined, State),
  {noreply,set_stop_time(calendar:local_time(),NS)};


handle_cast(start, State) ->
  Delta = utills:time_delta(get_stop_time(State),calendar:local_time()),
   Upd= rental:update_time(rentalsDb(State),Delta),
  US = update_rentals(Upd,State),
  {ok,T} = timer:send_interval(simulation:interval_milliseconds(), update),
  {noreply,set_timer(T,US)}.

handle_info(info, State) ->
  show_info(State),
  {noreply, State};
handle_info(update, State)when State#state.timer =:=undefined ->
  io:format("no up\n"),
  {noreply, State};
handle_info(update, State) when State#state.timer =/=undefined->
  NewRentals = rental:update_fines(rentalsDb(State)),
  io:format("up\n"),
  {noreply, update_rentals(NewRentals, State)}.


show_info(State) ->
  B = maps:size(booksDb(State)),
  R = maps:size(rentalsDb(State)),
  M = earnedMoney(State),
  K = rental:get_number_of_kept_books(rentalsDb(State)),
  io:format("Zarobiono = ~p
  przetrzymane ksiązki = ~p
  jest wypożyczone ~p z ~p ksiązek\n", [M, K, R, B]).

get_data(State) ->
  B = maps:size(booksDb(State)),
  R = maps:size(rentalsDb(State)),
  M = earnedMoney(State),
  K = rental:get_number_of_kept_books(rentalsDb(State)),
  "Zarobiono = " ++ integer_to_list(M) ++ "
   przetrzymane ksiązki = " ++ integer_to_list(K) ++
    " jest wypożyczone " ++ integer_to_list(R) ++
    " z " ++ integer_to_list(B) ++ " ksiązek\n".

