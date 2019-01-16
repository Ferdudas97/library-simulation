-module(client).

%% API
-export([handle_info/2, handle_call/3, handle_cast/2, start_link/2, init/1, add/1, info/1, stop/1, start/1, exit/1]).
-behavior(gen_server).
-record(client, {id, firstName, secondName, age, rentedBooks = []}).
-record(state, {clientsDb, pid, timer}).

get_clients(State) -> State#state.clientsDb.
get_timer(State) -> State#state.timer.
get_pid(State) -> State#state.pid.
set_clients(Clients,State) -> State#state{clientsDb = Clients}.
set_timer(Timer,State ) -> State#state{timer = Timer}.
new_state(Client,Pid) -> #state{clientsDb = Client,pid = Pid}.



start_link(Number,Pid) ->
  Client = add_n_new_clients(maps:new(), Number),
  State = new_state(Client,Pid),
  gen_server:start_link(?MODULE, State, []).



init(State) ->
  {ok,T} = timer:send_interval(simulation:interval_milliseconds(), interval),

  {ok, set_timer(T,State)}.

add(Pid) ->
  gen_server:cast(Pid,add).

info(Pid) ->
  gen_server:call(Pid,data).
stop(Pid) ->
  gen_server:cast(Pid,stop).
start(Pid) ->
  gen_server:cast(Pid,start).

exit(Pid) ->
  gen_server:cast(Pid,exit).
%% this clause will be called every 15 seconds
handle_info(interval, State) ->
  ClientsDB = get_clients(State),
  Pid = get_pid(State),
  Db = return(ClientsDB,Pid),
  NewDB = borrow(Db, Pid),
  ShouldCreate = should_create_new_client(),
  if
    ShouldCreate -> {noreply, set_clients(add_n_new_clients(NewDB,1),State)};
    true -> {noreply, set_clients(NewDB, State)}

  end.

borrow(ClientsDB, Pid) ->
  Clients = maps:values(ClientsDB),
  ClientsWhichwillBorrow = lists:filter(fun(X) -> is_borrowing(X) end, Clients),
  ClientsAfterBorrow = lists:map(fun(X) -> rent(X, Pid) end, ClientsWhichwillBorrow),
%%  io:format("Number of borrowed books= ~p ~n", [length(ClientsAfterBorrow)]),
  Mapped = utills:list_to_map(ClientsAfterBorrow, fun get_id/1),

  maps:merge(ClientsDB, Mapped).

return(ClientsDB,Pid) ->
  Clients = maps:values(ClientsDB),
  After = lists:map(fun (X) -> return_books(X,Pid) end, Clients),
  utills:list_to_map(After, fun get_id/1)
.

return_books(Client,Pid) ->
  Books = get_books(Client),

  After = lists:filter(fun(B)-> library:return_book(Pid,B) end, Books),
  if
    length(After) >0 ->    set_books(Client,After);
    true -> set_books(Client, [])
  end.




is_borrowing(Client) ->
  N = rand:uniform(101),
  Books = Client#client.rentedBooks,
  X = length(Books),

  if
    (N rem 100) * X < 10 -> true;
    true -> false
  end.

rent(Client, Pid) ->
  Ids = library:get_all_books_id(Pid),
  Id = utills:get_random_element(Ids),
  if
    Id=/=nothing ->  library:borrow_book(Pid, Id, Client#client.id),
      add_book_to_rented(Id, Client);
    true -> Client
  end.




add_book_to_rented(BookId, Client) ->
  Client#client{rentedBooks = [BookId | Client#client.rentedBooks]}.



should_create_new_client() ->
  Random = rand:uniform(101) rem 100,
  if
    Random < 25 -> true;
    true -> false
  end.
make_client(Id) ->
  _name = utills:get_random_element(names()),
  _surname = utills:get_random_element(surnames()),
  _age = rand:uniform(80) + 10,
  #client{id = Id, firstName = _name, secondName = _surname, age = _age}.


handle_call(data, _,  State) ->
  ClientsDB = get_clients(State),
  S = maps:size(ClientsDB),
  T = "Liczba klientów = " ++ integer_to_list(S) ++"\n",
  {reply,T,State}.
handle_cast(add, State) ->
  DB = get_clients(State),

  {noreply,set_clients(add_n_new_clients(DB,1),State)};

handle_cast(stop, State) ->
  timer:cancel(get_timer(State)),
  {noreply,State};


handle_cast(start, State) ->
  {ok,T} = timer:send_interval(simulation:interval_milliseconds(), interval),
  {noreply,set_timer(T,State)};
handle_cast(exit,State) ->
  {stop,shutdown,State}.




get_id(Clinet) -> Clinet#client.id.
get_books(Client) -> Client#client.rentedBooks.
set_books(Client,Books) -> Client#client{rentedBooks=Books}.
surnames() -> ["Chrzanowski", "Goral", "Malysz", "Skiba", "Nowak", "Kaczynski", "Lewandowski", "Milik", "Boruc"].
names() -> ["Radek", "Sebastian", "Wiktor", "Arek", "Adam", "Robert", "Jakub", "Michal"].

make_n_clients(ClientsDb, 0) -> [];
make_n_clients(ClientsDb, N) -> [make_client(N + maps:size(ClientsDb)) | make_n_clients(ClientsDb, N - 1)].
add_n_new_clients(ClientsDb, N) ->
  Clients = make_n_clients(ClientsDb, N),
  Mapped = utills:list_to_map(Clients, fun get_id/1),
  maps:merge(ClientsDb, Mapped).
