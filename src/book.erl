
-module(book).

%% API
-export([remove_book/2, add_book/2, add_new_book/1, rent_book/2, get_all_books/1, get_all_not_rented/1, get_all_rented/1, new_books/0, return_book/2, get_all_not_rented_id/1, create_n_new_books/2]).
-record(book, {id, genre, isRented =false}).


genres() -> ["Sci-fi","Comedy","Fantassy","Horror","poetry","satire","romance","math","encyclopedia","biography","history","criminal","fairytale"].
new_books() ->maps:new().

get_all_books(Books) -> maps:values(Books).

get_all_not_rented_id(Books) ->
  L = get_all_not_rented(Books),
  lists:map(fun(X) ->X#book.id end,L).
get_all_rented(Books) ->
  All = maps:values(Books),
  lists:filter(fun(B) -> B#book.isRented =:= true end, All).
get_all_not_rented(Books) ->
  All = maps:values(Books),
  lists:filter(fun(B) -> B#book.isRented =:= false end, All).
rent_book(Id, Books) ->
  Book = maps:get(Id, Books),
  Rented = Book#book{isRented = true},
  maps:put(Id, Rented, Books).

return_book(Id,Books) ->
  Book = maps:get(Id, Books),
  Rented = Book#book{isRented = false},
  maps:put(Id, Rented, Books).
add_book(Book, Books) ->
  maps:put(Book#book.id, Book, Books).

add_new_book(Books) ->
  create_n_new_books(Books,1).



remove_book(Id, Books) -> maps:remove(Id, Books).

create_n_book(Books,0) -> [];
create_n_book(Books,N) ->
  [create_book(N+maps:size(Books))|create_n_book(Books,N-1)].
create_n_new_books(BooksDb,N) ->
  Books = create_n_book(BooksDb,N),
  Mapped = utills:list_to_map(Books,fun get_id/1),
  maps:merge(BooksDb,Mapped).
create_book(Id) ->
  Genre = utills:get_random_element(genres()),
  #book{id = Id, genre = Genre}.


get_id(Book) ->
  Book#book.id.