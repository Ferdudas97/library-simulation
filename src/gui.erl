%%%-------------------------------------------------------------------
%%% @author radek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. gru 2018 21:06
%%%-------------------------------------------------------------------
-module(gui).
-author("radek").

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/3, start/0, handle_info/2]).
-behaviour(gen_server).

-record(state, {lpid :: pid(), cpid :: pid(), frame, text, env, timer}).

get_timer(State) -> State#state.timer.
set_timer(T, State) -> State#state{timer = T}.
get_lib(State) -> State#state.lpid.
get_client(State) -> State#state.cpid.
new_state(F, Text, Env) -> #state{frame = F, text = Text, env = Env}.
get_frame(State) -> State#state.frame.
set_frame(F, State) -> State#state{frame = F}.
get_text(State) -> State#state.text.
set_text(T, State) ->
  State#state{text = T}.
get_env(State) -> State#state.env.
new_simulation(Nc, Nl, State) ->
  {L, C} = simulation:main(Nc, Nl),
  State#state{lpid = L, cpid = C}.
create_gui(F, Pid) ->
  wxWindow:show(F),
  ClText = wxTextCtrl:new(F, 0, [{pos, {0, 0}}, {value, "10"}, {size, {150,
    50}}]),
  LiText = wxTextCtrl:new(F, 1, [{pos, {160, 0}}, {value, "1000"}, {size, {150,
    50}}]),
  StartButton = wxButton:new(F, 2, [{label, "New"}, {pos, {0, 50}}, {size, {150, 50}}]),
  AddClientButton = wxButton:new(F, 3, [{label, "Add Client"}, {pos, {0, 100}}, {size, {150, 50}}]),
  AddBookButton = wxButton:new(F, 3, [{label, "Add Book"}, {pos, {0, 150}}, {size, {150, 50}}]),

  StopStartButton = wxButton:new(F, 4, [{label, "Stop"}, {pos, {0, 200}}, {size, {150, 50}}]),

  wxWindow:connect(F, close_window, [{callback,
    fun(Evt, Obj) ->
      io:format("koniec\n"),
      gen_server:cast(Pid, exit),
      wxWindow:destroy(F)

    end}]),
  wxButton:connect(AddClientButton, command_button_clicked, [{callback,
    fun(Evt, Obj) ->
      gen_server:cast(Pid, add_client)

    end
  }]),
  wxButton:connect(AddBookButton, command_button_clicked, [{callback,
    fun(Evt, Obj) ->
      gen_server:cast(Pid, add_book)

    end
  }]),
  wxButton:connect(StopStartButton, command_button_clicked, [{callback,
    fun(Evt, Obj) ->
      Label = wxButton:getLabel(StopStartButton),
      if
        Label =:= "Start" ->
          gen_server:cast(Pid, start),
          wxButton:setLabel(StopStartButton, "Stop");
        true ->
          gen_server:cast(Pid, stop),
          wxButton:setLabel(StopStartButton, "Start")


      end

    end
  }]),
  wxButton:connect(StartButton, command_button_clicked, [{callback,
    fun(Evt, Obj) ->
      X = utills:is_integer(wxTextCtrl:getValue(ClText)),
      Y = utills:is_integer(wxTextCtrl:getValue(LiText)),
      if
        X =:= true, Y =:= true -> gen_server:cast(Pid, {start, value(ClText), value(LiText)});
        true -> io:format("wrong parameters")
      end


    end
  }]).




value(Counter) ->
  V = wxTextCtrl:getValue(Counter),
  {I, _} = string:to_integer(V),
  I.




handle_cast({start, C, L}, State) ->
  {ok, T} = timer:send_interval(simulation:interval_milliseconds(), update_text),
  TS = set_timer(T, State),
  LP = get_lib(State),
  CP = get_client(State),
  if
    LP =/= undefined, CP =/= undefined ->   simulation:stop(CP, LP);
      true -> nothing
  end,

  {noreply, new_simulation(C, L, TS)};
handle_cast(add_client, State) ->
  P = get_client(State),
  client:add(P),
  {noreply, State};
handle_cast(add_book, State) ->
  P = get_lib(State),
  library:add_new_book(P),
  {noreply, State};

handle_cast(start, State) when State#state.cpid =/= undefined ->
  C = get_client(State),
  L = get_lib(State),
  simulation:start(C, L),
  {ok, T} = timer:send_interval(simulation:interval_milliseconds(), update_text),
  TS = set_timer(T, State),
  {noreply, TS};
handle_cast(stop, State) when State#state.cpid =/= undefined ->
  C = get_client(State),
  L = get_lib(State),
  simulation:stop(C, L),
  timer:cancel(get_timer(State)),
  {noreply, State};

handle_cast(start, State) -> {noreply, State};
handle_cast(stop, State) -> {noreply, State};
handle_cast(exit, State) ->
  {stop, shutdown, State}.
handle_info(update_text, State) ->
  LInfo = library:get_info(get_lib(State)),
  CInfo = client:info(get_client(State)),
  wx:set_env(get_env(State)),

  wxStaticText:destroy(get_text(State)),
  T = text(get_frame(State), CInfo ++ LInfo),
  {noreply, set_text(T, State)};
handle_info(interval, State) -> {noreply, State}.

start() ->
  Wx = wx:new(),
  F = wxFrame:new(Wx, -1, "Symulacja!"),
  Text = text(F, "Text"),
  Env = wx:get_env(),
  {ok, P} = start_link(F, Text, Env),

  create_gui(F, P).


text(F, Text) ->
  wxStaticText:new(F, 109, Text, [{pos, {400, 50}}]).

start_link(F, Text, Env) -> gen_server:start_link(?MODULE, new_state(F, Text, Env), []).

init(State) ->
  {ok, State}.



handle_call(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).