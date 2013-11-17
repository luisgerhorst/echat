-module(echat_room_manager).
-behaviour(gen_server).

-export([start_link/1, pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api

start_link(Sup) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	gen_server:cast(Pid, {start_room_sup, Sup}),
	register(echat_room_manager, Pid),
	io:format("Room manager started.~n"),
	{ok, Pid}.
	
pid(Name) ->
	io:format("Pid for ~p requested.~n", [Name]),
	gen_server:call(whereis(echat_room_manager), {name_to_pid, Name}).

% gen_server

init([]) -> {ok, undefined}.
	
handle_call({name_to_pid, NameSearched}, _From, RoomSup) ->
	Rooms = supervisor:which_children(RoomSup),
	Found = lists:foldl(fun ({{room, Name}, Pid, _Type, _CallbackMod}, Found) ->
		if
			Name =:= NameSearched -> Pid;
			true -> Found
		end
	end, false, Rooms),
	io:format("Room manager found room with matching name: ~p~n", [Found]),
	Pid = case Found of
		PidFound when is_pid(PidFound) -> % exists
			PidFound;
		undefined -> % existed but was terminated
			{ok, PidRestarted} = supervisor:restart_child(RoomSup, {room, NameSearched}),
			PidRestarted;
		_Nothing -> % never existed
			{ok, PidCreated} = supervisor:start_child(RoomSup, {
				{room, NameSearched},
				{echat_room, start_link, [NameSearched]},
				transient,
				1000,
				worker,
				[echat_room]
			}),
			PidCreated
	end,
	{reply, Pid, RoomSup};
handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room_manager ~p~n", [Msg]), {noreply, State}.

handle_cast({start_room_sup, Sup}, undefined) ->
	{ok, RoomSup} = supervisor:start_child(Sup, {
		room_sup,
		{echat_room_sup, start_link, []},
		permanent,
		10000,
		supervisor,
		[echat_room_sup]
	}),
	{noreply, RoomSup};
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room_manager ~p~n", [Msg]), {noreply, State}.

handle_info(Msg, State) -> io:format("Unexpected message to echat_room_manager ~p~n", [Msg]), {noreply, State}.

terminate(normal, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
		