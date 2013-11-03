-module(echat_room_manager).
-behaviour(gen_server).

-export([start_link/1, get_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api

start_link(Sup) ->
	io:format("starting manager sup is ~p~n", [Sup]),
	{ok, Pid} = gen_server:start_link(?MODULE, [], [{debug, [log]}]),
	io:format("got manager pid ~p~n", [Pid]),
	gen_server:cast(Pid, {start_room_sup, Sup}),
	register(echat_room_manager, Pid),
	io:format("manager started~n"),
	{ok, Pid}.
	
get_pid(Name) ->
	io:format("get pid for ~p~n", [Name]),
	gen_server:call(whereis(echat_room_manager), {get_room, Name}).

% gen_server

init([]) ->
	io:format("manager init~n"),
	{ok, undefined}.
	
handle_call({get_room, Name}, _From, RoomSup) ->
	Pid = get_pid(Name, RoomSup),
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

% private

get_pid(SearchedName, RoomSup) ->
	Rooms = supervisor:which_children(RoomSup),
	FoundPid = lists:foldl(fun ({{room, Name}, Pid, _Type, _CallbackMod}, Contains) -> 
		NewContains = if
			Name =:= SearchedName -> Pid;
			true -> Contains
		end
	end, false, Rooms),
	case FoundPid of
		FoundPid when is_pid(FoundPid) -> FoundPid;
		FoundPid ->
			{ok, CreatedPid} = supervisor:start_child(RoomSup, {
				{room, SearchedName},
				{echat_room, start_link, [SearchedName]},
				transient,
				1000,
				worker,
				[echat_room]
			}),
			CreatedPid
	end.
		