-module(echat_room).
-behaviour(gen_server).

-export([start_link/1, save_message/4, messages_since/2, subscribe/1, unsubscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MESSAGESLIMIT, 100).

% api

start_link(RoomName) ->
	gen_server:start_link(?MODULE, [RoomName], [{debug, [log]}]).

save_message(RoomName, UserID, Nickname, Content) ->
	gen_server:cast(echat_room_manager:get_pid(RoomName), {save_message, UserID, Nickname, Content}).

messages_since(RoomName, Timestamp) ->
	gen_server:call(echat_room_manager:get_pid(RoomName), {messages_since, Timestamp}).
	
subscribe(RoomName) ->
	gen_server:cast(echat_room_manager:get_pid(RoomName), {subscribe, self()}).
	
unsubscribe(RoomName) ->
	gen_server:cast(echat_room_manager:get_pid(RoomName), {unsubscribe, self()}).
	
% gen_server

init([RoomName]) ->
	{ok, {RoomName, [], []}}.

handle_call({messages_since, LatestTimestamp}, _From, {RoomName, ConnectionPids, Messages}) ->
	LimitedMessages = lists:sublist(Messages, 1, ?MESSAGESLIMIT),
	LimitedMessagesDiff = [{UserID, Nickname, Timestamp, Content} || {UserID, Nickname, Timestamp, Content} <- LimitedMessages, LatestTimestamp < Timestamp],
	{reply, LimitedMessagesDiff, {RoomName, ConnectionPids, Messages}};
handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_cast({save_message, UserID, Nickname, Content}, {RoomName, ConnectionPids, Messages}) ->
	NewMessage = {UserID, Nickname, timestamp(), Content},
	[echat_stream_handler:new_message(ConnectionPid, RoomName, NewMessage) || ConnectionPid <- ConnectionPids],
	{noreply, {
		RoomName,
		ConnectionPids,
		[NewMessage|Messages]
	}};
handle_cast({subscribe, ConnectionPid}, {RoomName, ConnectionPids, Messages}) ->
	{noreply, {RoomName, [ConnectionPid|ConnectionPids], Messages}};
handle_cast({unsubscribe, ConnectionPid}, {RoomName, ConnectionPids, Messages}) ->
	{noreply, {RoomName, lists:delete(ConnectionPid, ConnectionPids), Messages}}; % stop or hibernate if no more subscribers
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_info(Msg, State) -> io:format("Unexpected message to echat_room ~p~n", [Msg]), {noreply, State}.
	
terminate(normal, _State) -> ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.