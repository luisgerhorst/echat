-module(echat_room).
-behaviour(gen_server).

-export([start_link/1, save_message/4, get_messages_since/2, subscribe/1, unsubscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api

start_link(RoomName) ->
	gen_server:start_link(?MODULE, [RoomName], [{debug, [log]}]).

save_message(Name, Content, UserID, Nickname) ->
	gen_server:cast(echat_room_manager:get_pid(Name), {save_message, Content, UserID, Nickname}).

get_messages_since(Name, Timestamp) ->
	gen_server:call(echat_room_manager:get_pid(Name), {get_messages_since, Timestamp}).
	
subscribe(Name) ->
	gen_server:cast(echat_room_manager:get_pid(Name), {subscribe, self()}).
	
unsubscribe(Name) ->
	gen_server:cast(echat_room_manager:get_pid(Name), {unsubscribe, self()}).
	
% gen_server

init([RoomName]) ->
	{ok, {RoomName, [], []}}.

handle_call({get_messages_since, LatestMessageTimestamp}, _From, {RoomName, Subscribers,Messages}) ->
	MessagesDiff = [{Timestamp, Content, UserID, Nickname} || {Timestamp, Content, UserID, Nickname} <- Messages, LatestMessageTimestamp < Timestamp],
	{reply, MessagesDiff, {RoomName, Subscribers, Messages}};
handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_cast({save_message, Content, UserID, Nickname}, {RoomName, Subscribers, Messages}) ->
	NewMessage = {timestamp(), Content, UserID, Nickname},
	[Subscriber ! {new_message, RoomName, timestamp(), Content, UserID, Nickname} || Subscriber <- Subscribers],
	{noreply, {RoomName, Subscribers, [NewMessage|Messages]}};
handle_cast({subscribe, Pid}, {RoomName, Subscribers, Messages}) ->
	{noreply, {RoomName, [Pid|Subscribers], Messages}};
handle_cast({unsubscribe, Pid}, {RoomName, Subscribers, Messages}) ->
	{noreply, {RoomName, lists:delete(Pid, Subscribers), Messages}}; % stop or hibernate if no more subscribers
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_info(Msg, State) -> io:format("Unexpected message to echat_room ~p~n", [Msg]), {noreply, State}.
	
terminate(normal, _State) -> ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.