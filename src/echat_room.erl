-module(echat_room).
-behaviour(gen_server).

-export([start_link/1, save_message/4, messages_since/2, subscribe/1, unsubscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MESSAGESLIMIT, 100). % max retuned messages of messages_since/2

-record(message, {
	user_id,
	nickname,
	timestamp=timestamp(),
	content
}).

-record(state, {
	room_name,
	subscribers=[],
	messages=[]
}).

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
	io:format("Starting room ~p~n", [RoomName]),
	% read messages from mnesia
	{ok, #state{room_name=RoomName}}.

handle_call({
	messages_since,
	LatestTimestamp
}, _From, State = #state{messages=Messages}) ->
	LimitedMessages = lists:sublist(Messages, 1, ?MESSAGESLIMIT),
	LimitedMessageTuplesDiff = [
		message_record_to_tuple(Message)
		|| Message = #message{timestamp=Timestamp}
		<- LimitedMessages
		, LatestTimestamp < Timestamp
	],
	{
		reply,
		LimitedMessageTuplesDiff,
		State
	};
handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_cast({
	save_message,
	UserID, 
	Nickname,
	Content
}, State = #state{
	messages=Messages,
	room_name=RoomName,
	subscribers=ConnectionPids
}) ->
	NewMessage = #message{user_id=UserID,nickname=Nickname,content=Content},
	[echat_stream_handler:new_message(ConnectionPid, RoomName, message_record_to_tuple(NewMessage)) || ConnectionPid <- ConnectionPids],
	{
		noreply,
		State#state{
			messages=[NewMessage|Messages]
		}
	};
handle_cast({
	subscribe,
	ConnectionPid
}, State = #state{
	subscribers=ConnectionPids
}) ->
	{
		noreply,
		State#state{
			subscribers=[ConnectionPid|ConnectionPids]
		}
	};
handle_cast({
	unsubscribe,
	ConnectionPid
}, State = #state{
	messages=Messages,
	room_name=RoomName,
	subscribers=ConnectionPids
}) ->
	case lists:delete(ConnectionPid, ConnectionPids) of
		[] -> {
				stop,
				normal,
				{no_subscribers, RoomName, Messages}
				% todo: call room manager to stop me instead?
			};
		NewConnectionPids -> {
				noreply,
				State#state{
					subscribers=NewConnectionPids
				}
			}
	end;
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_info(Msg, State) -> io:format("Unexpected message to echat_room ~p~n", [Msg]), {noreply, State}.

terminate(normal, {no_subscribers, RoomName, _Messages}) ->
	io:format("Room with name ~p terminated because of no subscribers.~n", [RoomName]),
	ok;
terminate(_Other, State) ->
	io:format("Error: Room with state ~p and pid ~p terminated.~n", [State, self()]),
	ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

message_record_to_tuple(#message{
	user_id=UserID,
	nickname=Nickname,
	timestamp=Timestamp,
	content=Content
}) -> {UserID, Nickname, Timestamp, Content}.

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.