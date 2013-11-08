-module(echat_room).
-behaviour(gen_server).

-export([start_link/1, save_message/3, load_messages_before/3, join/2, leave/2, unsubscribe/2, member_remover/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(USER_RECONNECT_TIMEOUT, 10000).

-record(message, {
	username,
	timestamp=timestamp(),
	content
}).

-record(room, {
	name,
	subscribers=[],
	members=[]
}).

% api

start_link(Room) ->
	gen_server:start_link(?MODULE, [Room], [{debug, [log]}]).

save_message(Room, Username, Content) ->
	gen_server:cast(echat_room_manager:get_pid(Room), {message, Username, Content}).
	
load_messages_before(Room, Timestamp, Limit) ->
	gen_server:call(echat_room_manager:get_pid(Room), {messages_before, Timestamp, Limit}).
	
join(Room, Username) ->
	gen_server:cast(echat_room_manager:get_pid(Room), {join, Username, self()}).
	
leave(Room, Username) ->
	gen_server:cast(echat_room_manager:get_pid(Room), {leave, Username}).
	
unsubscribe(Room, Username) ->
	gen_server:cast(echat_room_manager:get_pid(Room), {unsubscribe, Username}).
	
member_remover(RoomPid, Username) ->
	io:format("Remover for ~p started~n", [Username]),
	timer:sleep(?USER_RECONNECT_TIMEOUT),
	gen_server:cast(RoomPid, {remover_fired, Username}).
	
% gen_server

init([Room]) ->
	io:format("Starting room ~p~n", [Room]),
	% read messages from mnesia
	{ok, #room{
		name=Room
	}}.

handle_call({
	messages_before,
	_Timestamp,
	_Limit
}, _From, Room) ->
	% get messages before Timestamp with Limit
	{
		reply,
		[], % list of message tuples
		Room
	};

handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_cast({
	message,
	Username,
	Content
}, Room = #room{
	name=Name,
	subscribers=Subscribers
}) ->
	NewMessage = #message{username=Username,content=Content},
	send_message_to_subscribers(NewMessage, Subscribers, Name),
	{
		noreply,
		Room
	};

handle_cast({
	join,
	Username,
	Pid
}, State = #room{
	members=Members
}) ->
	io:format("User ~p joins room ...~n", [Username]),
	case proplists:get_value(Username, Members) of
		unsubscribed ->
			io:format("User was marked, I'll fix that.~n"),
			DeletedMarked = proplists:delete(Username, Members),
			Reactivated = [{Username, Pid}|DeletedMarked],
			{
				noreply,
				State#room{
					members=Reactivated
				}
			};
		undefined ->
			io:format("New User.~n"),
			{
				noreply,
				State#room{
					members=[{Username, Pid}|Members]
				}
			}
	end;

handle_cast({
	leave,
	Username
}, State = #room{
	members=Members
}) ->
	io:format("User ~p left room. Removing user.~n", [Username]),
	{
		noreply,
		State#room{
			members=proplists:delete(Username, Members) % remove
		}
	};
	
handle_cast({
	unsubscribe,
	Username
}, State = #room{
	members=Members
}) ->
	io:format("Unsubscribe ~p (connection terminated) marking user and starting remover~n", [Username]),
	Deleted = proplists:delete(Username, Members),
	Marked = [{Username, unsubscribed}|Deleted], % mark user as disconnected
	spawn(echat_room, member_remover, [self(), Username]), % start remover for disconnected user
	{
		noreply,
		State#room{
			members=Marked
		}
	};
	
handle_cast({
	remover_fired,
	Username
}, State = #room{
	members=Members
}) ->
	io:format("Reconnect Timeout for user ~p~n", [Username]),
	case proplists:get_value(Username, Members) of
		unsubscribed -> % hasn't reconnected
			io:format("-> NOT reconnected, remove~n"),
			{
				noreply,
				State#room{
					members=proplists:delete(Username, Members) % remove
				}
			};
		Pid when is_pid(Pid) -> % connected again
			io:format("-> has been reconnected~n"),
			{
				noreply,
				State
			}
	end;
	
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_info(Msg, State) -> io:format("Unexpected message to echat_room ~p~n", [Msg]), {noreply, State}.

terminate(normal, {no_subscribers, RoomName}) ->
	io:format("Room with name ~p terminated because of no subscribers.~n", [RoomName]),
	ok;

terminate(_Other, State) ->
	io:format("Error: Room with state ~p and pid ~p terminated.~n", [State, self()]),
	ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

send_message_to_subscribers(Message, Subscribers, Room) ->
	[echat_stream_handler:new_message(Member, Room, message_record_to_tuple(Message)) || Member <- Subscribers].

message_record_to_tuple(#message{
	username=Username,
	timestamp=Timestamp,
	content=Content
}) -> {Username, Content, Timestamp}.

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.