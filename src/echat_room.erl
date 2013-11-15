-module(echat_room).
-behaviour(gen_server).

-export([
	start_link/1,
	join/2, leave/2, unsubscribe/2, reconnect_timeout/2,
	message/3, messages_before/3, messages_between/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RECONNECT_TIMEOUT, 10000). % not sure if good, but 1000 was not enough. ask someone or look at bullet.js

-record(room, {
	name,
	members=[]
}).

% api

start_link(Room) ->
	gen_server:start_link(?MODULE, [Room], []).
	
join(Room, Username) ->
	gen_server:call(echat_room_manager:pid(Room), {join, Username}).
	
leave(Room, Username) ->
	gen_server:cast(echat_room_manager:pid(Room), {leave, Username}).
	
unsubscribe(Room, Username) ->
	gen_server:cast(echat_room_manager:pid(Room), {disconnected, Username}).
	
reconnect_timeout(RoomPid, Username) ->
	io:format("Remover for ~p started~n", [Username]),
	timer:sleep(?RECONNECT_TIMEOUT),
	gen_server:cast(RoomPid, {reconnect_timeout, Username}).

message(Room, Username, Content) ->
	gen_server:call(echat_room_manager:pid(Room), {message, Username, Content}).

messages_before(Room, Timestamp, Limit) ->
	gen_server:call(echat_room_manager:pid(Room), {messages_before, Timestamp, Limit}).

messages_between(Room, StartTimestamp, EndTimestamp) ->
	gen_server:call(echat_room_manager:pid(Room), {messages_between, StartTimestamp, EndTimestamp}).
	
% gen_server

init([Room]) ->
	io:format("Starting room ~p~n", [Room]),
	% read messages from mnesia
	{ok, #room{
		name=Room
	}}.

handle_cast({
	leave,
	Username
}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("User ~p left room. Removing user.~n", [Username]),
	Deleted = proplists:delete(Username, Members),
	send_user(Room, Deleted, {leave, Username}),
	case Deleted of
		[] ->
			io:format("All members disconnected, stopping room.~n"),
			{
				stop,
				normal,
				{no_members, Room}
			};
		NewMembers ->
			{
				noreply,
				State#room{
					members=NewMembers
				}
			}
	end;

handle_cast({
	disconnected,
	Username
}, State = #room{
	members=Members
}) ->
	io:format("Unsubscribe ~p (connection terminated) marking user and starting remover~n", [Username]),
	Deleted = proplists:delete(Username, Members),
	Marked = [{Username, disconnected}|Deleted], % mark user as disconnected
	spawn(echat_room, reconnect_timeout, [self(), Username]), % start delete timeout for disconnected user
	{
		noreply,
		State#room{
			members=Marked
		}
	};

handle_cast({
	reconnect_timeout,
	Username
}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("Reconnect Timeout for user ~p~n", [Username]),
	case proplists:get_value(Username, Members) of
		disconnected -> % hasn't reconnected
			io:format("-> NOT reconnected, remove~n"),
			Deleted = proplists:delete(Username, Members),
			send_user(Room, Deleted, {leave, Username}),
			case Deleted of
				[] ->
					io:format("All members disconnected, stopping room.~n"),
					{
						stop,
						normal,
						{no_members, Room}
					};
				Deleted ->
					{
						noreply,
						State#room{
							members=Deleted
						}
					}
			end;
		_Pid -> % reconnected
			io:format("-> has been reconnected~n"),
			{
				noreply,
				State
			}
	end;
	
handle_cast(Msg, State) -> io:format("Unexpected cast to echat_room ~p~n", [Msg]), {noreply, State}.

handle_call({
	message,
	Username,
	Content
}, _From, State = #room{
	name=Room,
	members=Members
}) ->
	NewMessage = echat_messages:new(Room, Username, Content),
	send_message(Room, Members, NewMessage),
	echat_messages:save(NewMessage),
	{
		reply,
		echat_messages:timestamp(NewMessage),
		State
	};

handle_call({
	join,
	Username
}, {ConnectionPid, _Ref}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("User ~p joins room ...~n", [Username]),
	case proplists:get_value(Username, Members) of
		disconnected ->
			io:format("User was marked, I'll fix that.~n"),
			DeletedMarked = proplists:delete(Username, Members),
			Reactivated = [{Username, ConnectionPid}|DeletedMarked],
			{
				reply,
				lists:reverse([ Username || {Username, _Connection} <- Reactivated ]),
				State#room{
					members=Reactivated
				}
			};
		undefined ->
			io:format("New User ~p ~p.~n", [Username, ConnectionPid]),
			Added = [{Username, ConnectionPid}|Members],
			send_user(Room, Added, {join, Username}),
			{
				reply,
				lists:reverse([ Username || {Username, _Connection} <- Added ]),
				State#room{
					members=Added
				}
			};
		Pid ->
			io:format("Unexpected join, username ~p already associated with pid ~p~n", [Username, Pid]),
			{
				noreply,
				State
			}
	end;

handle_call({
	messages_before,
	Timestamp,
	Limit
}, _From, State = #room{
	name=Room
}) ->
	{
		reply,
		echat_messages:before(Room, Timestamp, Limit),
		State
	};
	
handle_call({
	messages_between,
	StartTimestamp,
	EndTimestamp
}, _From, State = #room{
	name=Room
}) ->
	{
		reply,
		echat_messages:between(Room, StartTimestamp, EndTimestamp),
		State
	};

handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_info(Msg, State) -> io:format("Unexpected message to echat_room ~p~n", [Msg]), {noreply, State}.

terminate(normal, {no_members, Room}) ->
	io:format("Room with name ~p terminated because of no members.~n", [Room]),
	ok;
terminate(_Other, State) ->
	io:format("Error: Room with state ~p and pid ~p terminated.~n", [State, self()]),
	ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

send_message(Room, Members, Message) ->
	io:format("sending new messages to members ~p~n", [Members]),
	RelativeMessage = {SenderUsername, _Content, _Timestamp} = echat_messages:convert_to_relative_tuple(Message),
	each_member_pid(fun (Pid, Username) ->
		if
			SenderUsername =:= Username -> ok;
			true -> echat_stream_handler:new_message(Pid, Room, RelativeMessage)
		end
	end, Members).
	
send_user(Room, Members, Action = {_Type, PerformingUsername}) ->
	each_member_pid(fun (Pid, Username) ->
		if
			PerformingUsername =:= Username -> ok;
			true -> echat_stream_handler:members_change(Pid, Room, Action)
		end
	end, Members).
	
each_member_pid(Fun, Members) ->
	lists:foreach(fun ({Username, Connection}) ->
		case Connection of
			disconnected -> ok;
			Pid -> Fun(Pid, Username)
		end
	end, Members).