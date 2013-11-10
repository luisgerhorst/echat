-module(echat_room).
-behaviour(gen_server).

-export([start_link/1, save_message/3, load_messages_before/3, join/2, leave/2, unsubscribe/2, member_delete_timeout/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(USER_RECONNECT_TIMEOUT, 10000). % not sure if good, but 1000 was not enough. ask someone or look at bullet.js

-record(room, {
	name,
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
	gen_server:cast(echat_room_manager:get_pid(Room), {disconnected, Username}).
	
member_delete_timeout(RoomPid, Username) ->
	io:format("Remover for ~p started~n", [Username]),
	timer:sleep(?USER_RECONNECT_TIMEOUT),
	gen_server:cast(RoomPid, {member_delete_timeout, Username}).
	
% gen_server

init([Room]) ->
	io:format("Starting room ~p~n", [Room]),
	% read messages from mnesia
	{ok, #room{
		name=Room
	}}.

handle_call({
	messages_before,
	Timestamp,
	Limit
}, _From, State = #room{
	name=Room
}) ->
	{
		reply,
		echat_messages:read(Room, Timestamp, Limit),
		State
	};

handle_call(Msg, _From, State) -> io:format("Unexpected call to echat_room ~p~n", [Msg]), {noreply, State}.
	
handle_cast({
	message,
	Username,
	Content
}, State = #room{
	name=Room,
	members=Members
}) ->
	NewMessage = echat_messages:new(Room, Username, Content),
	send_message_to_subscribers(NewMessage, Members, Room),
	echat_messages:save(NewMessage),
	{
		noreply,
		State
	};

handle_cast({
	join,
	Username,
	Pid
}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("User ~p joins room ...~n", [Username]),
	case proplists:get_value(Username, Members) of
		disconnected ->
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
			io:format("New User ~p ~p.~n", [Username, Pid]),
			MemberAdded = [{Username, Pid}|Members],
			send_users_to_subscribers(MemberAdded, Room),
			{
				noreply,
				State#room{
					members=MemberAdded
				}
			}
	end;

handle_cast({
	leave,
	Username
}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("User ~p left room. Removing user.~n", [Username]),
	MemberDeleted = proplists:delete(Username, Members),
	send_users_to_subscribers(MemberDeleted, Room),
	{
		noreply,
		State#room{
			members=MemberDeleted
		}
	};
	
handle_cast({
	disconnected,
	Username
}, State = #room{
	members=Members
}) ->
	io:format("Unsubscribe ~p (connection terminated) marking user and starting remover~n", [Username]),
	Deleted = proplists:delete(Username, Members),
	Marked = [{Username, disconnected}|Deleted], % mark user as disconnected
	spawn(echat_room, member_delete_timeout, [self(), Username]), % start delete timeout for disconnected user
	{
		noreply,
		State#room{
			members=Marked
		}
	};
	
handle_cast({
	member_delete_timeout,
	Username
}, State = #room{
	members=Members,
	name=Room
}) ->
	io:format("Reconnect Timeout for user ~p~n", [Username]),
	case proplists:get_value(Username, Members) of
		disconnected -> % hasn't reconnected
			io:format("-> NOT reconnected, remove~n"),
			MemberDeleted = proplists:delete(Username, Members),
			send_users_to_subscribers(MemberDeleted, Room),
			{
				noreply,
				State#room{
					members= MemberDeleted % remove
				}
			};
		_Pid -> % reconnected
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

send_message_to_subscribers(Message, Members, Room) ->
	io:format("sending new messages to members ~p~n", [Members]),
	RelativeMessage = echat_messages:convert_to_relative_tuple(Message),
	each_member_pid(fun (Pid) ->
		echat_stream_handler:new_message(Pid, Room, RelativeMessage)
	end, Members).
	
send_users_to_subscribers(Members, Room) ->
	Usernames = [ Username || {Username, _State} <- Members ],
	each_member_pid(fun (Pid) ->
		echat_stream_handler:members_change(Pid, Room, Usernames)
	end, Members).
	
each_member_pid(Fun, Members) ->
	lists:map(fun ({_Username, Connection}) ->
		case Connection of
			disconnected ->
				{error, disconnected};
			Pid ->
				{ok, Fun(Pid)}
		end
	end, Members).