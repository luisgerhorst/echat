-module(echat_stream_handler).

-export([new_message/3, members_change/4]).
-export([init/4, stream/3, info/3, terminate/2]).

-record(user, {
	username,
	rooms=[]
}).

% api

new_message(ConnectionPid, RoomName, Message) ->
	ConnectionPid ! {message, RoomName, Message}.
	
members_change(ConnectionPid, RoomName, Usernames, Action) ->
	ConnectionPid ! {members, RoomName, Usernames, Action}.

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("! New connection ~p~n", [self()]),
	{ok, Req, unregistered}. % State: {UserID, Nickname, Rooms}

stream(EncodedData, Req, User) ->
	try jiffy:decode(EncodedData) of
		{[
			{<<"type">>, Type},
			{<<"data">>, Data}
		]} ->
			handle(Type, Data, Req, User);
		Ejson ->
			io:format("Parsing message ~p resulted in invalid event ejson ~p~n", [EncodedData, Ejson]),
			res(none, Req, User)
	catch
		Exception:Reason ->
			io:format("Parsing message ~p cause error ~p ~p~n", [EncodedData, Exception, Reason]),
			res(none, Req, User)
	end.
	
info({members, Room, Usernames, {Action, Username}}, Req, User) -> % from room
	res(
		<<"users">>,
		{[
			{<<"room">>, Room},
			{<<"users">>, lists:reverse(Usernames)},
			{<<"action">>, Action},
			{<<"username">>, Username}
		]},
		Req,
		User
	);

info({message, Room, {Username, Content, Timestamp}}, Req, User) -> % from room
	io:format("Connection ~p received message.", [self()]),
	res(
		<<"message">>, 
		{[
			{<<"room">>, Room},
			{<<"message">>,
				{[
					{<<"username">>, Username},
					{<<"content">>, Content},
					{<<"timestamp">>, Timestamp}
				]}
			}
		]},
		Req,
		User
	);

info(Msg, Req, State) ->
	io:format("Unexpected info: ~p~n", [Msg]),
	{ok, Req, State}.

terminate(_Req, #user{username=Username,rooms=Rooms}) ->
	io:format("! Connection to ~p terminated, unsubscribing from rooms ~p~n", [Username, Rooms]),
	lists:map(fun (Room) ->
		echat_room:unsubscribe(Room, Username)
	end, Rooms),
	unregister_username(Username),
	ok;
terminate(_Req, State) ->
	io:format("! Connection terminated. State was ~p~n", [State]),
	ok.
	
% handle

% todo: user record instead of tuple

handle(<<"register">>, Username, Req, unregistered) ->
	io:format("! Received username req for ~p in connection ~p~n", [Username, self()]),
	{Available, NewState} = register_username(Username),
	res(
		<<"register_res">>,
		{[
			{<<"username">>, Username},
			{<<"accepted">>, Available}
		]},
		Req,
		NewState
	);
			
handle(<<"join">>, RoomMixedCase, Req, User = #user{
	username=Username,
	rooms=Rooms
}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p joins ~p~n", [self(), Room]),
	echat_room:join(Room, Username),
	res(
		none,
		Req,
		User#user{
			rooms=lists:merge(Rooms, [Room])
		}
	);
	
handle(<<"leave">>, RoomMixedCase, Req, User = #user{
	username=Username,
	rooms=Rooms
}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p leaves ~p~n", [self(), Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, User); true ->
		echat_room:leave(Room, Username),
		res(
			none,
			Req,
			User#user{
				rooms=lists:delete(Room, Rooms)
			}
		)
	end;
	
handle(<<"message">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"content">>, Content}
]}, Req, User = #user{
	username=Username,
	rooms=Rooms
}) when is_binary(Content) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Message from ~p with content ~p, connection pid ~p~n", [Username, Content, self()]),
	case lists:member(Room, Rooms) of false -> res(none, Req, User); true ->
		echat_room:message(Room, Username, Content),
		res(
			none,
			Req,
			User
		)
	end;
	
handle(<<"messages_before">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"timestamp">>, Timestamp}, % load everything before this timestamp
	{<<"limit">>, Limit}
]}, Req, User = #user{
	rooms=Rooms
}) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p requests ~p messages before ~p in room ~p~n", [self(), Limit, Timestamp, Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, User); true ->
		Messages = echat_room:messages_before(Room, Timestamp, Limit),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>,
			{[
				{<<"room">>, Room},
				{<<"messages">>, MessagesEjson}
			]},
			Req,
			User
		)
	end;
	
handle(<<"messages_between">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"startTimestamp">>, StartTimestamp}, % load everything before this timestamp
	{<<"endTimestamp">>, EndTimestamp}
]}, Req, User = #user{
	rooms=Rooms
}) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection to ~p requests messages between ~p and ~p in room ~p~n", [User#user.username, StartTimestamp, EndTimestamp, Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, User); true ->
		Messages = echat_room:messages_between(Room, StartTimestamp, EndTimestamp),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>, % custom or unique (with messages_before) event here?
			{[
				{<<"room">>, Room},
				{<<"messages">>, MessagesEjson}
			]},
			Req,
			User
		)
	end;
	
handle(Type, Data, Req, State) ->
	io:format("Unexpected event ~p with data ~p, State is ~p~n", [Type, Data, State]),
	res(none, Req, State).
	
% response

res(none, Req, State) -> {ok, Req, State}.

res(Type, Data, Req, State) when is_binary(Type) ->
	Res = jiffy:encode({[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]}),
	{reply, Res, Req, State}.

% private

lowercase(Binary) ->
	list_to_binary(string:to_lower(binary_to_list(Binary))).

message_tuples_to_ejson(Messages) ->
	[
		{[
			{<<"username">>, Username},
			{<<"content">>, Content},
			{<<"timestamp">>, Timestamp}
		]}
		|| {Username, Content, Timestamp}
		<- Messages
	].
	
register_username(Username) ->
	case ets:lookup(usernames, Username) of
		[] ->
			ets:insert(usernames, {Username}),
			{
				true,
				#user{
					username=Username
				}
			};
		[{Username}] ->
			{
				false,
				unregistered
			}
	end.
	
unregister_username(Username) ->
	ets:delete(usernames, Username).