-module(echat_stream_handler).

-export([new_message/3, members_change/3]).
-export([init/4, stream/3, info/3, terminate/2]).

-record(user, {
	username,
	rooms=[]
}).

% api

new_message(ConnectionPid, RoomName, Message) ->
	ConnectionPid ! {message, RoomName, Message}.
	
members_change(ConnectionPid, RoomName, Usernames) ->
	ConnectionPid ! {members, RoomName, Usernames}.

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
	
info({members, Room, Usernames}, Req, User) -> % from room
	res(
		<<"users">>,
		{[
			{<<"room">>, Room},
			{<<"users">>, lists:reverse(Usernames)}
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
	un_register(Username),
	ok;
terminate(_Req, State) ->
	io:format("! Connection terminated. State was ~p~n", [State]),
	ok.
	
% handle

% todo: user record instead of tuple

handle(<<"register">>, Username, Req, unregistered) ->
	io:format("! Received username req for \"~p\" in connection ~p~n", [Username, self()]),
	{Available, NewState} = register(Username),
	res(
		<<"register_res">>,
		{[
			{<<"username">>, Username},
			{<<"available">>, Available}
		]},
		Req,
		NewState
	);
			
handle(<<"join">>, RoomMixedCase, Req, User = #user{username=Username,rooms=Rooms}) when is_binary(RoomMixedCase) ->
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
	
handle(<<"leave">>, RoomMixedCase, Req, User = #user{username=Username,rooms=Rooms}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p leaves ~p~n", [self(), Room]),
	echat_room:leave(Room, Username),
	res(
		none,
		Req,
		User#user{
			rooms=lists:delete(Room, Rooms)
		}
	);
	
handle(<<"load_messages">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"timestamp">>, Timestamp}, % load everything before this timestamp
	{<<"limit">>, Limit}
]}, Req, User) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p requests ~p messages before ~p in room ~p~n", [self(), Limit, Timestamp, Room]),
	Messages = echat_room:load_messages_before(Room, Timestamp, Limit),
	MessagesEjson = message_tuples_to_ejson(Messages),
	res(
		<<"messages">>,
		{[
			{<<"room">>, Room},
			{<<"messages">>, MessagesEjson}
		]},
		Req,
		User
	);
	
handle(<<"message">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"content">>, Content}
]}, Req, User = #user{
	username=Username
}) when is_binary(Content) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Message from ~p with content ~p, connection pid ~p~n", [Username, Content, self()]),
	% check if Rooms contain Room?
	echat_room:save_message(Room, Username, Content),
	res(
		none,
		Req,
		User
	);
	
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
	
register(Username) ->
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
	
un_register(Username) ->
	ets:delete(usernames, Username).