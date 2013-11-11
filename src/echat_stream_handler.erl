-module(echat_stream_handler).

-export([new_message/3, members_change/4]).
-export([init/4, stream/3, info/3, terminate/2]).

% api

new_message(Pid, Room, Message) ->
	Pid ! {message, Room, Message}.
	
members_change(Pid, Room, Usernames, Action) ->
	Pid ! {members, Room, Usernames, Action}.

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("! New connection ~p~n", [self()]),
	{ok, Req, unregistered}. % State: {UserID, Nickname, Rooms}

stream(EncodedData, Req, State) ->
	try jiffy:decode(EncodedData) of
		{[
			{<<"type">>, Type},
			{<<"data">>, Data}
		]} ->
			handle(Type, Data, Req, State);
		Ejson ->
			io:format("Parsing message ~p resulted in invalid event ejson ~p~n", [EncodedData, Ejson]),
			res(none, Req, State)
	catch
		Exception:Reason ->
			io:format("Parsing message ~p cause error ~p ~p~n", [EncodedData, Exception, Reason]),
			res(none, Req, State)
	end.
	
info({members, Room, Usernames, {Action, Username}}, Req, State = {registered, _Username, _Rooms}) ->
	res(
		<<"users">>,
		{[
			{<<"room">>, Room},
			{<<"users">>, lists:reverse(Usernames)},
			{<<"action">>, Action},
			{<<"username">>, Username}
		]},
		Req,
		State
	);

info({message, Room, {Username, Content, Timestamp}}, Req, State = {registered, _Username, _Rooms}) ->
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
		State
	);

info(Msg, Req, State) ->
	io:format("Unexpected info ~p at state ~p~n", [Msg, State]),
	{ok, Req, State}.

terminate(_Req, {registered, Username, Rooms}) ->
	io:format("! Connection to ~p terminated, unsubscribing from rooms ~p~n", [Username, Rooms]),
	lists:foreach(fun (Room) ->
		echat_room:unsubscribe(Room, Username)
	end, Rooms),
	unregister_username(Username),
	ok;
terminate(_Req, State) ->
	io:format("! Connection terminated. State was ~p~n", [State]),
	ok.
	
% handle

handle(<<"register">>, Username, Req, unregistered) ->
	io:format("! Received username req for ~p in connection ~p~n", [Username, self()]),
	{Accepted, NewState} = register_username(Username),
	io:format("Accepted ~p, new state ~p~n", [Accepted, NewState]),
	res(
		<<"register_res">>,
		{[
			{<<"username">>, Username},
			{<<"accepted">>, Accepted}
		]},
		Req,
		NewState
	);
			
handle(<<"join">>, RoomMixedCase, Req, {registered, Username, Rooms}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p joins ~p~n", [self(), Room]),
	echat_room:join(Room, Username),
	res(
		none,
		Req,
		{registered, Username, lists:merge(Rooms, [Room])}
	);
	
% don't forget to check if user is room member when handling the following requests
	
handle(<<"leave">>, RoomMixedCase, Req, State = {registered, Username, Rooms}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p leaves ~p~n", [self(), Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, State); true ->
		echat_room:leave(Room, Username),
		res(
			none,
			Req,
			{registered, Username, lists:delete(Room, Rooms)}
		)
	end;
	
handle(<<"message">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"content">>, Content}
]}, Req, State = {registered, Username, Rooms}) when is_binary(Content) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Message from ~p with content ~p, connection pid ~p~n", [Username, Content, self()]),
	case lists:member(Room, Rooms) of false -> res(none, Req, State); true ->
		echat_room:message(Room, Username, Content),
		res(
			none,
			Req,
			State
		)
	end;
	
handle(<<"messages_before">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"timestamp">>, Timestamp}, % load everything before this timestamp
	{<<"limit">>, Limit}
]}, Req, State = {registered, _Username, Rooms}) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection ~p requests ~p messages before ~p in room ~p~n", [self(), Limit, Timestamp, Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, State); true ->
		Messages = echat_room:messages_before(Room, Timestamp, Limit),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>,
			{[
				{<<"room">>, Room},
				{<<"messages">>, MessagesEjson}
			]},
			Req,
			State
		)
	end;
	
handle(<<"messages_between">>, {[
	{<<"room">>, RoomMixedCase},
	{<<"startTimestamp">>, StartTimestamp}, % load everything before this timestamp
	{<<"endTimestamp">>, EndTimestamp}
]}, Req, State = {registered, Username, Rooms}) ->
	Room = lowercase(RoomMixedCase),
	io:format("! Connection to ~p requests messages between ~p and ~p in room ~p~n", [Username, StartTimestamp, EndTimestamp, Room]),
	case lists:member(Room, Rooms) of false -> res(none, Req, State); true ->
		Messages = echat_room:messages_between(Room, StartTimestamp, EndTimestamp),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>, % custom or unique (with messages_before) event here?
			{[
				{<<"room">>, Room},
				{<<"messages">>, MessagesEjson}
			]},
			Req,
			State
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

% usernames
	
register_username(Username) ->
	case ets:lookup(usernames, Username) of
		[] ->
			ets:insert(usernames, {Username}),
			{
				true,
				{registered, Username, []}
			};
		[{Username}] ->
			{
				false,
				unregistered
			}
	end.
	
unregister_username(Username) ->
	ets:delete(usernames, Username).