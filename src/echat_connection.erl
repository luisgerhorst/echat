-module(echat_connection).

-export([message/3, user/3]).
-export([init/4, stream/3, info/3, terminate/2]).

% api

message(Pid, Room, Message) ->
	Pid ! {message, Room, Message}.
	
user(Pid, Room, Action) ->
	io:format("Members change: pid ~p room ~p action ~p~n", [Pid, Room, Action]),
	Pid ! {user, Room, Action}.

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("! New connection ~p~n", [self()]),
	{ok, Req, unregistered}.

stream(EncodedData, Req, State) ->
	try jiffy:decode(EncodedData) of
		{[
			{<<"type">>, Type},
			{<<"data">>, Data},
			{<<"ref">>, Ref}
		]} ->
			handle(Type, Data, Ref, Req, State);
		Ejson ->
			io:format("Parsing message ~p resulted in invalid event ejson ~p~n", [EncodedData, Ejson]),
			res(none, Req, State)
	catch
		Exception:Reason ->
			io:format("Parsing message ~p cause error ~p ~p~n", [EncodedData, Exception, Reason]),
			res(none, Req, State)
	end.
	
info({user, Room, {Action, Username}}, Req, State = {registered, _Username, _Rooms}) ->
	res(
		<<"user">>,
		{[
			{<<"room">>, Room},
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
		echat_room:disconnected(Room, Username)
	end, Rooms),
	unregister_username(Username),
	ok;
terminate(_Req, State) ->
	io:format("! Connection terminated. State was ~p~n", [State]),
	ok.
	
% handle

handle(<<"register">>, Username, Ref, Req, unregistered) when is_binary(Username) ->
	io:format("! Received username req for ~p in connection ~p~n", [Username, self()]),
	{Accepted, NewState} = register_username(Username),
	io:format("Accepted ~p, new state ~p~n", [Accepted, NewState]),
	res(
		<<"register_res">>,
		Accepted,
		Ref,
		Req,
		NewState
	);
			
handle(Type = <<"join">>, Data = RoomMixedCase, Ref, Req, State = {registered, Username, Rooms}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	IsMember = lists:member(Room, Rooms),
	if IsMember ->
		io:format("Unexpected event ~p with data ~p, State is ~p. Already room member!~n", [Type, Data, State]),
		res(none, Req, State);
	not IsMember ->
		io:format("! Connection ~p joins ~p~n", [self(), Room]),
		Usernames = echat_room:join(Room, Username),
		res(
			<<"users">>,
			Usernames,
			Ref,
			Req,
			{registered, Username, lists:merge(Rooms, [Room])}
		)
	end;
	
handle(Type = <<"leave">>, Data = RoomMixedCase, _Ref, Req, State = {registered, Username, Rooms}) when is_binary(RoomMixedCase) ->
	Room = lowercase(RoomMixedCase),
	IsMember = lists:member(Room, Rooms),
	if not IsMember ->
		io:format("Unexpected event ~p with data ~p, State is ~p. No room member!~n", [Type, Data, State]),
		res(none, Req, State);
	IsMember ->
		io:format("! Connection ~p leaves ~p~n", [self(), Room]),
		echat_room:leave(Room, Username),
		res(
			none,
			Req,
			{registered, Username, lists:delete(Room, Rooms)}
		)
	end;
	
handle(Type = <<"message">>, Data = {[
	{<<"room">>, RoomMixedCase},
	{<<"content">>, Content}
]}, Ref, Req, State = {registered, Username, Rooms}) when is_binary(RoomMixedCase), is_binary(Content) ->
	Room = lowercase(RoomMixedCase),
	IsMember = lists:member(Room, Rooms),
	if not IsMember ->
		io:format("Unexpected event ~p with data ~p, State is ~p. No room member!~n", [Type, Data, State]),
		res(none, Req, State);
	IsMember ->
		io:format("! Message from ~p with content ~p, connection pid ~p~n", [Username, Content, self()]),
		Timestamp = echat_room:message(Room, Username, Content),
		res(
			<<"message_timestamp">>,
			Timestamp,
			Ref,
			Req,
			State
		)
	end;
	
handle(Type = <<"messages_before">>, Data = {[
	{<<"room">>, RoomMixedCase},
	{<<"timestamp">>, Timestamp}, % load everything before this timestamp
	{<<"limit">>, Limit}
]}, Ref, Req, State = {registered, _Username, Rooms}) when is_binary(RoomMixedCase), is_number(Timestamp), is_integer(Limit) ->
	Room = lowercase(RoomMixedCase),
	IsMember = lists:member(Room, Rooms),
	if not IsMember ->
		io:format("Unexpected event ~p with data ~p, State is ~p. No room member!~n", [Type, Data, State]),
		res(none, Req, State);
	IsMember ->
		io:format("! Connection ~p requests ~p messages before ~p in room ~p~n", [self(), Limit, Timestamp, Room]),
		Messages = echat_room:messages_before(Room, Timestamp, Limit),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>,
			MessagesEjson,
			Ref,
			Req,
			State
		)
	end;
	
handle(Type = <<"messages_between">>, Data = {[
	{<<"room">>, RoomMixedCase},
	{<<"startTimestamp">>, StartTimestamp},
	{<<"endTimestamp">>, EndTimestamp}
]}, Ref, Req, State = {registered, Username, Rooms}) when is_binary(RoomMixedCase), is_number(StartTimestamp), is_number(EndTimestamp) ->
	Room = lowercase(RoomMixedCase),
	IsMember = lists:member(Room, Rooms),
	if not IsMember ->
		io:format("Unexpected event ~p with data ~p, State is ~p. No room member!~n", [Type, Data, State]),
		res(none, Req, State);
	IsMember ->
		io:format("! Connection to ~p requests messages between ~p and ~p in room ~p~n", [Username, StartTimestamp, EndTimestamp, Room]),
		Messages = echat_room:messages_between(Room, StartTimestamp, EndTimestamp),
		MessagesEjson = message_tuples_to_ejson(Messages),
		res(
			<<"messages">>,
			MessagesEjson,
			Ref,
			Req,
			State
		)
	end;
	
handle(Type, Data, Ref, Req, State) ->
	io:format("Unexpected event ~p (ref ~p) with data ~p, State is ~p.~n", [Type, Ref, Data, State]),
	res(none, Req, State).
	
% response

res(none, Req, State) -> {ok, Req, State}.

res(Type, Data, Req, State) when is_binary(Type) ->
	Res = jiffy:encode({[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]}),
	{reply, Res, Req, State}.
	
res(Type, Data, Ref, Req, State) when is_binary(Type), is_integer(Ref) ->
	Res = jiffy:encode({[
		{<<"type">>, Type},
		{<<"data">>, Data},
		{<<"ref">>, Ref}
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