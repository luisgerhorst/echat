-module(echat_stream_handler).

-export([new_message/3]).
-export([init/4, stream/3, info/3, terminate/2]).

% api

new_message(ConnectionPid, RoomName, Message) ->
	ConnectionPid ! {new_message, RoomName, Message}.

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("new connection~n"),
	{ok, Req, undefined}. % State: {UserID, Nickname, Rooms}

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

info({new_message, RoomName, {UserID, Nickname, Timestamp, Content}}, Req, User) -> % from room
	res(<<"new_message">>, {[
		{<<"room">>, RoomName},
		{<<"message">>,
			{[
				{<<"timestamp">>, Timestamp},
				{<<"content">>, Content},
				{<<"userID">>, UserID},
				{<<"nickname">>, Nickname}
			]}
		}
	]}, Req, User);
info(Msg, Req, State) ->
	io:format("Unexpected info: ~p~n", [Msg]),
	{ok, Req, State}.

terminate(_Req, {_UserID, _Nickname, RoomNames}) when is_list(RoomNames) ->
	io:format("Controlled handler disconnect, unsubscribing from rooms ~p ~n", [RoomNames]),
	lists:map(fun echat_room:unsubscribe/1, RoomNames),
	ok;
terminate(Req, User) ->
	io:format("Uncontrolled handler terminate, Req ~p user is ~p~n", [Req, User]),
	ok.
	
% handle

% todo: user record instead of tuple

handle(<<"user">>, {[
	{<<"id">>, UserID},
	{<<"nickname">>, Nickname},
	{<<"rooms">>, RoomsInfo}
]}, Req, _User) when is_integer(UserID), is_binary(Nickname) ->
	io:format("rooms info ~p~n", [RoomsInfo]),
	res(
		<<"rooms_old_messages">>, 
		user_res(RoomsInfo), 
		Req,
		{
			UserID,
			Nickname,
			extract_room_names(RoomsInfo)
		}
	);
handle(<<"join">>, RoomName, Req, {UserID, Nickname, RoomNames}) when is_binary(RoomName) ->
	echat_room:subscribe(RoomName),
	res(
		<<"room_old_messages">>,
		join_res(RoomName),
		Req,
		{UserID, Nickname, [RoomName|RoomNames]}
	);
handle(<<"leave">>, RoomName, Req, User) when is_binary(RoomName) ->
	echat_room:unsubscribe(RoomName),
	res(none, Req, User);
handle(<<"message">>, {[
	{<<"room">>, RoomName},
	{<<"content">>, Content}
]}, Req, {UserID, Nickname, _RoomNames} = User) when is_binary(RoomName), is_binary(Content) ->
	io:format("content ~p~n", [Content]),
	echat_room:save_message(RoomName, UserID, Nickname, Content),
	res(none, Req, User);
handle(Type, Data, Req, State) ->
	io:format("Unexpected event ~p with data ~p~n", [Type, Data]),
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

user_res(RoomsInfo) ->
	lists:map(fun (
		{[
			{<<"name">>, RoomName},
			{<<"latest">>, Timestamp}
		]}
	) when is_binary(RoomName), is_integer(Timestamp) ->
		echat_room:subscribe(RoomName),
		{[
			{<<"room">>, RoomName},
			{<<"messages">>, messages_since_ejson(RoomName, Timestamp)}
		]}
	end, RoomsInfo).
	
join_res(RoomName) when is_binary(RoomName) ->
	{[
		{<<"room">>, RoomName},
		{<<"messages">>, messages_since_ejson(RoomName, -1)}
	]}.

extract_room_names(RoomsInfo) ->
	ExtractRoomName = fun (
		{[
			{<<"name">>, RoomName},
			{<<"latest">>, _Timestamp}
		]}
	) when is_binary(RoomName) ->
		RoomName
	end,
	lists:map(ExtractRoomName, RoomsInfo).

messages_since_ejson(RoomName, Timestamp) ->
	MessagesDiff = lists:reverse(echat_room:messages_since(RoomName, Timestamp)),
	TranformToEjson = fun ({UserID, Nickname, Timestamp, Content}) when is_integer(UserID), is_binary(Nickname), is_integer(Timestamp), is_binary(Content) ->
		{[
			{<<"timestamp">>, Timestamp},
			{<<"content">>, Content},
			{<<"userID">>, UserID},
			{<<"nickname">>, Nickname}
		]}
	end,
	lists:map(TranformToEjson, MessagesDiff).