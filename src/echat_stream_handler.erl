-module(echat_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("new connection~n"),
	{ok, Req, undefined}. % State: {UserID, Nickname, Rooms}

stream(EncodedData, Req, User) ->
	{[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]} = jiffy:decode(EncodedData), % catch error here!
	handle(Type, Data, Req, User).
%stream(Data, Req, State) ->
%	io:format("Unexpected data in stream: ~p~n", [Data]),
%	{ok, Req, State}.

info({new_message, RoomName, Timestamp, Content, UserID, Nickname}, Req, State) -> % from room
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
	]}, Req, State);
info(Msg, Req, State) ->
	io:format("Unexpected info: ~p~n", [Msg]),
	{ok, Req, State}.

terminate(_Req, {_UserID, _Nickname, Rooms}) when is_list(Rooms) ->
	lists:map(fun echat_room:unsubscribe/1, Rooms),
	ok;
terminate(_Req, _User) -> ok.
	
% handle

get_messages(RoomName, LatestMessageTimestamp) ->
	echat_room:subscribe(RoomName),
	MessagesDiff = echat_room:get_messages_since(RoomName, LatestMessageTimestamp),
	lists:map(fun ({Timestamp, Content, UserID, Nickname}) -> {[
		{<<"timestamp">>, Timestamp},
		{<<"content">>, Content},
		{<<"userID">>, UserID},
		{<<"nickname">>, Nickname}
	]} end, MessagesDiff).

handle(<<"user">>, {[
	{<<"id">>, UserID},
	{<<"nickname">>, Nickname},
	{<<"rooms">>, RoomsInfo}
]}, Req, _User) ->
	io:format("rooms info ~p~n", [RoomsInfo]),
	ResData = lists:map(fun ({[
		{<<"name">>, RoomName},
		{<<"latest">>, Timestamp}
	]}) -> {[
		{<<"room">>, RoomName},
		{<<"messages">>, get_messages(RoomName, Timestamp)}
	]} end, RoomsInfo),
	Rooms = lists:map(fun ({[
		{<<"name">>, RoomName},
		{<<"latest">>, _Timestamp}
	]}) -> RoomName end, RoomsInfo),
	res(<<"old_messages">>, ResData, Req, {UserID, Nickname, Rooms});
handle(<<"join">>, RoomName, Req, User) ->
	echat_room:subscribe(RoomName),
	echat_room:get_messages_since(RoomName, -1),
	res(none, Req, User);
handle(<<"leave">>, RoomName, Req, User) ->
	echat_room:unsubscribe(RoomName),
	res(none, Req, User);
handle(<<"message">>, {[
	{<<"room">>, RoomName},
	{<<"content">>, Content}
]}, Req, {UserID, Nickname, _Rooms} = User) ->
	io:format("content ~p~n", [Content]),
	echat_room:save_message(RoomName, Content, UserID, Nickname),
	res(none, Req, User);
handle(Type, Data, Req, State) ->
	io:format("Unexpected event ~p with data ~p~n", [Type, Data]),
	res(none, Req, State).
	
% response

res(none, Req, State) -> {ok, Req, State}.

res(Type, Data, Req, State) ->
	Res = jiffy:encode( {[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]} ),
	{reply, Res, Req, State}.