-module(echat_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

% todo: better event names

% receive

init(_Transport, Req, _Opts, _Active) ->
	echat_room:subscribe(),
	{ok, Req, undefined}.

stream(EncodedData, Req, State) ->
	{[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]} = jiffy:decode(EncodedData), % catch error here!
	handle(Type, Data, Req, State);
stream(Data, Req, State) ->
	io:format("Unexpected data in stream: ~p~n", [Data]),
	{ok, Req, State}.

info({new_message, {Timestamp, Content, UserID, Username}}, Req, State) ->
	res(<<"message">>, {[
		{<<"timestamp">>, Timestamp},
		{<<"content">>, Content},
		{<<"userID">>, UserID},
		{<<"username">>, Username}
	]}, Req, State).

terminate(_Req, _State) ->
	echat_room:unsubscribe(),
	ok.
	
% handle

handle(<<"message">>, {[
	{<<"content">>, Content},
	{<<"userID">>, UserID},
	{<<"username">>, Username}
]}, Req, State) when is_binary(Content), is_integer(UserID), is_binary(Username) ->
	echat_room:save_message(Content, UserID, Username),
	res(none, Req, State);
handle(<<"messages?">>, LatestMessageTimestamp, Req, State) when is_integer(LatestMessageTimestamp) ->
	MessagesDiffEJSON = [ {[
		{<<"timestamp">>, Timestamp},
		{<<"content">>, Content},
		{<<"userID">>, UserID},
		{<<"username">>, Username}
	]} || {Timestamp, Content, UserID, Username} <- echat_room:load_messages_since(LatestMessageTimestamp)],
	res(<<"messages">>, MessagesDiffEJSON, Req, State);
handle(Type, Data, Req, State) ->
	io:format("Unexpected event ~p with data ~p~n", [Type, Data]),
	res(none, Req, State).
	
% response

res(none, Req, Messages) -> {ok, Req, Messages}.

res(Type, Data, Req, Messages) ->
	Res = jiffy:encode( {[
		{<<"type">>, Type},
		{<<"data">>, Data}
	]} ),
	{reply, Res, Req, Messages}.