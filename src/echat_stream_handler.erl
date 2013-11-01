-module(echat_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

% receive

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	{ok, Req, []}.

stream(EncodedData, Req, Messages) ->
	[Type|Data] = jiffy:decode(EncodedData), % catch error here!
	handle(Type, Data, Req, Messages);
stream(Data, Req, Messages) ->
	io:format("Unexpected data in stream: ~p~n", [Data]),
	{ok, Req, Messages}.

info(Info, Req, Messages) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, Messages}.

terminate(_Req, _Messages) ->
	io:format("bullet terminate~n"),
	ok.
	
% handle

handle(<<"message">>, [Content, UserID, Username], Req, Messages) when is_binary(Content), is_integer(UserID), is_binary(Username) ->
	NewMessages = [{timestamp(), Content, UserID, Username}|Messages],
	res(none, Req, NewMessages);
handle(<<"update?">>, [LatestMessageTimestamp], Req, Messages) when is_integer(LatestMessageTimestamp) ->
	MessagesDiff = [[Timestamp, Content, UserID, Username] || {Timestamp, Content, UserID, Username} <- Messages, LatestMessageTimestamp < Timestamp], % when newer/bigger timestamp take over and convert tuple to list
	res(<<"messages">>, MessagesDiff, Req, Messages);
handle(Type, Data, Req, Messages) ->
	io:format("Unexpected event ~p with data ~p~n", [Type, Data]),
	res(none, Req, Messages).
	
% response

res(none, Req, Messages) -> {ok, Req, Messages}.

res(Type, Data, Req, Messages) ->
	Res = jiffy:encode([Type|Data]),
	{reply, Res, Req, Messages}.
	
% tools

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.