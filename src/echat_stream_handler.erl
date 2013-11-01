-module(echat_stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 1000).

init(_Transport, Req, _Opts, _Active) ->
		io:format("bullet init~n"),
		{ok, Req, undefined}.

stream(Data, Req, State) ->
		io:format("stream received ~s~n", [Data]),
		{ok, Req, State}.

info(Info, Req, State) ->
		io:format("info received ~p~n", [Info]),
		{ok, Req, State}.

terminate(_Req, _State) ->
		io:format("bullet terminate~n"),
		ok.