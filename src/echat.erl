-module(echat).

%% API.
-export([start/0, stop/0]).

%% API.

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowlib),
	ok = application:start(cowboy),
	ok = application:start(echat).

stop() ->
	ok = application:stop(echat),
	ok = application:stop(mnesia),
	ok = application:stop(cowboy),
	ok = application:stop(ranch),
	ok = application:stop(crypto).