-module(echat).

%% API.
-export([start/0, stop/0]).

%% API.

start() -> [
	application:start(crypto),
	application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),
	application:start(echat)].

stop() -> [
	application:stop(echat),
	application:stop(mnesia),
	application:stop(cowboy),
	application:stop(cowlib),
	application:stop(ranch),
	application:stop(crypto)].