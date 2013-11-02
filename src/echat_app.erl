-module(echat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    echat_sup:start_link(). % start manager, do stuff above in manager and send manager's pid to each handler's init

stop(_State) ->
    ok.