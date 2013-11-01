-module(echat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/_bullet", bullet_handler, [{handler, echat_stream_handler}]},
            {"/", cowboy_static, [
                {directory, {priv_dir, echat, []}},
                {file, "index.html"},
                {mimetypes, [
                        {<<".html">>, [<<"text/html">>]}
                ]}
            ]},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, echat, []}},
                {mimetypes, [
                        {<<".js">>, [<<"text/javascript">>]},
                        {<<".css">>, [<<"text/css">>]}
                ]}
            ]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    echat_sup:start_link().

stop(_State) ->
    ok.