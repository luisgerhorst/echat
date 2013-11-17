-module(echat_app).

-define(PORT, 8080). % the port, you can change this!

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, []) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/bullet", bullet_handler, [{handler, echat_stream_handler}]},
            {"/bullet.js", cowboy_static, [
                {directory, {priv_dir, bullet, []}},
                {file, "bullet.js"},
                {mimetypes, [{<<".js">>, [<<"text/javascript">>]}]}
            ]},
            {"/", cowboy_static, [
                {directory, {priv_dir, echat, []}},
                {file, "index.html"},
                {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
            ]},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, echat, []}},
                {mimetypes, [
                    {<<".html">>, [<<"text/html">>]},
                    {<<".js">>, [<<"text/javascript">>]},
                    {<<".css">>, [<<"text/css">>]}
                ]}
            ]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, ?PORT}], [{env, [{dispatch, Dispatch}]}]),
    ets:new(usernames, [set, public, named_table]),
    echat_messages:start(),
    echat_sup:start_link(). % start manager, do stuff above in manager and send manager's pid to each handler's init

stop(_State) ->
    ok.