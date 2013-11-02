-module(echat_room).
-behaviour(gen_server).

-export([start_link/0, save_message/3, get_messages_since/1, subscribe/0, unsubscribe/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% api

start_link() ->
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	register(echat_room, Pid),
	{ok, Pid}.

save_message(Content, UserID, Nickname) ->
	gen_server:cast(whereis(echat_room), {save_message, Content, UserID, Nickname}).

get_messages_since(Timestamp) ->
	gen_server:call(whereis(echat_room), {get_messages_since, Timestamp}).
	
subscribe() ->
	gen_server:cast(whereis(echat_room), {subscribe, self()}).
	
unsubscribe() ->
	gen_server:cast(whereis(echat_room), {unsubscribe, self()}).
	
% gen_server

init([]) ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
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
					{<<".js">>, [<<"text/javascript">>]},
					{<<".css">>, [<<"text/css">>]}
				]}
			]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
	{ok, {[], []}}.

handle_call({get_messages_since, LatestMessageTimestamp}, _From, {Subscribers,Messages}) ->
	MessagesDiff = [{Timestamp, Content, UserID, Nickname} || {Timestamp, Content, UserID, Nickname} <- Messages, LatestMessageTimestamp < Timestamp],
	{reply, MessagesDiff, {Subscribers, Messages}};
handle_call(Msg, _From, State) ->
	io:format("Unexpected call to echat_room ~p~n", [Msg]),
	{noreply, State}.
	
handle_cast({save_message, Content, UserID, Nickname}, {Subscribers, Messages}) ->
	NewMessage = {timestamp(), Content, UserID, Nickname},
	[Subscriber ! {new_message, NewMessage} || Subscriber <- Subscribers],
	{noreply, {Subscribers, [NewMessage|Messages]}};
handle_cast({subscribe, Pid}, {Subscribers, Messages}) ->
	{noreply, {[Pid|Subscribers], Messages}};
handle_cast({unsubscribe, Pid}, {Subscribers, Messages}) ->
	{noreply, {lists:delete(Pid, Subscribers), Messages}};
handle_cast(Msg, State) ->
	io:format("Unexpected cast to echat_room ~p~n", [Msg]),
	{noreply, State}.
	
handle_info(Msg, State) ->
	io:format("Unexpected message to echat_room ~p~n", [Msg]),
	{noreply, State}.
	
terminate(normal, _State) -> ok.
	
code_change(_OldVsn, State, _Extra) -> {ok, State}.
	
% private

timestamp() ->
	{Mega, Sec, Micro} = now(),
	Timestamp = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.