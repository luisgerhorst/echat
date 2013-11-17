-module(echat_messages).

-export([start/0, new/3, convert_to_relative_tuple/1, timestamp/1, save/1, before/3, between/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(messages, {
	room,
	username,
	timestamp,
	content
}).

-define(NODES, [node()]).
-define(TABLE_NAME, messages).

% start

start() ->
	
	case mnesia:create_schema(?NODES) of
		ok -> io:format("Mnesia schema created.~n");
		{error, {_, {already_exists, _}}} -> io:format("Mnesia schema already exists.~n");
		{error, SchemaReason} -> io:format("Error: Unable to create Mnesia schema because of reason ~p~n", [SchemaReason])
	end,
	
	case application:start(mnesia) of
		ok -> io:format("Mnesia started.~n");
		{error, {already_started, mnesia}} -> io:format("Mnesia already running.~n");
		{error, StartReason} -> io:format("Error: Unable to start Mnesia because of reason ~p~n", [StartReason])
	end,
	
	case
		mnesia:create_table(
			?TABLE_NAME,
			[
				{attributes, record_info(fields, messages)},
				{disc_copies, ?NODES},
				{type, bag}
			]
		)
	of
		{atomic, ok} ->
			io:format("Mnesia table for messages created.~n");
		{aborted, {already_exists, ?TABLE_NAME}} ->
			io:format("Mnesia table for messages already exists.~n");
		{aborted, TableReason} ->
			io:format("Error: Unable to create table because of reason ~p~n", TableReason)
	end,
	
	ok = mnesia:wait_for_tables([?TABLE_NAME], 10000). % required
	
% data
	
new(Room, Username, Content) ->
	{Room, Username, Content, timestamp_ms()}. % don't use this
	
timestamp_ms() ->
	{Mega, Sec, Micro} = now(),
	(Mega*1000000 + Sec)*1000 + Micro/1000. % thats ms, the internet is wrong! I use a double so you can well use it as id for messages

convert_to_relative_tuple({_Room, Username, Content, Timestamp}) ->
	{Username, Content, Timestamp}. % you can use this
	
timestamp({_Room, _Username, _Content, Timestamp}) ->
	Timestamp.
	
% read/write

save({Room, Username, Content, Timestamp}) ->
	Fun = fun () ->
		mnesia:write(
			#messages{
				room=Room,
				username=Username,
				content=Content,
				timestamp=Timestamp
			}
		)
	end,
	mnesia:activity(transaction, Fun).
	
before(SearchedRoom, BeforeTimestamp, Limit) -> % faster?
	Fun = fun () ->
		Match = ets:fun2ms(fun
				(Message = #messages{
					room=Room,
					username=Username,
					content=Content,
					timestamp=Timestamp
				}) when Room =:= SearchedRoom, Timestamp < BeforeTimestamp ->
					{Username, Content, Timestamp}
		end),
		mnesia:select(?TABLE_NAME, Match)
	end,
	MessagesBefore = lists:reverse(mnesia:activity(transaction, Fun)), % before timestamp, newest first
	MessagesLimited = case Limit > length(MessagesBefore) of
		true ->
			MessagesBefore;
		false ->
			{Cut, _} = lists:split(Limit, MessagesBefore),
			Cut
	end,
	Messages = lists:reverse(MessagesLimited), % limited before timestamp, oldest first
	% io:format("Messages for request for before ~p, limit ~p in room ~p found: ~p~n", [BeforeTimestamp, Limit, SearchedRoom, Messages]),
	Messages.
	
between(SearchedRoom, StartTimestamp, EndTimestamp) ->
	Fun = fun () ->
		Match = ets:fun2ms(fun
				(Message = #messages{
					room=Room,
					username=Username,
					content=Content,
					timestamp=Timestamp
				}) when Room =:= SearchedRoom, StartTimestamp < Timestamp, Timestamp < EndTimestamp ->
					{Username, Content, Timestamp}
		end),
		mnesia:select(?TABLE_NAME, Match)
	end,
	mnesia:activity(transaction, Fun).