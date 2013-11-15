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

% start

start() ->
	try
		io:format("Trying to create a mnesia table for messages ...~n"),
		create()
	catch _Exception:_Reason ->
		io:format("Error, will load existing mnesia table for messages.~n"),
		load()
	end.

create() ->
	ok = mnesia:create_schema(?NODES),
	application:start(mnesia),
	mnesia:create_table(
		messages,
		[
			{attributes, record_info(fields, messages)},
			{disc_copies, ?NODES},
			{type, bag}
		]
	),
	mnesia:wait_for_tables([messages], 5000),
	ok.

load() ->
	application:start(mnesia),
	mnesia:wait_for_tables([messages], 5000),
	ok.
	
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
		mnesia:select(messages, Match)
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
		mnesia:select(messages, Match)
	end,
	mnesia:activity(transaction, Fun).

