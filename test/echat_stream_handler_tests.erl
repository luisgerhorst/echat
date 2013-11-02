-module(echat_stream_handler_tests).
-include_lib("eunit/include/eunit.hrl").

% todo: write working tests (common test?)

%add_message_test() ->
	%JSON = <<"{\"type\":\"message\",\"data\":{\"content\":\"hi\",\"userID\":1,\"username\":\"luis\"}}">>,
	%Result = echat_stream_handler:stream(JSON, my_req, my_state),
	%?assertMatch({ok, my_req, my_state}, Result).
	
%load_messages_test() ->
	%JSON = <<"{\"type\":\"messages?\",\"data\":2}">>, % i have everthing 'till 2
	%Messages = [{3, <<"c">>, 1, <<"luis">>},{2, <<"b">>, 1, <<"luis">>},{1, <<"a">>, 1, <<"luis">>}], % list in erlang is reversed
	%Result = echat_stream_handler:stream(JSON, req, Messages),
	%?assertMatch({reply, <<"{\"type\":\"messages\",\"data\":[{\"timestamp\":3,\"content\":\"c\",\"userID\":1,\"username\":\"luis\"}]}">>, req, Messages}, Result). % reply should be everthing after 2