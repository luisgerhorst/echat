-module(static_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").
-include_lib("etest_http/include/etest_http.hrl").

before_suite() -> application:start(echat).
	
after_suite() -> application:stop(echat).

test_index() ->
	Response = ?perform_get("http://localhost:8080/"),
	?assert_status(200, Response).
	
test_other() ->
	Response = ?perform_get("http://localhost:8080/style.css"),
	?assert_status(200, Response).