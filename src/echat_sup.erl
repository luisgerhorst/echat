
-module(echat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    io:format("starting supervisor~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("starting supervisor step 2~n"),
    {ok, {{one_for_one, 1, 1}, [
        {
            room_manager,
            {echat_room_manager, start_link, [self()]},
            permanent,
            1000,
            worker,
            [echat_room_manager]
        }
    ]}}.

