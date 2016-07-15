-module(tumbleweed_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("supervisor.hrl").

-define(SUPERVISOR, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
    RestartStrategy = #one_for_one{max_restarts = 1,
                                   max_seconds_between_restarts = 1},
    Children = [
        child(worker,     tumbleweed_file,     []),
        child(worker,     tumbleweed_config,   []),
        child(worker,     tumbleweed_rest,     []),
        child(supervisor, tumbleweed_data_sup, [])],

    {ok, {RestartStrategy, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
