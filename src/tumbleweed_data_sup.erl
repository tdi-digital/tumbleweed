-module(tumbleweed_data_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, data_source_spec/0, data_source_spec_for/2, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("supervisor.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link(_) ->
  SupervisorResult = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  case SupervisorResult of
    {ok, _} ->
      ChildResult = supervisor:start_child(?SERVER, data_source_spec()),
      case ChildResult of
        {error, _} -> ChildResult;
        {ok, _}    -> SupervisorResult
      end;
    {error, _} -> SupervisorResult
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = #one_for_one{max_restarts = 2,
                                 max_seconds_between_restarts = 10},
  Children = [child(worker, tumbleweed_data, [])],
  {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

data_source_spec() ->
  Config = tumbleweed_config:get_property("local_source"),
  ServerType = json:obj_fetch("server_type", Config),
  data_source_spec_for(ServerType, Config).

data_source_spec_for("mysql", Config) ->
  Host     = json:obj_fetch("host", Config),
  Port     = json:obj_fetch("port", Config),
  User     = json:obj_fetch("user", Config),
  Password = json:obj_fetch("password", Config),
  Database = json:obj_fetch("base", Config),

  child(worker, mysql, [tumbleweed, Host, Port, User, Password, Database]);

data_source_spec_for("oracle", Config) ->
  Host        = json:obj_fetch("host", Config),
  Port        = json:obj_fetch("port", Config),
  User        = json:obj_fetch("user", Config),
  Password    = json:obj_fetch("password", Config),
  ServiceName = json:obj_fetch("base", Config),

  child(worker, oracle, [Host, Port, User, Password, ServiceName]);

data_source_spec_for("test", _Config) ->
  child(worker, test, []);

data_source_spec_for(ServerType, Config) ->
  erlang:error(unknown_server_type,[ServerType, Config]).
