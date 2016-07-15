-module(tumbleweed_cowboy_sunset_config).
-behaviour(cowboy_handler).

%% API
-export([init/2]).

%% cowboy callbacks
-export([
  allowed_methods/2,
  content_types_provided/2,
  json_handler/2]).


%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, json_handler}], Req, State}.

json_handler(Req, State) ->
  Config = tumbleweed:get_sunset_config(),
  JBody = json:encode(Config),
  {JBody, Req, State}.
