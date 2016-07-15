-module(tumbleweed_cowboy_sunset_logo).
-behaviour(cowboy_handler).

%% API
-export([init/2]).

%% cowboy callbacks
-export([
  allowed_methods/2,
  content_types_provided/2,
  image_handler/2]).


%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"image/png">>, image_handler}], Req, State}.

image_handler(Req, State) ->
  <<Image/binary>> = tumbleweed:get_sunset_logo(),
  {Image, Req, State}.