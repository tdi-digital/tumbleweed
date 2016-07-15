-module(tumbleweed_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  setup_rest_interface(),
  tumbleweed_sup:start_link().

stop(_State) ->
  cowboy:stop_listener(tumbleweed),
  ok.

%%%===================================================================

% Todo: Each URL handler declares it's own path
% Todo: Embed Ranch
setup_rest_interface() ->
  Paths = [
    {"/product/:product_code",       tumbleweed_cowboy_product,       []},
    {"/product_image/:product_code", tumbleweed_cowboy_product_image, []},
    {"/sunset_config",               tumbleweed_cowboy_sunset_config, []},
    {"/sunset_logo",                 tumbleweed_cowboy_sunset_logo,   []}],

  CompiledPaths = cowboy_router:compile([{'_', Paths}]),
  CowboyPort = 3000,
  cowboy:start_http(tumbleweed, 5, [{port, CowboyPort}], [{env, [{dispatch, CompiledPaths}]}]).