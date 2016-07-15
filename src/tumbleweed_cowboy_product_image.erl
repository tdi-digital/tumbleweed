-module(tumbleweed_cowboy_product_image).
-behaviour(cowboy_handler).

%% API
-export([init/2]).

%% cowboy callbacks
-export([
  allowed_methods/2,
  content_types_provided/2,
  resource_exists/2,
  image_handler/2]).

%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"image/jpeg">>, image_handler}], Req, State}.

resource_exists(Req, State) ->
  ProductCode = binary_to_list(cowboy_req:binding(product_code, Req)),

  Image = tumbleweed:get_product_image(ProductCode),
  case Image of
    <<Image/binary>> -> {true,  Req, {state, Image}};
    not_found        -> {false, Req, State}
  end.

image_handler(Req, State) ->
  {state, Image} = State,
  {Image, Req, State}.
