-module(tumbleweed_cowboy_product).
-behaviour(cowboy_handler).

%% API
-export([init/2]).

%% cowboy callbacks
-export([
  allowed_methods/2,
  content_types_provided/2,
  resource_exists/2,
  json_handler/2]).

-include("records.hrl").

%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, json_handler}], Req, State}.

resource_exists(Req, State) ->
  ProductCode = binary_to_list(cowboy_req:binding(product_code, Req)),

  Product = tumbleweed:get_product(ProductCode),
  case Product of
    #product{} -> {true,  Req, {state, Product}};
    not_found  -> {false, Req, State}
  end.

json_handler(Req, State) ->
  {state, P} = State,
  JProduct = json:obj_store("description", P#product.description,
                   json:obj_store("price", P#product.price,
                     json:obj_new())),

  JBody = json:encode(JProduct),
  {JBody, Req, State}.
