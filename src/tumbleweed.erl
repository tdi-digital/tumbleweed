-module(tumbleweed).
-include("records.hrl").

-export([start/0, stop/0, test/0, build/0]).
-export([
  get_product/1,
  get_product_image/1,
  get_sunset_config/0,
  get_sunset_logo/0]).

start() ->
  application:ensure_all_started(?MODULE).

stop() ->
  application:stop(?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

get_product(Code) ->
  ProductQuery = tumbleweed_config:get_property("product_query"), % Todo: Switch to atom

  {_Fields, Rows} = tumbleweed_data:query(ProductQuery, [Code]),
  case Rows of
    [[Description, Price]|_] -> #product{description = Description,
                                               price = Price};
    [] -> not_found
  end.

%%%===================================================================

get_product_image(Code) ->
  FilePath = "/product_images/" ++ Code,

  LocalImage = tumbleweed_file:get_file(FilePath),
  case LocalImage of
    <<LocalImage/binary>> -> LocalImage;
    not_found ->
      WebImage = tumbleweed_rest:get_product_image(Code),
      case WebImage of
        <<WebImage/binary>> ->
          tumbleweed_file:save_and_return(FilePath, WebImage);
        not_found -> not_found
      end
  end.

%%%===================================================================

get_sunset_config() ->
  tumbleweed_config:get_property("sunset").

%%%===================================================================

get_sunset_logo() ->
  tumbleweed_file:get_file("/images/logo").

%%%===================================================================
%%% Internal functions
%%%===================================================================

test() ->
  build(),
  observer:start(),
  tumbleweed:start(),
  tumbleweed:stop().

build() ->
  systools:make_script("tumbleweed"),
  systools:make_tar("tumbleweed"),
  target_system:create("tumbleweed").

%7896714202884
%7891000053508
%7897571925589