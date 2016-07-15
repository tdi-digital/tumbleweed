-module(tumbleweed_rest).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
  get_product_image/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {rest_server, product_image}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  RestConfig = tumbleweed_config:get_property("rest"),
  RestServer   = json:obj_fetch("rest_server",   RestConfig),
  ProductImage = json:obj_fetch("product_image", RestConfig),
  State = #state{rest_server = RestServer, product_image = ProductImage},
  gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

get_product_image(Code) ->
  gen_server:call(?SERVER, {produtc_image, Code}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(State) -> {ok, State}.

handle_call({produtc_image, Code}, _From, State = #state{rest_server = RestServer,
                                                       product_image = ProductQuery}) ->

  URL = RestServer ++ query:build(ProductQuery, [Code]),
  case get_rest_resource(URL) of
    {200, Result}     -> {reply, list_to_binary(Result), State};
    {error, Reason}   -> {stop,  Reason, State};
    {_Other, _Reason} -> {reply, not_found, State} % Todo: Log OtherCode and Reason
  end;

handle_call(_Request, _From, State) -> {reply, not_implemented, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_rest_resource(Url) -> get_rest_resource(Url, []).
get_rest_resource(Url, Headers) ->
  HttpResult = httpc:request(get,{Url, Headers},[],[]),

  case HttpResult of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers,  Body}} -> {200, Body};
    {ok, {{_Version, Code, ReasonPhrase}, _Headers, _Body}} -> {Code, ReasonPhrase};
    {error, Reason} -> {error, Reason}
  end.