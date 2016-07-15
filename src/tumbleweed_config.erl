-module(tumbleweed_config).
-behaviour(gen_server).

%% API
-export([start_link/0, get_property/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  ConfigFile = tumbleweed_file:get_file("/config.json"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigFile], []).

get_property(Property) ->
  gen_server:call(?SERVER, {get, Property}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ConfigFile]) ->
  ConfigText = binary_to_list(ConfigFile),

  DecodeResult = json:decode_string(ConfigText),
  case DecodeResult of
    {ok, JsonObject} -> {ok, [{json_object, JsonObject}]};
    {error, Reason}  -> {stop, Reason}
  end.

handle_call({get, Property}, _From, State) ->
  JsonObject = proplists:get_value(json_object, State),

  Result = json:obj_find(Property, JsonObject),
  case Result of
    {ok, Value}     -> {reply, Value, State};
    {error, Reason} -> {stop, Reason, State}
  end;

handle_call(_Request, _From, State) -> {reply, not_implemented, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================