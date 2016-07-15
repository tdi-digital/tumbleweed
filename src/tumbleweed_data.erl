-module(tumbleweed_data).
-behaviour(gen_server).

%% API
-export([start_link/0, query/2]).

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
  LocalSource = tumbleweed_config:get_property("local_source"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [LocalSource], []).

query(Query, Parameters) ->
  gen_server:call(?SERVER, {query, Query, Parameters}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LocalSource]) ->
  ServerType = json:obj_fetch("server_type", LocalSource),
  {ok, [{server_type, ServerType}]}.

handle_call({query, Query, Parameters}, _From, State) ->
  ServerType = proplists:get_value(server_type, State),

  QueryResult = get_query_result(ServerType, Query, Parameters),
  case QueryResult of
    {ok, Result}    -> {reply, Result, State};
    {error, Reason} -> {stop,  Reason, State}
  end;

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(normal, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_query_result("test", _Query, [Code]) ->
  Row = case Code of
          "7894000010014" -> ["AMIDO DE MILHO MAIZENA 200G", 5.49];
          "7891000053508" -> ["ACHOCOLATADO NESCAU 2.0 - 400G", 4.99];
          "7891079000229" -> ["MACARRAO INSTANTANEO NISSIN MIOJO GALINHA CAIPIRA 85G", 2.30];
          "7896004400327" -> ["COCO RALADO SOCOCO FLOCOS SWEET 100G", 2.50];
          "7896292300545" -> ["ERVILHA PREDILECTA", 2.97];
          "7893000383005" -> ["QUALY CREMOSA SEM SAL 500 G", 4.00];
          "7891098038456" -> ["CHA MATE NATURAL LEAO GRANEL 250G", 4.50];
          _ -> ["PRODUTO NAO CADASTRADO", 0]
        end,
  {ok, {[], [Row]}};

get_query_result("mysql", Query, Parameters) ->
  Query2 = query:build(Query, Parameters),
  MySqlResult = mysql:fetch(tumbleweed, Query2),
  clean_mysql_result(MySqlResult).

clean_mysql_result({data,  Result}) ->
  MySqlFields = mysql:get_result_field_info(Result),
  Fields = [binary_to_list(Field) || {_Table, Field, _Length, _Type} <- MySqlFields],
  Rows   = mysql:get_result_rows(Result),
  {ok, {Fields, Rows}};
clean_mysql_result({error, Result}) ->
  {error, Result}.