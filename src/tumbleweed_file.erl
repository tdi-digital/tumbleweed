-module(tumbleweed_file).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, get_file/1, save_and_return/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  AppDir = code:priv_dir(tumbleweed),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [AppDir], []).

get_file(Name) ->
  gen_server:call(?SERVER, {get_file, Name}, infinity).

save_and_return(Name, File) ->
  gen_server:call(?SERVER, {save_file, Name, File}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([AppDir]) ->
  State = [{app_dir, AppDir}],
  {ok, State}.

handle_call({get_file, Name}, _From, State) ->
  AppDir = proplists:get_value(app_dir, State),
  FileResult = file:read_file(AppDir ++ Name),
  case FileResult of
    {ok, File}      -> {reply, File, State};
    {error, enoent} -> {reply, not_found, State};
    {error, Reason} -> {stop, Reason, State}
  end;

handle_call({save_file, Name, File}, _From, State) ->
  AppDir = proplists:get_value(app_dir, State),
  WriteResult = file:write_file(AppDir ++ Name, File),
  case WriteResult of
    ok              -> {reply, File, State};
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

