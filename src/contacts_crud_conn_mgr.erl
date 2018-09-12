%% @doc contacts_crud_conn_manager keeps connection to postgres alive
%% and executes extended queries.
%% In case connection is not responsive, epgsql should
%% throw an exception and contacts_crud_sup should restart
%% this process reinititializing the conneciton.
-module(contacts_crud_conn_mgr).

-behaviour(gen_server).

-export([start_link/0]).

%% API

-export([execute_equery/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
  ]).

-record(state, {
    conn = undefined :: undefined | pid()
}).

%% @doc execute_equery executes an extended query using the pre-established
%% connection and returns exactly what epsql would
-spec execute_equery(iodata(), list()) -> epgsql_cmd_equery:response().
execute_equery(QueryString, Values) ->
    {ok, Result} =
        gen_server:call(?MODULE, {execute_equery, QueryString, Values}),
    Result.

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Init loads configuration from config files, starts a connection
%% and stores it's PID in gen_server's state
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, Configs} = application:get_env(contacts_crud, postgres_config),
    #{
        username := Username,
        password := Password,
        host := Host,
        database := Database,
        port := Port
    } = Configs,
    {ok, C} = epgsql:connect(Host, Username, Password, #{
        database => Database,
        port => Port,
        timeout => 4000
    }),
    NewState = #state{conn = C},
    {ok, NewState}.

%% Executes an extended query
-spec handle_call({execute_equery, iodata(), list()}, {pid(), _}, #state{})
    -> {reply, {ok, epgsql_cmd_equery:response()}, #state{}}.
handle_call({execute_equery, QueryString, Values}, _From, State) ->
    C = get_conn(State),
    Result = epgsql:equery(C, QueryString, Values),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    C = get_conn(State),
    ok = epgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% State Getters
%% =============================================================================

get_conn(State) ->
    State#state.conn.

