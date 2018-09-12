%%%-------------------------------------------------------------------
%% @doc contacts_crud top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(contacts_crud_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> contacts_crud_types:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

-spec init([]) ->
    {ok, {{supervisor:strategy(), 10, 10}, [supervisor:child_spec()]}}.
init([]) ->
    Procs = [{contacts_crud_conn_mgr, {contacts_crud_conn_mgr, start_link, []},
    permanent, 5000, worker, [contacts_crud_conn_mgr]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.

%%====================================================================
%% Internal functions
%%====================================================================
