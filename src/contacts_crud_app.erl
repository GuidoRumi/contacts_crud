%%%-------------------------------------------------------------------
%% @doc contacts_crud public API
%% @end
%%%-------------------------------------------------------------------

-module(contacts_crud_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(epgsql, permanent),
    %% Compiles and loads cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/contacts", contacts_crud_http_handler, [contacts]},
            {"/contacts/surname/:surname",
                contacts_crud_http_handler, [find_surname]},
            {"/contacts/delete/:email", contacts_crud_http_handler, [delete]},
            {"/contacts/:email", contacts_crud_http_handler, [update]}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(contacts_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    %% Starts leader_cron task to delete inactive users
    %% every 10 minutes
    leader_cron:start_link([node()]),
    leader_cron:schedule_task(
        {cron, {[0, 10, 20, 30, 40, 50], all, all, all, all}},
        {contacts_crud_queries, delete_inactive, []}),
    contacts_crud_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
