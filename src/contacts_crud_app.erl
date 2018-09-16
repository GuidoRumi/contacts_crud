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
    contacts_crud_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
