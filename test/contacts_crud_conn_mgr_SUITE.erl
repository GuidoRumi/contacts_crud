-module(contacts_crud_conn_mgr_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("./resources/contacts_crud_resources.hrl").
-include_lib("./resources/contacts_crud_conn_mgr_resources.hrl").
-export([all/0]).

all() ->
    [
        test_insert_then_select,
        test_wrong_table,
        test_execute_equery_epgsql_transparency
    ].

init_per_suite(Config) ->
    ?assertMatch(
        {ok, _},
        application:ensure_all_started(contacts_crud, permanent)
    ),
    Config.

end_per_suite(_Config) ->
    ?assertMatch(
        {ok, _},
        contacts_crud_conn_mgr:execute_equery(?DELETE_ALL_QUERY, [])
    ),
    ok.

test_insert_then_select(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_conn_mgr:execute_equery(?INSERT_QUERY, ?CONTACT_1)
    ),
    ?assertMatch(
        {ok, _, [{?CONTACT_1_EMAIL}]},
        contacts_crud_conn_mgr:execute_equery(?SELECT_EMAIL_QUERY, [])
    ).

test_wrong_table(_Config) ->
    ?assertMatch(
        {error, {error, error, _, undefined_table, _, _}},
        contacts_crud_conn_mgr:execute_equery(?SELECT_WRONG_TABLE, ?CONTACT_1)
    ).

%% Tests that contacts_crud_conn_mgr:execute_equery/2 returns the same value that epgsql_equery/3
test_execute_equery_epgsql_transparency(_Config) ->
    meck:new(epgsql),
    meck:expect(epgsql, equery, fun (_, _, _) -> {ok, mocked_result} end),
    ?assertEqual(
        {ok, mocked_result},
        contacts_crud_conn_mgr:execute_equery(?INSERT_QUERY, ?CONTACT_1)
    ),
    meck:unload(epgsql).
