-module(contacts_crud_queries_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("./resources/contacts_crud_resources.hrl").
-include_lib("./resources/contacts_crud_queries_resources.hrl").
-export([all/0]).

all() ->
    [
        test_create_ok,
        test_create_duplicate_pkey,
        test_find_one_surname_empty,
        test_create_then_find_one_surname,
        test_create_two_then_find_one_surname,
        test_create_then_update_ok,
        test_create_then_update_missing_record,
        test_list,
        test_flag_as_inactive_and_delete_inactive
    ].

init_per_suite(Config) ->
    ?assertMatch(
        {ok, _},
        application:ensure_all_started(contacts_crud, permanent)
    ),
    Config.

%% Deletes every record when suite finishes execution
end_per_suite(_Config) ->
    ?assertMatch(
        {ok, _},
        contacts_crud_conn_mgr:execute_equery(?DELETE_ALL_QUERY, [])
    ),
    ok.

%% Deletes every record at the beginning of every testcase
init_per_testcase(_Case, Config) ->
    ?assertMatch(
        {ok, _},
        contacts_crud_conn_mgr:execute_equery(?DELETE_ALL_QUERY, [])
    ),
    Config.

test_create_ok(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ).

test_create_duplicate_pkey(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertMatch(
        {error, {error, error, _, unique_violation, _, _}},
        contacts_crud_queries:create(?CONTACT_1)
    ).

%% Asserts test_find_one_surname returns an empty list in it's
%% values list if there are no records
test_find_one_surname_empty(_Config) ->
    ?assertMatch(
        {ok, _, []},
        contacts_crud_queries:find_one_surname(?CONTACT_1_SURNAME)
    ).

%% Creates a records then asserts find_one_surname actually finds it
test_create_then_find_one_surname(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertMatch(
        {ok, _, [?CONTACT_1_TUPLE]},
        contacts_crud_queries:find_one_surname(?CONTACT_1_SURNAME)
    ).

%% Asserts that find_one_surname gets no more than one record
%% even if there's two already inserted
test_create_two_then_find_one_surname(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_2)
    ),
    {ok, _, ResList} =
        contacts_crud_queries:find_one_surname(?CONTACT_1_SURNAME),
    ?assertEqual(
        1,
        length(ResList)
    ).

%% Asserts that update changes values
test_create_then_update_ok(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:update(?CONTACT_1_UPDATE)
    ),
    ?assertMatch(
        {ok, _, [?CONTACT_1_UPDATE_TUPLE]},
        contacts_crud_queries:find_one_surname(?CONTACT_1_UPDATED_SURNAME)
    ).

%% Tries to update missing record resulting in 0 records modified
test_create_then_update_missing_record(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertEqual(
        {ok, 0},
        contacts_crud_queries:update(?CONTACT_2_UPDATE)
    ).

%% Creates 2 records and asserts they are retrieved by list()
test_list(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_2)
    ),
    ?assertMatch(
        {ok, _, [?CONTACT_2_TUPLE, ?CONTACT_1_TUPLE]},
        contacts_crud_queries:list()
    ).

%% Creates 2 records, flag one as inactive, delete inactives
%% and assert list() only retrieves the other
test_flag_as_inactive_and_delete_inactive(_Config) ->
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_1)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:create(?CONTACT_2)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:flag_as_inactive(?CONTACT_1_EMAIL)
    ),
    ?assertEqual(
        {ok, 1},
        contacts_crud_queries:delete_inactive()
    ),
    ?assertMatch(
        {ok, _, [?CONTACT_2_TUPLE]},
        contacts_crud_queries:list()
    ).
