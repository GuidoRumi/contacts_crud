-module(contacts_crud_http_handler_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("./resources/contacts_crud_resources.hrl").
-include_lib("./resources/contacts_crud_http_handler_resources.hrl").

-export([all/0]).

all() ->
    [
        test_list,
        test_create,
        test_create_then_list,
        test_create_wrong_keys,
        test_create_duplicate_email,
        test_update_then_find_surname,
        test_delete_not_found,
        test_delete
    ].

init_per_suite(Config) ->
    ?assertMatch(
        {ok, _},
        application:ensure_all_started(contacts_crud, permanent)
    ),
    ?assertMatch(
        {ok, _},
        application:ensure_all_started(hackney, permanent)
    ),
    Config.

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

test_list(_Config) ->
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, ClientRef} =
        hackney:get(URL, Headers, Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    ?assertEqual(
        200,
        StatusCode
    ),
    ?assertEqual(
        <<"[]">>,
        Body
    ).

test_create(_Config) ->
    RecordMap = #{
        <<"email">> => <<"user@example.com">>,
        <<"name">> => <<"examplename">>,
        <<"surname">> => <<"examplesurname">>,
        <<"phone_number">> => <<"+5412123331">>
    },
    JsonRecord = jiffy:encode(RecordMap),
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = JsonRecord,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, ClientRef} =
        hackney:post(URL, Headers, Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    ?assertEqual(
        204,
        StatusCode
    ),
    ?assertEqual(
        <<>>,
        Body
    ).

test_create_then_list(_Config) ->
    create_record_1(),
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, ClientRef} =
        hackney:get(URL, Headers, Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    [ContactMap] = jiffy:decode(Body, [return_maps]),
    ?assertMatch(#{
        <<"email">> := ?CONTACT_1_EMAIL,
        <<"name">> := ?CONTACT_1_NAME,
        <<"surname">> := ?CONTACT_1_SURNAME,
        <<"phone_number">> := ?CONTACT_1_PHONE_NUMBER
    }, ContactMap),
    ?assertEqual(
        200,
        StatusCode
    ).

test_create_wrong_keys(_Config) ->
    WrongMap = #{
        <<"wrongemail">> => <<"user@example.com">>,
        <<"name">> => <<"examplename">>,
        <<"surname">> => <<"examplesurname">>,
        <<"phone_number">> => <<"+5412123331">>
    },
    JsonRecord = jiffy:encode(WrongMap),
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = JsonRecord,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, ClientRef} =
        hackney:post(URL, Headers, Payload, Options),
    {ok, Body} = hackney:body(ClientRef),
    ?assertEqual(
        400,
        StatusCode
    ),
    ?assertEqual(
        <<"Missing key: email">>,
        Body
    ).

test_create_duplicate_email(_Config) ->
    create_record_1(),
    JsonRecord = jiffy:encode(?CONTACT_MAP_1),
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = JsonRecord,
    Options = [],
    {ok, StatusCode2, _ResponseHeaders2, ClientRef2} =
        hackney:post(URL, Headers, Payload, Options),
    {ok, Body2} = hackney:body(ClientRef2),
    ?assertEqual(
        409,
        StatusCode2
    ),
    ?assertEqual(
        <<"Email already exists">>,
        Body2
    ).


test_update_then_find_surname(_Config) ->
    %Creates record
    create_record_1(),
    JsonUpdateRecord = jiffy:encode(?CONTACT_UPDATE_MAP_1),
    UpdateURL = <<"http://localhost:8080/contacts/user@example.com">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Options = [],
    UpdatePayload = JsonUpdateRecord,
    {ok, UpdateStatusCode, _ResponseHeaders, ClientRefUpdate} =
        hackney:put(UpdateURL, Headers, UpdatePayload, Options),
    {ok, UpdateBody} = hackney:body(ClientRefUpdate),
    ?assertEqual(
        204,
        UpdateStatusCode
    ),
    ?assertEqual(
        <<>>,
        UpdateBody
    ),
    %% Finds surname
    FindURL = <<"http://localhost:8080/contacts/surname/">>,
    FindUrlSurname = <<FindURL/bytes, ?CONTACT_UPDATE_MAP_1_SURNAME/bytes>>,
    UpdatePayload = JsonUpdateRecord,
    {ok, FindStatusCode, _ResponseFindHeaders, ClientRefFind} =
        hackney:get(FindUrlSurname, Headers, <<>>, Options),
    {ok, FindBody} = hackney:body(ClientRefFind),
    ?assertEqual(
        200,
        FindStatusCode
    ),
    ?assertEqual(
        jiffy:encode([?CONTACT_UPDATED_MAP_1]),
        FindBody
    ).


test_delete_not_found(_Config) ->
    Url = <<"http://localhost:8080/contacts/delete/">>,
    ConcatUrl = <<Url/bytes, ?CONTACT_1_EMAIL/bytes>>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, _ClientRef} =
        hackney:delete(ConcatUrl, Headers, Payload, Options),
    ?assertEqual(
        404,
        StatusCode
    ).

test_delete(_Config) ->
    create_record_1(),
    Url = <<"http://localhost:8080/contacts/delete/">>,
    ConcatUrl = <<Url/bytes, ?CONTACT_1_EMAIL/bytes>>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = <<>>,
    Options = [],
    {ok, StatusCode, _ResponseHeaders, ClientRef} =
        hackney:delete(ConcatUrl, Headers, Payload, Options),
    ?assertEqual(
        204,
        StatusCode
    ).

create_record_1() ->
    %% Creates a record
    JsonRecord = jiffy:encode(?CONTACT_MAP_1),
    URL = <<"http://localhost:8080/contacts">>,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Payload = JsonRecord,
    Options = [],
    {ok, _StatusCode, _ResponseHeaders, _ClientRef} =
        hackney:post(URL, Headers, Payload, Options).