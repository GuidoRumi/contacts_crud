-module(contacts_crud_http_handler).
-include_lib("eunit/include/eunit.hrl").

%% Webmachine API
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    delete_resource/2
]).

-record(state, {op}).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, db_to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, json_to_db}
    ], Req, State}.

delete_resource(Req, State) ->
    case delete_db_record(Req) of
        {ok, 1} ->
            {true, Req, State};
        {ok, 0} ->
            contacts_crud_errors:not_found(Req)
    end.

db_to_json(Req, #state{op=contacts} = State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            {get_db_list(), Req, State};
        _ ->
            contacts_crud_errors:not_found(Req)
    end;

db_to_json(Req, #state{op=find_surname} = State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Surname = cowboy_req:binding(surname, Req),
            {ok, Columns, Rows} = contacts_crud_queries:find_one_surname(Surname),
            RecordMap = epgsql_to_map(Columns, Rows),
            ResponseBody = jiffy:encode(RecordMap),
            {ResponseBody, Req, State};
        _ ->
            contacts_crud_errors:not_found(Req)
    end.

json_to_db(Req, #state{op=contacts} = State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            try create_db_record(Req) of
                {ok, 1} ->
                    {true, Req, State};
                {error, {_, _, _, unique_violation, _, _}} ->
                    contacts_crud_errors:unique_violation(Req)
            catch
                _Error:{badkey, Key} ->
                    contacts_crud_errors:missing_key(Key, Req)
            end;
        _ ->
            contacts_crud_errors:not_found(Req)
    end;

json_to_db(Req, #state{op=update} = State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            try update_db_record(Req) of
                {ok, 1} ->
                    {true, Req, State};
                {ok, 0} ->
                    contacts_crud_errors:not_found(Req)
            catch
                _Error:{badkey, Key} ->
                    contacts_crud_errors:missing_key(Key, Req)
            end;
        _ ->
            contacts_crud_errors:not_found(Req)
    end.


get_db_list() ->
    {ok, Columns, Rows} = contacts_crud_queries:list(),
    RetMap = epgsql_to_map(Columns, Rows),
    jiffy:encode(RetMap).

create_db_record(Req) ->
    {ok, Data, _Req1} = cowboy_req:read_body(Req),
    RecordMap = jiffy:decode(Data, [return_maps]),
    Values = extract_map_values(RecordMap),
    contacts_crud_queries:create(Values).

update_db_record(Req) ->
    {ok, Data, _Req1} = cowboy_req:read_body(Req),
    Email = cowboy_req:binding(email, Req),
    RecordMap = jiffy:decode(Data, [return_maps]),
    EmailRecordMap = maps:put(<<"email">>, Email, RecordMap),
    Values = extract_map_values(EmailRecordMap),
    contacts_crud_queries:update(Values).

delete_db_record(Req) ->
    Email = cowboy_req:binding(email, Req),
    contacts_crud_queries:flag_as_inactive(Email).

extract_map_values(RecordMap) ->
    [
        maps:get(<<"email">>, RecordMap),
        maps:get(<<"name">>, RecordMap),
        maps:get(<<"surname">>, RecordMap),
        maps:get(<<"phone_number">>, RecordMap)
    ].


epgsql_to_map(Columns, Rows) ->
    Keys = lists:map(fun(Col) ->
        element(2, Col)
    end, Columns),
    lists:map(fun(Val) ->
        lists:foldl(fun(K, Acc2) ->
            Idx = map_size(Acc2) + 1,
            maps:put(K, element(Idx, Val), Acc2)
        end, maps:new(), Keys)
    end, Rows).



-ifdef(TEST).

epgsql_to_map_test() ->
    Columns = [
        {column, <<"id">>, int4, 4, -1, 0},
        {column, <<"name">>, text, -1, -1, 0}],
    Rows = [{<<"1">>, <<"alice">>}, {<<"2">>, <<"alice2">>}],
    [Res, Res2] = epgsql_to_map(Columns, Rows),

    ?assert(maps:get(<<"id">>, Res) == <<"1">>),
    ?assert(maps:get(<<"name">>, Res) == <<"alice">>),
    ?assert(maps:get(<<"id">>, Res2) == <<"2">>),
    ?assert(maps:get(<<"name">>, Res2) == <<"alice2">>).

epgsql_to_map_empty_test() ->
    Columns = [
        {column, <<"id">>, int4, 4, -1, 0},
        {column, <<"name">>, text, -1, -1, 0}],
    Rows = [],
    Res = epgsql_to_map(Columns, Rows),
    ?assertEqual([], Res).

-endif.