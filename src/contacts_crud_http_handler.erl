-module(contacts_crud_http_handler).
-include_lib("eunit/include/eunit.hrl").

%% exclude dialyzer for eunit tests
-dialyzer({nowarn_function,
    [epgsql_to_maps_test/0, epgsql_to_maps_empty_test/0]}).

%% Cowboy callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    delete_resource/2,
    db_to_json/2,
    json_to_db/2
]).

-record(state, {op}).

%% @doc init/2 is the first step in cowboy request flow
%% sets operation in state
%% @end
-spec init(cowboy_req:req(), nonempty_maybe_improper_list()) ->
    {'cowboy_rest', cowboy_req:req(), #state{}}.
init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

-spec allowed_methods(cowboy_req:req(), #state{}) ->
    {[<<_:24, _:_*8>>, ...], cowboy_req:req(), #state{}}.
allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
    {Methods, Req, State}.

-spec content_types_provided(cowboy_req:req(), #state{}) ->
    {[{<<_:128>>, 'db_to_json'}, ...], cowboy_req:req(), #state{}}.
content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, db_to_json}
    ], Req, State}.

-spec content_types_accepted(cowboy_req:req(), #state{}) ->
    {[{<<_:128>>, 'json_to_db'}, ...], cowboy_req:req(), #state{}}.
content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, json_to_db}
    ], Req, State}.

%% @doc delete_resource/2 calback for contacts/delete/:email
%% flags contact as inactive
%% @end
-spec delete_resource(cowboy_req:req(), #state{}) ->
    {'true', map(), _} | map().
delete_resource(Req, State) ->
    case delete_db_record(Req) of
        {ok, 1} ->
            {true, Req, State};
        {ok, 0} ->
            contacts_crud_errors:not_found(Req)
    end.

%% @doc db_to_json/2 callback for GET /contacts
-spec db_to_json(#{'method':=_, _=>_}, #state{op::'contacts' | 'update'}) ->
    {'true', #{'method':=_,  _=>_}, #state{op::'contacts' | 'update'}} | map().
db_to_json(Req, #state{op=contacts} = State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            {get_db_list(), Req, State};
        _ ->
            contacts_crud_errors:not_found(Req)
    end;

%% @doc db_to_json/2 callback for GET /contacts/:surname
%% returns a json with one record mathcing surname
%% @end
db_to_json(Req, #state{op=find_surname} = State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Surname = cowboy_req:binding(surname, Req),
            EpgsqResponse = contacts_crud_queries:find_one_surname(Surname),
            RecordMap = epgsql_to_maps(EpgsqResponse),
            ResponseBody = jiffy:encode(RecordMap),
            {ResponseBody, Req, State};
        _ ->
            contacts_crud_errors:not_found(Req)
    end.

%% @doc json_to_db/2 callback for POST /contacts, creates a new record
-spec json_to_db(#{'method':=_, _=>_}, #state{op::'contacts' | 'update'}) ->
    {'true', #{'method':=_, _=>_}, #state{op::'contacts' | 'update'}} | map().
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

%% @doc json_to_db/2 callback for put /contacts/:email, updates existing record
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

%% @doc get_db_list/0 returns a JSON of a list with every contact
-spec get_db_list() -> iodata().
get_db_list() ->
    EpgsqResponse = contacts_crud_queries:list(),
    RetMap = epgsql_to_maps(EpgsqResponse),
    jiffy:encode(RetMap).

%% @doc create_db_record/1 creates a record on db
-spec create_db_record(cowboy_req:req()) -> epgsql_cmd_equery:response().
create_db_record(Req) ->
    {ok, Data, _Req1} = cowboy_req:read_body(Req),
    RecordMap = jiffy:decode(Data, [return_maps]),
    Values = extract_map_values(RecordMap),
    contacts_crud_queries:create(Values).

%% @doc update_db_record/1 creates a record on db
-spec update_db_record(cowboy_req:req()) -> epgsql_cmd_equery:response().
update_db_record(Req) ->
    {ok, Data, _Req1} = cowboy_req:read_body(Req),
    Email = cowboy_req:binding(email, Req),
    RecordMap = jiffy:decode(Data, [return_maps]),
    EmailRecordMap = maps:put(<<"email">>, Email, RecordMap),
    Values = extract_map_values(EmailRecordMap),
    contacts_crud_queries:update(Values).

%% @doc delete_db_record/1 deletes record on db
-spec delete_db_record(cowboy_req:req()) -> epgsql_cmd_equery:response().
delete_db_record(Req) ->
    Email = cowboy_req:binding(email, Req),
    contacts_crud_queries:flag_as_inactive(Email).

-spec extract_map_values(map()) -> list().
extract_map_values(RecordMap) ->
    [
        maps:get(<<"email">>, RecordMap),
        maps:get(<<"name">>, RecordMap),
        maps:get(<<"surname">>, RecordMap),
        maps:get(<<"phone_number">>, RecordMap)
    ].

%% @doc epgsql_to_maps/1 converts a succesful epgsql return to a list of maps
-spec epgsql_to_maps(epgsql_cmd_equery:response()) -> [map()].
epgsql_to_maps(EpgsqlResponse) ->
    {ok, Columns, Rows} = EpgsqlResponse,
    Keys = lists:map(fun(Col) ->
        element(2, Col)
    end, Columns),
    lists:map(fun(Val) ->
        lists:foldl(fun(K, Acc2) ->
            Idx = map_size(Acc2) + 1,
            maps:put(K, element(Idx, Val), Acc2)
        end, maps:new(), Keys)
    end, Rows).

-ifdef(EUNIT).

epgsql_to_maps_test() ->
    Columns = [
        {column, <<"id">>, int4, 4, -1, 0},
        {column, <<"name">>, text, -1, -1, 0}],
    Rows = [{<<"1">>, <<"alice">>}, {<<"2">>, <<"alice2">>}],
    [Res, Res2] = epgsql_to_maps({ok, Columns, Rows}),

    ?assert(maps:get(<<"id">>, Res) == <<"1">>),
    ?assert(maps:get(<<"name">>, Res) == <<"alice">>),
    ?assert(maps:get(<<"id">>, Res2) == <<"2">>),
    ?assert(maps:get(<<"name">>, Res2) == <<"alice2">>).

epgsql_to_maps_empty_test() ->
    Columns = [
        {column, <<"id">>, int4, 4, -1, 0},
        {column, <<"name">>, text, -1, -1, 0}],
    Rows = [],
    Res = epgsql_to_maps({ok, Columns, Rows}),
    ?assertEqual([], Res).

-endif.