-module(contacts_crud_errors).

-export([
    missing_key/2,
    unique_violation/1,
    not_found/1
]).

missing_key(Key, Req) ->
    Response = "Missing key: " ++ Key,
    cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, Response , Req).

unique_violation(Req) ->
    Response = "Email already exists",
    cowboy_req:reply(409, #{
        <<"content-type">> => <<"application/json">>
    }, Response , Req).

not_found(Req) ->
    cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, Req).