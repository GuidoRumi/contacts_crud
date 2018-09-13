%% @doc contacts_crud_queries defines QueryStrings and executes them, returning
%% raw qpgsql responses
%% @end
-module(contacts_crud_queries).

-export([
    create/1,
    update/1,
    flag_as_inactive/1,
    find_one_surname/1,
    list/0,
    delete_inactive/0
]).

%% @doc create/1 expects a list with values corresponding to
%% columns [email, name, surname, phone_number, active] in that
%% order
%% @end
-spec create(list()) -> epgsql_cmd_equery:response().
create(Values)->
    QueryString = "insert into contacts_app.contacts
        (email, name, surname, phone_number, active)
        values ($1, $2, $3, $4, true)",
    contacts_crud_conn_mgr:execute_equery(QueryString, Values).

%% @doc update/1 expects a list with values corresponding to
%% columns [email, name, surname, phone_number, active] in that
%% order
%% @end
-spec update(list()) -> epgsql_cmd_equery:response().
update(Values) ->
    QueryString = "update contacts_app.contacts
        set name = $2, surname = $3, phone_number = $4
        where email = $1",
    contacts_crud_conn_mgr:execute_equery(QueryString, Values).

%% @doc flag_as_inactive/1 expects an iodata() corresponding
%% to contact's email and updates it's active field to false
%% @end
-spec flag_as_inactive(iodata()) -> epgsql_cmd_equery:response().
flag_as_inactive(Email) ->
    QueryString = "update contacts_app.contacts
        set active = false
        where email = $1",
    contacts_crud_conn_mgr:execute_equery(QueryString, [Email]).

%% @doc find_one_surname/1 expects an iodata() corresponding
%% to contact's surname
%% @end
-spec find_one_surname(iodata()) -> epgsql_cmd_equery:response().
find_one_surname(Surname) ->
    QueryString = "select email, name, surname, phone_number
        from contacts_app.contacts
        where surname = $1
        limit 1",
    contacts_crud_conn_mgr:execute_equery(QueryString, [Surname]).

%% @doc list/0 returns a raw epsql return with
%% list with every record
%% @end
-spec list() -> epgsql_cmd_equery:response().
list() ->
    QueryString = "select email, name, surname, phone_number
        from contacts_app.contacts
        order by surname asc",
    contacts_crud_conn_mgr:execute_equery(QueryString, []).

%% @doc delete_inactive/0 deletes every record that
%% has it's active field set to false
%% @end
-spec delete_inactive() -> epgsql_cmd_equery:response().
delete_inactive() ->
     QueryString = "delete from contacts_app.contacts
        where active = false",
    contacts_crud_conn_mgr:execute_equery(QueryString, []).
