-define(CONTACT_1_EMAIL, <<"user@example.com">>).
-define(CONTACT_1, [?CONTACT_1_EMAIL, <<"contact_name_1">>, <<"contact_surname_1">>, <<"phone_number">>, true]).

-define(INSERT_QUERY,
    "insert into contacts_app.contacts
    (email, name, surname, phone_number, active) values ($1, $2, $3, $4, $5)").

-define(SELECT_EMAIL_QUERY,
    "select email from contacts_app.contacts"
).

-define(SELECT_WRONG_TABLE,
    "select email from contacts_app.contact"
).

