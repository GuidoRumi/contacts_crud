-define(CONTACT_1_EMAIL, <<"user@example.com">>).
-define(CONTACT_1_NAME, <<"examplename">>).
-define(CONTACT_1_SURNAME, <<"examplesurname">>).
-define(CONTACT_1_PHONE_NUMBER, <<"+5412123331">>).


-define(CONTACT_MAP_1, #{
        <<"email">> => ?CONTACT_1_EMAIL,
        <<"name">> => ?CONTACT_1_NAME,
        <<"surname">> => ?CONTACT_1_SURNAME,
        <<"phone_number">> => ?CONTACT_1_PHONE_NUMBER
    }
).

-define(CONTACT_UPDATE_MAP_1_NAME,
    <<"examplename2">>
).
-define(CONTACT_UPDATE_MAP_1_SURNAME,
    <<"examplesurname2">>
).
-define(CONTACT_UPDATE_MAP_1_PHONE_NUMBER,
    <<"phone_number">>
).

-define(CONTACT_UPDATE_MAP_1, #{
        <<"name">> => ?CONTACT_UPDATE_MAP_1_NAME,
        <<"surname">> => ?CONTACT_UPDATE_MAP_1_SURNAME,
        <<"phone_number">> => ?CONTACT_UPDATE_MAP_1_PHONE_NUMBER
    }
).

-define(CONTACT_UPDATED_MAP_1, #{
        <<"name">> => ?CONTACT_UPDATE_MAP_1_NAME,
        <<"surname">> => ?CONTACT_UPDATE_MAP_1_SURNAME,
        <<"phone_number">> => ?CONTACT_UPDATE_MAP_1_PHONE_NUMBER,
        <<"email">> => <<"user@example.com">>
    }
).
