-module(contacts_crud_types).
-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-export_type([
    startlink_err/0,
    startlink_ret/0
]).