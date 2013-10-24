dbfer
=====

A simple tool to accumulate a number of records and save them to DBF formatted file.
Plain Erlang.

How to use
----------

Straightforward usage example now follows.

```

very_clever_and_accurate_name_of_the_function() ->
    Schema = dbfer_schema:new([
        dbfer_schema:numeric("AMOUNT", 17, 4),
        dbfer_schema:chars("A_AUDIT_NO", 6),
        dbfer_schema:chars("C_AUDIT_NO", 6),
        dbfer_schema:chars("AUTHSOURCE", 1),
        dbfer_schema:chars("AUTH_CODE", 6),
        dbfer_schema:numeric("BR_ID", 3, 0),
        dbfer_schema:date("DATE")
    ]),
    Records = [
        [124200, <<"666ABC">>, <<"1234">>, "", "CODE42", 42, calendar:universal_time()],
        ...
    ],
    dbfer:save("trash-me.dbf", Schema, Records).

```
