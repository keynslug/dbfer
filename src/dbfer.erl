%%
%% Library interface.

-module(dbfer).

%%

-export([serialize/2]).
-export([serialize/3]).
-export([save/3]).
-export([save/4]).

%%

-type field() :: dbfer_schema:field().
-type schema() :: [field()].

-type datarecord() :: [binary() | string() | number() | boolean() | calendar:datetime()].

-export_type([field/0, schema/0]).

%%

-define(FILESIG, 3).
-define(DATAMARK, <<13>>).
-define(RECORDMARK, <<32>>).
-define(ENDMARK, <<26>>).

%%

-spec serialize(schema(), [datarecord()]) -> iolist().

serialize(Fields, Records) ->
    serialize(Fields, Records, []).

-spec serialize(schema(), [datarecord()], proplists:proplist()) -> iolist().

serialize(Fields, Records, Opts) ->
    FileHeader = encode_file_header(length(Records), Fields, Opts),
    FieldsHeader = [dbfer_encoder:encode_header(F) || F <- Fields],
    RecordsData = [encode_record(Fields, R, ?RECORDMARK) || R <- Records],
    [FileHeader, FieldsHeader, ?DATAMARK, RecordsData, ?ENDMARK].

-spec save(file:filename(), schema(), [datarecord()]) -> ok | {error, atom()}.

save(Filename, Fields, Records) ->
    save(Filename, Fields, Records, []).

-spec save(file:filename(), schema(), [datarecord()], proplists:proplist()) -> ok | {error, atom()}.

save(Filename, Fields, Records, Opts) ->
    file:write_file(Filename, serialize(Fields, Records, Opts)).

%%

encode_file_header(N, Fields, Opts) ->
    {Year, Month, Day} = get_opt(date, Opts),
    RecordStripe = lists:sum([L || {_, _, L, _} <- Fields]) + 1,
    FieldsStripe = (length(Fields) + 1) * 32 + 1,
    <<?FILESIG, (Year - 1900), (Month - 1), Day, N:4/little-unit:8,   %% 8 bytes
        FieldsStripe:2/little-unit:8, RecordStripe:2/little-unit:8,   %% 4 bytes
        0:20/unit:8>>.                                                %% 20 bytes

encode_record([], [], Acc) ->
    Acc;

encode_record([Field | RestFields], [Value | RestValues], Acc) ->
    Data = dbfer_encoder:encode(Field, Value),
    encode_record(RestFields, RestValues, <<Acc/binary, Data/binary>>).

%%

get_opt(Key, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        false ->
            get_defaut_opt(Key);
        Date ->
            Date
    end.

get_defaut_opt(date) ->
    {Date, _Time} = calendar:universal_time(),
    Date.
