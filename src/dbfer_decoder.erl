-module(dbfer_decoder).

-export([read/1]).
-export([get_field/3, get_field_num/2]).


-define(INT, little-integer-unit:8).

-record(record, {number, name, type, size, dec}).

read(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    {Count, Start, Size} = decode_header(Bin),
    Fields = decode_field_header(Bin),
    Records = [decode_record(Fields, Size, Bin, Start, N) || N <- lists:seq(0, Count-1)],
    {Fields, lists:filter(fun(R) -> R =/= skip_deleted end, Records)}.

decode_header(Bin) ->
    Header = binary:part(Bin, 0, 30),
    <<Type:1/binary, _Date:3/binary, Count:4/?INT, Start:2/?INT, Size:2/?INT, _/binary>> = Header,
    {Count, Start, Size}.

decode_field_header(Bin) ->
    Fields = decode_field_header([], 0, decode_field_info(Bin, 0), Bin),
    Fields.
    
decode_field_header(Acc, _N, stop, _Bin) ->
    lists:reverse(Acc);

decode_field_header(Acc, N, Field, Bin) ->
    decode_field_header([Field| Acc], N+1, decode_field_info(Bin, N+1), Bin).

decode_field_info(Bin, N) ->
    Field = binary:part(Bin, 32 + N*32, 31),
    case Field of
        <<"\r", _/binary>> ->
            stop;
        <<Name:11/binary, Type:1/binary, _Disp:4/binary, Size:1/?INT, Dec:1/?INT, _/binary>> ->
            #record{number = N+1, name = decode_name(Name), type = Type, size = Size, dec = Dec}
    end.

decode_name(Name) ->
    [Res | _] = binary:split(Name, <<0>>),
    Res.

decode_record(Fields, Size, Bin, Start, N) ->
    case binary:part(Bin, Start + N*Size, 1) of
        <<"*">> ->
            skip_deleted;
        <<" ">> ->
            Record = binary:part(Bin, Start + 1 + N*Size, Size-1),
            read_fields([], Fields, 0, Record)
    end.

read_fields(Acc, [], _Start, _Record) ->
    lists:reverse(Acc);

read_fields(Acc, [FI | Fields], Start, Record) ->
    Field = binary:part(Record, Start, FI#record.size),
    read_fields([decode_field(Field, FI) | Acc], Fields, Start + FI#record.size, Record).

decode_field(Field, #record{type = <<"N">>, dec = 0}) ->
    {R, _} = string:to_integer(string:strip(binary:bin_to_list(Field))),
    R;

decode_field(Field, #record{type = <<"N">>}) ->
    {R, _} = string:to_float(string:strip(binary:bin_to_list(Field))),
    R;

decode_field(Field, #record{type = <<"D">>}) ->
    <<Year:4/binary, Month:2/binary, Day:2/binary>> = Field,
    {to_int(Year), to_int(Month), to_int(Day)};

decode_field(<<"Y">>, #record{type = <<"L">>}) ->
    true;

decode_field(<<"N">>, #record{type = <<"L">>}) ->
    false;

decode_field(Field, _FI) ->
    Field.

get_field_num(Name, FieldInfo) ->
    [N] = [F#record.number || F <- FieldInfo, F#record.name == Name],
    N.

get_field(Name, FieldInfo, Record) ->
    lists:nth(get_field_num(Name, FieldInfo), Record).

to_int(Bin) ->
    list_to_integer(binary:bin_to_list(Bin)).
