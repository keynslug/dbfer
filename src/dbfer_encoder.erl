%%
%% Dbf data and header encoder.

-module(dbfer_encoder).

%%

-export([encode/2]).
-export([encode_header/1]).

%%

-spec encode(dbfer:field(), any()) -> binary().

encode({Type, Name, MaxLength, Precision}, Data) ->
    try
        encode(Data, Type, MaxLength, Precision)
    catch
        error:_ ->
            error({badarg, Name})
    end.

encode(String, $C, MaxLength, _) ->
    encode_chars(String, MaxLength);

encode(true, $L, _, _) ->
    <<"Y">>;

encode(false, $L, _, _) ->
    <<"N">>;

encode(N, $N, MaxLength, 0) ->
    pad_left(integer_to_binary(N), MaxLength);

encode(N, $N, MaxLength, Precision) ->
    Bin0 = integer_to_binary(abs(N)),
    Bin1 = pad_left(Bin0, max(Precision + 1, byte_size(Bin0)), $0),
    SignBin = if N < 0 -> <<$->>; true -> <<>> end,
    IntLength = byte_size(Bin1) - Precision,
    <<IntBin:IntLength/binary, FracBin/binary>> = Bin1,
    pad_left(<<SignBin/binary, IntBin/binary, $., FracBin/binary>>, MaxLength);

encode(X, $F, MaxLength, 0) ->
    pad_left(integer_to_binary(trunc(X)), MaxLength);

encode(X, $F, MaxLength, Precision) ->
    Xi = trunc(X),
    Xd = trunc(abs(X - Xi) * math:pow(10, Precision)),
    Xib = integer_to_binary(Xi),
    Xdb = pad_left(integer_to_binary(Xd), Precision, $0),
    pad_left(<<Xib/binary, $., Xdb/binary>>, MaxLength);

encode({{Year, Month, Day}, {_, _, _}}, $D, _, _) ->
    YearPart = pad_left(integer_to_binary(Year), 4, $0),
    MonthPart = pad_left(integer_to_binary(Month), 2, $0),
    DayPart = pad_left(integer_to_binary(Day), 2, $0),
    <<YearPart/binary, MonthPart/binary, DayPart/binary>>.

%%

-spec encode_header(dbfer:field()) -> binary().

encode_header({Type, Name, MaxLength, Precision}) ->
    NamePart = pad_right(Name, 10 + 1, 0),
    <<NamePart/binary, Type, 0:4/unit:8, MaxLength, Precision, 0:14/unit:8>>.

%%

encode_chars(Bin, MaxLength) when is_binary(Bin) ->
    pad_right(Bin, MaxLength, $\s);

encode_chars(String, MaxLength) ->
    encode_chars(iolist_to_binary(String), MaxLength).

%%

pad_left(Bin, Length) ->
    pad_left(Bin, Length, $\s).

pad_left(Bin, Length, C) ->
    Pad = make_pad(Length - byte_size(Bin), C),
    <<Pad/binary, Bin/binary>>.

pad_right(Bin, Length, C) ->
    Pad = make_pad(Length - byte_size(Bin), C),
    <<Bin/binary, Pad/binary>>.

make_pad(N, C) when N >= 0 ->
    make_pad(N, C, <<>>).

make_pad(0, _, Pad) ->
    Pad;

make_pad(N, C, Pad) ->
    make_pad(N - 1, C, <<Pad/binary, C>>).
