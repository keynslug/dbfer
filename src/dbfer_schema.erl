%%
%% Schema definitions.

-module(dbfer_schema).

%%

-export([new/1]).

-export([chars/2]).
-export([logical/1]).
-export([numeric/3]).
-export([float/3]).
-export([date/1]).

%%

-opaque field() :: {byte(), binary(), 1..255, 0..20}.

-export_type([field/0]).

%%
%% Schema definitions

-spec new([field()]) -> dbfer:schema().

new(Fields) ->
    %% would be great to check for uniqueness
    Fields.

%%
%% Fields definitions

-type field_name() :: binary() | string().

-spec chars(field_name(), 1..255) -> field().

chars(Name, MaxLength) ->
    get_field_def($C, Name, MaxLength).

-spec logical(field_name()) -> field().

logical(Name) ->
    get_field_def($L, Name, 1).

-spec numeric(field_name(), 1..21, 0..20) -> field().

numeric(Name, MaxLength, Precision) ->
    get_field_def($N, Name, MaxLength, Precision).

-spec float(field_name(), 1..21, 0..20) -> field().

float(Name, MaxLength, Precision) ->
    get_field_def($F, Name, MaxLength, Precision).

-spec date(field_name()) -> field().

date(Name) ->
    get_field_def($D, Name, 8).

%%

get_field_def(Type, Name, Length) ->
    get_field_def(Type, Name, Length, 0).

get_field_def(Type, Name, Length, Precision) ->
    FName = validate_name(Name),
    FLength = validate_length(Length, Type),
    FPrecision = validate_precision(Precision, Length),
    {Type, FName, FLength, FPrecision}.

%%

validate_length(Length, $C) when Length > 0, Length < 255 -> Length;
validate_length(Length, $L) -> Length;
validate_length(Length, $N) when Length > 0, Length < 21 -> Length;
validate_length(Length, $F) when Length > 0, Length < 21 -> Length;
validate_length(Length, $D) -> Length;

validate_length(_, _) ->
    error({badarg, length}).

validate_precision(0, _Length) -> 0;
validate_precision(Precision, Length) when Precision >= 0, Precision < Length -> Precision;

validate_precision(_, _) ->
    error({badarg, precision}).

validate_name(Name) when is_list(Name) ->
    validate_name(iolist_to_binary(Name));

validate_name(Name) when is_binary(Name), byte_size(Name) =< 10 ->
    case is_alphanum_string(Name) of
        true ->
            Name;
        false ->
            error({badarg, name})
    end;

validate_name(_) ->
    error({badarg, name}).

%%

is_alphanum_string(Subject) ->
    nomatch =:= re:run(Subject, <<"[^a-zA-Z0-9_]">>, [{capture, none}]).
