%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(json_schema_formats).

-export([validate/2,
         validate_date_time/1, validate_date/1, validate_time/1,
         validate_duration/1,
         validate_email/1, validate_idn_email/1,
         validate_hostname/1, validate_idn_hostname/1,
         validate_ipv4/1, validate_ipv6/1,
         validate_uri/1, validate_uri_reference/1,
         validate_iri/1, validate_iri_reference/1,
         validate_uuid/1,
         validate_uri_template/1,
         validate_json_pointer/1, validate_relative_json_pointer/1,
         validate_regex/1]).

-spec validate(Format :: binary(), Data :: binary()) -> ok | {error, term()}.
validate(<<"date-time">>, Data) ->
  validate_date_time(Data);
validate(<<"date">>, Data) ->
  validate_date(Data);
validate(<<"time">>, Data) ->
  validate_time(Data);
validate(<<"duration">>, Data) ->
  validate_duration(Data);
validate(<<"email">>, Data) ->
  validate_email(Data);
validate(<<"idn-email">>, Data) ->
  validate_idn_email(Data);
validate(<<"hostname">>, Data) ->
  validate_hostname(Data);
validate(<<"idn-hostname">>, Data) ->
  validate_idn_hostname(Data);
validate(<<"ipv4">>, Data) ->
  validate_ipv4(Data);
validate(<<"ipv6">>, Data) ->
  validate_ipv6(Data);
validate(<<"uri">>, Data) ->
  validate_uri(Data);
validate(<<"uri-reference">>, Data) ->
  validate_uri_reference(Data);
validate(<<"iri">>, Data) ->
  validate_iri(Data);
validate(<<"iri-reference">>, Data) ->
  validate_iri_reference(Data);
validate(<<"uuid">>, Data) ->
  validate_uuid(Data);
validate(<<"uri-template">>, Data) ->
  validate_uri_template(Data);
validate(<<"json-pointer">>, Data) ->
  validate_json_pointer(Data);
validate(<<"relative-json-pointer">>, Data) ->
  validate_relative_json_pointer(Data);
validate(<<"regex">>, Data) ->
  validate_regex(Data);
validate(_, _) ->
  {error, unknown_format}.

-spec validate_date_time(binary()) -> ok | {error, term()}.
validate_date_time(Data) ->
  DateTime = rfc3339_date_time_pattern(),
  re_match(Data, <<"^", DateTime/binary, "$">>, [caseless]).

-spec validate_date(binary()) -> ok | {error, term()}.
validate_date(Data) ->
  Date = rfc3339_full_date_pattern(),
  re_match(Data, <<"^", Date/binary, "$">>, []).

-spec validate_time(binary()) -> ok | {error, term()}.
validate_time(Data) ->
  Time = rfc3339_full_time_pattern(),
  re_match(Data, <<"^", Time/binary, "$">>, []).

-spec rfc3339_date_time_pattern() -> binary().
rfc3339_date_time_pattern() ->
  Date = rfc3339_full_date_pattern(),
  Time = rfc3339_full_time_pattern(),
  <<"^", Date/binary, "T", Time/binary, "$">>.

-spec validate_duration(binary()) -> ok | {error, term()}.
validate_duration(Data) ->
  Duration = rfc3339_duration_pattern(),
  re_match(Data, <<"^", Duration/binary, "$">>, []).

-spec rfc3339_full_date_pattern() -> binary().
rfc3339_full_date_pattern() ->
  <<"(\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))">>.

-spec rfc3339_full_time_pattern() -> binary().
rfc3339_full_time_pattern() ->
  Hour = <<"([01]\\d|2[0-3])">>,
  Minute = <<"([0-5]\\d)">>,
  Second = <<"([0-5]\\d|60)">>,
  FractionalSecond = <<"(\\.\\d+)">>,
  <<"(", Hour/binary, ":", Minute/binary, ":", Second/binary,
    FractionalSecond/binary, "?",
    "(Z|[+-]", Hour/binary, ":", Minute/binary,"))">>.

-spec rfc3339_duration_pattern() -> binary().
rfc3339_duration_pattern() ->
  Second = <<"(\\d+S)">>,
  Minute = <<"((\\d+M)", Second/binary, "?)">>,
  Hour = <<"((\\d+H)", Minute/binary, "?)">>,
  Time = <<"(T(", Hour/binary, "|", Minute/binary, "|", Second/binary, "))">>,
  Day = <<"(\\d+D)">>,
  Week = <<"(\\d+W)">>,
  Month = <<"((\\d+M)", Day/binary, "?)">>,
  Year = <<"(\\d+Y)", Month/binary, "?">>,
  Date = <<"((", Day/binary, "|", Month/binary, "|", Year/binary, ")",
           Time/binary, "?)">>,
  <<"P(", Date/binary, "|", Time/binary, "|", Week/binary, ")">>.

-spec validate_email(binary()) -> ok | {error, term()}.
validate_email(Data) ->
  case is_ascii_only(Data) of
    ok ->
      Email = email_pattern(),
      re_match(Data, <<"^", Email/binary, "$">>, []);
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_idn_email(binary()) -> ok | {error, term()}.
validate_idn_email(Data) ->
  Email = email_pattern(),
  re_match(Data, <<"^", Email/binary, "$">>, []).

-spec email_pattern() -> binary().
email_pattern() ->
  Hostname = hostname_pattern(),
  <<"[^@]+@", Hostname/binary>>.

-spec validate_hostname(binary()) -> ok | {error, term()}.
validate_hostname(Data) ->
  case is_ascii_only(Data) of
    ok ->
      Hostname = hostname_pattern(),
      re_match(Data, <<"^", Hostname/binary, "$">>, []);
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_idn_hostname(binary()) -> ok | {error, term()}.
validate_idn_hostname(Data) ->
  Hostname = hostname_pattern(),
  re_match(Data, <<"^", Hostname/binary, "$">>, []).

-spec hostname_pattern() -> binary().
hostname_pattern() ->
  Segment = <<"([\\p{L}\\p{N}][\\p{L}\\p{N}\-]*)">>,
  <<Segment/binary, "(\\.", Segment/binary, ")*", "\\.?">>.

-spec validate_ipv4(binary()) -> ok | {error, term()}.
validate_ipv4(Data) ->
  case inet:parse_ipv4strict_address(binary_to_list(Data)) of
    {ok, _} ->
      ok;
    {error, einval} ->
      {error, invalid_format}
  end.

-spec validate_ipv6(binary()) -> ok | {error, term()}.
validate_ipv6(Data) ->
  %% Do a quick check first since inet:parse_ipv6strict_address/1 accepts a
  %% scope but the JSON Schema test suite does not.
  case re_match(Data, <<"^[0-9a-fA-F:\.]+$">>, []) of
    ok ->
      case inet:parse_ipv6strict_address(binary_to_list(Data)) of
        {ok, _} ->
          ok;
        {error, einval} ->
          {error, invalid_format}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_uri(binary()) -> ok | {error, term()}.
validate_uri(Data) ->
  case is_ascii_only(Data) of
    ok ->
      case uri:parse(Data) of
        {ok, #{scheme := _}} ->
          ok;
        {ok, _} ->
          {error, invalid_uri};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_uri_reference(binary()) -> ok | {error, term()}.
validate_uri_reference(Data) ->
  case is_ascii_only(Data) of
    ok ->
      case uri:parse(Data) of
        {ok, _} ->
          ok;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_iri(binary()) -> ok | {error, term()}.
validate_iri(Data) ->
  case uri:parse(Data) of
    {ok, #{scheme := _}} ->
      ok;
    {ok, _} ->
      {error, invalid_uri};
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_iri_reference(binary()) -> ok | {error, term()}.
validate_iri_reference(Data) ->
  case uri:parse(Data) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_uuid(binary()) -> ok | {error, term()}.
validate_uuid(Data) ->
  RE = <<"^[\\da-fA-F]{8}(-[\\da-fA-F]{4}){3}-[\\da-fA-F]{12}$">>,
  re_match(Data, RE, []).

-spec validate_uri_template(binary()) -> ok | {error, term()}.
validate_uri_template(_Data) ->
  %% TODO (do not forget to re-enable uri-template spec tests)
  error(unimplemented).

-spec validate_json_pointer(binary()) -> ok | {error, term()}.
validate_json_pointer(Data) ->
  case json_pointer:parse(Data) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

-spec validate_relative_json_pointer(binary()) -> ok | {error, term()}.
validate_relative_json_pointer(Data) ->
  %% TODO Use json_relative_pointer:parse/1 once it is implemented
  RE = <<"^(0|[1-9][0-9]*)(#|(/([^~/]|~[01])*)*)$">>,
  re_match(Data, RE, []).

-spec validate_regex(binary()) -> ok | {error, term()}.
validate_regex(Data) ->
  %% This will accept lots of regexp constructions which are supported by re
  %% but not by ECMA 262, but this will do for the time being.
  case re:compile(Data) of
    {ok, _} ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

%% Of course re:compile_option() isn't exported either...
-spec re_match(Subject :: binary(), Pattern :: binary(), Options :: [term()]) ->
        ok | {error, term()}.
re_match(Subject, RE, Options) ->
  case re:run(Subject, RE, Options) of
    {match, _} ->
      ok;
    nomatch ->
      {error, invalid_format}
  end.

-spec is_ascii_only(binary()) -> ok | {error, invalid_characters}.
is_ascii_only(Data) ->
  case re:run(Data, <<"[^\\x00-\x7f]">>) of
    {match, _} ->
      {error, invalid_characters};
    nomatch ->
      ok
  end.
