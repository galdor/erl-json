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

-module(json_schema_formats_test).

-include_lib("eunit/include/eunit.hrl").

validate_date_time_test_() ->
  %% Most of it is already covered by date and time tests
  Validate = fun json_schema_formats:validate_date_time/1,
  [?_assertEqual(ok, Validate(<<"2010-01-01T10:20:30Z">>)),
   ?_assertEqual(ok, Validate(<<"2010-07-20T00:00:00+02:00">>)),
   ?_assertEqual(ok, Validate(<<"2010-12-31T23:59:60-05:30">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-01t10:20:30z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-01-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30Z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-01-01T10:20:30">>))].

validate_date_test_() ->
  Validate = fun json_schema_formats:validate_date/1,
  [?_assertEqual(ok, Validate(<<"2010-01-01">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-09">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-10">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-19">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-20">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-29">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-29">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-30">>)),
   ?_assertEqual(ok, Validate(<<"2010-01-31">>)),
   ?_assertEqual(ok, Validate(<<"2010-12-31">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"abcd-01-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-00-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-13-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-20-01">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-01-00">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"2010-01-32">>))].

validate_time_test_() ->
  Validate = fun json_schema_formats:validate_time/1,
  [?_assertEqual(ok, Validate(<<"00:00:00Z">>)),
   ?_assertEqual(ok, Validate(<<"23:59:59Z">>)),
   ?_assertEqual(ok, Validate(<<"23:59:60Z">>)),
   ?_assertEqual(ok, Validate(<<"10:20:30+02:00">>)),
   ?_assertEqual(ok, Validate(<<"10:20:30-07:30">>)),
   ?_assertEqual(ok, Validate(<<"10:20:30+23:59">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<":30">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:61Z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"24:00:00Z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"23:60:00Z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"23:60:00Z">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30+">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30+02">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30+02:3">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30+60:00">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10:20:30+23:60">>))].

validate_duration_test_() ->
  Validate = fun json_schema_formats:validate_duration/1,
  [?_assertEqual(ok, Validate(<<"P10W">>)),
   ?_assertEqual(ok, Validate(<<"PT3H">>)),
   ?_assertEqual(ok, Validate(<<"PT10H10M">>)),
   ?_assertEqual(ok, Validate(<<"PT5H30M10S">>)),
   ?_assertEqual(ok, Validate(<<"PT10M">>)),
   ?_assertEqual(ok, Validate(<<"PT20M30S">>)),
   ?_assertEqual(ok, Validate(<<"PT120S">>)),
   ?_assertEqual(ok, Validate(<<"P1Y">>)),
   ?_assertEqual(ok, Validate(<<"P2Y5M">>)),
   ?_assertEqual(ok, Validate(<<"P20Y2M30D">>)),
   ?_assertEqual(ok, Validate(<<"P12M">>)),
   ?_assertEqual(ok, Validate(<<"P1M3D">>)),
   ?_assertEqual(ok, Validate(<<"P100D">>)),
   ?_assertEqual(ok, Validate(<<"P1YT10H20M30S">>)),
   ?_assertEqual(ok, Validate(<<"P1Y2M10DT5H10M20S">>)),
   ?_assertEqual(ok, Validate(<<"P10MT10M">>)),
   ?_assertEqual(ok, Validate(<<"P50DT2H">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"10M">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"P5S">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"PT2Y">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"P10D20D">>))].

validate_email_test_() ->
  Validate = fun json_schema_formats:validate_email/1,
  [?_assertEqual(ok, Validate(<<"foo@example.com">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo@">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"example.com">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"@example.com">>)),
   ?_assertEqual({error, invalid_characters}, Validate(<<"été@example.com">>))].

validate_idn_email_test_() ->
  Validate = fun json_schema_formats:validate_idn_email/1,
  [?_assertEqual(ok, Validate(<<"foo@example.com">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo@">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"example.com">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"@example.com">>)),
   ?_assertEqual(ok, Validate(<<"été@example.com">>))].

validate_hostname_test_() ->
  Validate = fun json_schema_formats:validate_hostname/1,
  [?_assertEqual(ok, Validate(<<"com">>)),
   ?_assertEqual(ok, Validate(<<"com.">>)),
   ?_assertEqual(ok, Validate(<<"example.com">>)),
   ?_assertEqual(ok, Validate(<<"example.com.">>)),
   ?_assertEqual(ok, Validate(<<"a.example.com">>)),
   ?_assertEqual(ok, Validate(<<"a.example.com.">>)),
   ?_assertEqual({error, invalid_characters}, Validate(<<"été.example.com">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo..">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<".foo">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"-foo.bar">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo..bar">>))].

validate_idn_hostname_test_() ->
  Validate = fun json_schema_formats:validate_idn_hostname/1,
  [?_assertEqual(ok, Validate(<<"com">>)),
   ?_assertEqual(ok, Validate(<<"com.">>)),
   ?_assertEqual(ok, Validate(<<"example.com">>)),
   ?_assertEqual(ok, Validate(<<"example.com.">>)),
   ?_assertEqual(ok, Validate(<<"été.example.com">>)),
   ?_assertEqual(ok, Validate(<<"été.example.com.">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo..">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<".foo">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"-foo.bar">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo..bar">>))].

validate_ipv4_test_() ->
  Validate = fun json_schema_formats:validate_ipv4/1,
  [?_assertEqual(ok, Validate(<<"192.168.0.1">>)),
   ?_assertEqual(ok, Validate(<<"255.255.255.255">>)),
   ?_assertEqual(ok, Validate(<<"0.0.0.0">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"192">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"1.2.3.4.5">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"1.2.3.256">>))].

validate_ipv6_test_() ->
  Validate = fun json_schema_formats:validate_ipv6/1,
  [?_assertEqual(ok, Validate(<<"::">>)),
   ?_assertEqual(ok, Validate(<<"::1">>)),
   ?_assertEqual(ok, Validate(<<"ff01::43">>)),
   ?_assertEqual(ok, Validate(<<"1080::8:800:200c:417a">>)),
   ?_assertEqual(ok, Validate(<<"fc80:2:3:4:5:6:7:8">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<":">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"1.2.3.4.5.6.7.8.9">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"12345::1">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"123g::1">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<":abcdef">>))].

validate_uri_test_() ->
  Validate = fun json_schema_formats:validate_uri/1,
  [?_assertEqual(ok, Validate(<<"http://example.com/a/b/c?foo=bar#abc">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"//example.com/foo">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"example.com/foo">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"/foo/bar">>)),
   ?_assertEqual({error, invalid_characters},
                 Validate(<<"http://example.com/a/b/c?foo=bar#été">>))].

validate_uri_reference_test_() ->
  Validate = fun json_schema_formats:validate_uri_reference/1,
  [?_assertEqual(ok, Validate(<<"http://example.com/a/b/c?foo=bar#abc">>)),
   ?_assertEqual(ok, Validate(<<"//example.com/foo">>)),
   ?_assertEqual(ok, Validate(<<"/foo/bar">>)),
   ?_assertEqual({error, invalid_characters},
                 Validate(<<"/foo/été">>))].

validate_iri_test_() ->
  Validate = fun json_schema_formats:validate_iri/1,
  [?_assertEqual(ok, Validate(<<"http://example.com/a/b/c?foo=bar#été">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"//example.com/été">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"example.com/été">>)),
   ?_assertEqual({error, invalid_uri}, Validate(<<"/foo/été">>))].

validate_iri_reference_test_() ->
  Validate = fun json_schema_formats:validate_iri_reference/1,
  [?_assertEqual(ok, Validate(<<"http://example.com/a/b/c?foo=bar#été">>)),
   ?_assertEqual(ok, Validate(<<"//example.com/été">>)),
   ?_assertEqual(ok, Validate(<<"/foo/été">>))].

validate_uuid_test_() ->
  Validate = fun json_schema_formats:validate_uuid/1,
  [?_assertEqual(ok, Validate(<<"54a37602-2d48-45ce-a649-fe4305ff4bd1">>)),
   ?_assertEqual(ok, Validate(<<"F90168E7-75B6-40B2-BFAD-3FEE48A40F18">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"123456">>)),
   ?_assertEqual({error, invalid_format},
                 Validate(<<"12345678-9012-3456-7890-12345678901">>)),
   ?_assertEqual({error, invalid_format},
                 Validate(<<"12345678-9012-3456-789-123456789012">>)),
   ?_assertEqual({error, invalid_format},
                 Validate(<<"12345678-9012-3456-7890-12345678901g">>)),
   ?_assertEqual({error, invalid_format},
                 Validate(<<"12345678:9012:3456:7890:123456789012">>))].

validate_uri_template_test_() ->
  Validate = fun json_schema_formats:validate_uri_template/1,
  [?_assertError(unimplemented, Validate(<<"">>))].

validate_json_pointer_test_() ->
  Validate = fun json_schema_formats:validate_json_pointer/1,
  [?_assertEqual(ok, Validate(<<"/foo">>)),
   ?_assertEqual(ok, Validate(<<"/a/b/c">>)),
   ?_assertEqual(ok, Validate(<<"/a~0b/~1c">>)),
   ?_assertEqual({error, truncated_escape_sequence}, Validate(<<"/foo/~">>)),
   ?_assertEqual({error, {invalid_escape_sequence, <<"~3">>}},
                 Validate(<<"/foo/~3">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"foo/bar">>))].

validate_relative_json_pointer_test_() ->
  Validate = fun json_schema_formats:validate_relative_json_pointer/1,
  [?_assertEqual(ok, Validate(<<"0/foo">>)),
   ?_assertEqual(ok, Validate(<<"10/a/b/c">>)),
   ?_assertEqual(ok, Validate(<<"25/a~0b/~1c">>)),
   ?_assertEqual(ok, Validate(<<"3#">>)),
   ?_assertEqual(ok, Validate(<<"3">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"02#">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"0foo/bar">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"0/foo/~">>)),
   ?_assertEqual({error, invalid_format}, Validate(<<"0/foo/~3">>))].

validate_regex_test_() ->
  Validate = fun json_schema_formats:validate_regex/1,
  [?_assertEqual(ok,
                 Validate(<<"">>)),
   ?_assertEqual(ok,
                 Validate(<<"[a-z]+">>)),
   ?_assertMatch({error, {_Msg, _Position}},
                 Validate(<<"[a-z">>))].
