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

-module(json_schema_spec_test).

-include_lib("eunit/include/eunit.hrl").

-define(assertMatchVar(Guard, ExprVar, ExprStringVar),
        begin
          ((fun () ->
                case (ExprVar) of
                  Guard -> ok;
                  __V -> erlang:error({assertMatch,
                                       [{module, ?MODULE},
                                        {line, ?LINE},
                                        {expression, ExprStringVar},
                                        {pattern, (??Guard)},
                                        {value, __V}]})
                end
            end)())
        end).

-define(_assertMatchVar(Pattern, ExprVar, ExprStringVar),
        ?_test(?assertMatchVar(Pattern, ExprVar, ExprStringVar))).

spec_test_() ->
  DisabledTestFiles = ["optional/ecmascript-regex.json",
                       "optional/format/date-time.json",
                       "optional/format/email.json",
                       "optional/format/hostname.json",
                       "optional/format/idn-email.json",
                       "optional/format/idn-hostname.json",
                       "optional/format/iri-reference.json",
                       "optional/format/iri.json",
                       "optional/format/uri-reference.json",
                       "optional/format/uri-template.json",
                       "optional/format/uri.json",
                       "unevaluatedItems.json",
                       "unevaluatedProperties.json"],
  PrivDir = code:priv_dir(json),
  TestDir = filename:join([PrivDir, "test", "spec-tests"]),
  filelib:fold_files(TestDir, "\\.json$", true,
                     fun (FileName, Acc) ->
                         RelPath = string:prefix(FileName, TestDir ++ "/"),
                         case lists:member(RelPath, DisabledTestFiles) of
                           true ->
                             Acc;
                           false ->
                             [gen_tests_from_file(FileName, RelPath) | Acc]
                         end
                     end, []).

-spec gen_tests_from_file(file:filename(), file:filename()) ->
        [{binary(), term()}].
gen_tests_from_file(FileName, RelPath) ->
  {ok, Data} = file:read_file(FileName),
  {ok, Values} = json:parse(Data),
  lists:map(fun (Value) -> gen_test_group(Value, RelPath) end, Values).

-spec gen_test_group(json:value(), file:filename()) -> [{binary(), term()}].
gen_test_group(#{<<"description">> := Description,
                 <<"schema">> := SchemaValue,
                 <<"tests">> := Cases},
               FileName) ->
  case json_schema:parse(SchemaValue) of
    {ok, Schema} ->
      {Description, gen_test_cases(Cases, Schema, FileName)};
    {error, Reason} ->
      error({invalid_schema, FileName, SchemaValue, Reason})
  end.

-spec gen_test_cases([json:value()], json_schema:schema(), file:filename()) ->
        [{binary(), term()}].
gen_test_cases(Cases, Schema, FileName) ->
  lists:map(fun (Case) ->
                gen_test_case(Case, Schema, FileName)
            end, Cases).

-spec gen_test_case(json:value(), json_schema:schema(), file:filename()) ->
        {binary(), term()}.
gen_test_case(Value = #{<<"description">> := Description,
                        <<"data">> := Data,
                        <<"valid">> := Valid},
              Schema, FileName) ->
  Comment = case maps:find(<<"comment">>, Value) of
              {ok, String} ->
                <<" (", String/binary, ")">>;
              error ->
                <<"">>
            end,
  Result = try
             json_schema:validate(Schema, Data)
           catch
             error:Error:StackTrace ->
               error(#{schema => Schema,
                       data => Data,
                       error => Error,
                       stack_trace => StackTrace})
           end,
  ExprString = io_lib:format("json_schema:validate(~tp, ~tp)", [Schema, Data]),
  Assertion = case Valid of
                true ->
                  ?_assertMatchVar(ok, Result, ExprString);
                false ->
                  ?_assertMatchVar({error, _}, Result, ExprString)
              end,
  Label = io_lib:format(<<"~ts - ~ts~ts">>, [FileName, Description, Comment]),
  {iolist_to_binary(Label), Assertion}.
