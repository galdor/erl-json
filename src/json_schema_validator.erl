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

-module(json_schema_validator).

-export([validate/3]).

-type validator() :: #{options := json_schema:options(),
                       value := json:value(),
                       value_path := json_pointer:pointer(),
                       schema := json_schema:schema(),
                       schema_path := json_pointer:pointer(),
                       data := validation_data(),
                       base_uri := uri:uri() | undefined}.

%% We need a place to store validation data for the current schema and
%% value. If we were to support annotations, it would simply by a set of them.
-type validation_data() ::
        #{items => non_neg_integer(), % number of items matched
          contains => non_neg_integer(), % number of successfully matched items
          properties => #{binary() := boolean()}, % matched member names
          if_ => boolean(), % success or failure of the condition
          content_encoding => binary()}. % decoded data

%% Keywords have to be validated in a precise orders because some of them
%% depend on data yielded from the validation of others.
-spec keywords() -> [atom()].
keywords() ->
  [items,
   additional_items, % depends on 'items'
   unevaluated_items, % depends on 'additional_items'
   contains,
   properties,
   pattern_properties,
   additional_properties, % depends on 'properties' and 'pattern_properties'
   unevaluated_properties, % depends on 'additional_properties'
   dependent_schemas,
   property_names,
   if_, then, else, % 'then' and 'else' depend on 'if_'
   all_of, any_of, one_of,
   not_,
   const, enum, type,
   multiple_of, maximum, exclusive_maximum, minimum, exclusive_minimum,
   max_length, min_length, pattern,
   max_items, min_items, unique_items,
   max_contains, min_contains, % depends on 'contains'
   max_properties, min_properties, required, dependent_required,
   format,
   content_encoding,
   content_media_type, % depends on 'content_encoding'
   content_schema].

-spec new_validator(json_schema:schema(), json:value(),
                    json_schema:options()) ->
        validator().
new_validator(Schema, Value, Options) ->
  BaseURI = case maps:find(base_uri, Options) of
              {ok, URIString} when is_binary(URIString) ->
                case uri:parse(URIString) of
                  {ok, URI} ->
                    URI;
                  Error ->
                    throw(Error)
                end;
              {ok, URI} ->
                URI;
              error ->
                undefined
            end,
  #{options => Options,
    value => Value,
    value_path => [],
    schema => Schema,
    schema_path => [],
    data => #{},
    base_uri => BaseURI}.

-spec validate(json_schema:schema(), json:value(), json_schema:options()) ->
        ok | {error, json_schema:validation_errors()}.
validate(Schema, Value, Options) ->
  try
    Validator = new_validator(Schema, Value, Options),
    case validate1(Validator) of
      {[], _} ->
        ok;
      {Errors, _} ->
        {error, Errors}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec validate1(validator()) -> {json_schema:validation_errors(), validator()}.
validate1(V = #{schema := true}) ->
  {[], V};
validate1(V = #{schema := false}) ->
  {[validation_error(V, invalid_value)], V};
validate1(V = #{schema := Schema}) ->
  Keywords = keywords(),
  F = fun (Keyword, {Errors, V2}) ->
          case maps:find(Keyword, Schema) of
            {ok, KeywordValue} ->
              {Errors2, V3} = validate_keyword(Keyword, KeywordValue, V2),
              Errors3 = lists:map(fun (Error) ->
                                      Error#{keyword => Keyword}
                                  end, Errors2),
              {Errors3 ++ Errors, V3};
            error ->
              {Errors, V2}
          end
      end,
  lists:foldl(F, {[], V}, Keywords).

-spec validate_keyword(Keyword :: atom(), json:value(), validator()) ->
        {json_schema:validation_errors(), validator()}.

validate_keyword(const, KeywordValue, V = #{value := Value}) ->
  check_true(KeywordValue == Value, V);

validate_keyword(enum, KeywordValue, V = #{value := Value}) ->
  check_true(lists:any(fun (Element) -> Element == Value end, KeywordValue), V);

validate_keyword(type, KeywordValue, V = #{value := Value}) ->
  Types = case KeywordValue of
            Ts when is_list(Ts) -> Ts;
            T -> [T]
          end,
  Pred = fun (T) -> value_has_simple_type(Value, T) end,
  check_true(lists:any(Pred, Types), V);

validate_keyword(additional_items, KeywordValue,
                 V = #{value := Value, data := #{items := MaxIndex}}) ->
  Elements = lists:sublist(Value, MaxIndex+1, length(Value)),
  F = fun (Element, {I, Errors}) ->
          V2 = sub_validator(V, Element, integer_to_binary(I), KeywordValue,
                             <<"additionalItems">>),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  case lists:foldl(F, {MaxIndex, []}, Elements) of
    {_, []} ->
      {[], V};
    {_, Errors} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(unevaluated_items, _KeywordValue, V = #{value := _Value}) ->
  %% TODO unevaluatedItems
  %% Do not forget to re-enable tests in json_schema_spec_test.erl
  {[], V};

validate_keyword(items, KeywordValue, V = #{value := Value,
                                            data := Data}) when
    is_list(KeywordValue), is_list(Value) ->
  NbSchemas = length(KeywordValue),
  NbValues = length(Value),
  N = min(NbSchemas, NbValues),
  Schemas = lists:sublist(KeywordValue, N),
  Values = lists:sublist(Value, N),
  F = fun ({Schema, Element}, {I, Errors}) ->
          V2 = sub_validator(V, Element, integer_to_binary(I), Schema,
                             [<<"items">>, integer_to_binary(I)]),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  MaxIndex = max(maps:get(items, Data, 0), N),
  V2 = V#{data => Data#{items => MaxIndex}},
  case lists:foldl(F, {0, []}, lists:zip(Schemas, Values)) of
    {_, []} ->
      {[], V2};
    {_, Errors} ->
      {[validation_error(V2, {invalid_value, Errors})], V2}
  end;
validate_keyword(items, KeywordValue, V = #{value := Value}) when
    is_list(Value) ->
  F = fun (Element, {I, Errors}) ->
          V2 = sub_validator(V, Element, integer_to_binary(I), KeywordValue,
                             <<"items">>),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  case lists:foldl(F, {0, []}, Value) of
    {_, []} ->
      {[], V};
    {_, Errors} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(contains, KeywordValue,
                 V = #{value := Value, schema := Schema, data := Data}) when
    is_list(Value) ->
  %% The test suite indicates that contains is ignored if minContains is
  %% zero. This is not clear at all in the specifications (see Draft 2019-09
  %% Core 9.3.1.4). It means that we have to check here for minContains.
  case maps:find(min_contains, Schema) of
    {ok, 0} ->
      {[], V};
    _ ->
      F = fun (Element, {I, Errors}) ->
              V2 = sub_validator(V, Element, integer_to_binary(I), KeywordValue,
                                 <<"contains">>),
              {Errors2, _} = validate1(V2),
              {I+1, Errors2 ++ Errors}
          end,
      {_, Errors} = lists:foldl(F, {0, []}, Value),
      NbMatches = length(Value) - length(Errors),
      if
        NbMatches > 0 ->
          {[], V#{data => Data#{contains => NbMatches}}};
        true ->
          {[validation_error(V, {invalid_value, Errors})], V}
      end
  end;

validate_keyword(additional_properties, KeywordValue,
                 V = #{value := Value, data := Data}) when
    is_map(Value) ->
  MatchedProperties = maps:get(properties, Data, #{}),
  F = fun (MemberName, MemberValue, Errors) ->
          case maps:is_key(MemberName, MatchedProperties) of
            true ->
              Errors;
            false ->
              V2 = sub_validator(V, MemberValue, MemberName, KeywordValue,
                                 <<"additionalProperties">>),
              {Errors2, _} = validate1(V2),
              Errors2 ++ Errors
          end
      end,
  {maps:fold(F, [], Value), V};

validate_keyword(unevaluated_properties, _KeywordValue,
                 V = #{value := Value}) when
    is_map(Value) ->
  %% TODO unevaluatedProperties
  %% Do not forget to re-enable tests in json_schema_spec_test.erl
  {[], V};

validate_keyword(properties, KeywordValue, V = #{value := Value,
                                                 data := Data}) when
    is_map(Value) ->
  F = fun (MemberName, Schema, {Errors, Properties}) ->
          case maps:find(MemberName, Value) of
            {ok, MemberValue} ->
              Properties2 = Properties#{MemberName => true},
              V2 = sub_validator(V, MemberValue, MemberName, Schema,
                                 <<"properties">>),
              {Errors2, _} = validate1(V2),
              {Errors2 ++ Errors, Properties2};
            error ->
              {Errors, Properties}
          end
      end,
  Properties = maps:get(properties, Data, #{}),
  {Errors, Properties2} = maps:fold(F, {[], Properties}, KeywordValue),
  V2 = V#{data => Data#{properties => Properties2}},
  {Errors, V2};

validate_keyword(pattern_properties, KeywordValue, V = #{value := Value,
                                                         data := Data}) when
    is_map(Value) ->
  F = fun (Pattern, Schema, {Errors, Properties}) ->
          F2 = fun (MemberName, MemberValue, {Errors2, Properties2}) ->
                   case re:run(MemberName, Pattern, [unicode]) of
                     {match, _} ->
                       Properties3 = Properties2#{MemberName => true},
                       V2 = sub_validator(V, MemberValue, MemberName, Schema,
                                          [<<"patternProperties">>, Pattern]),
                       {Errors3, _} = validate1(V2),
                       {Errors3 ++ Errors2, Properties3};
                     nomatch ->
                       {Errors2, Properties2}
                   end
               end,
          {Errors2, Properties2} = maps:fold(F2, {Errors, Properties}, Value),
          {Errors2 ++ Errors, maps:merge(Properties, Properties2)}
      end,
  Properties = maps:get(properties, Data, #{}),
  {Errors, Properties2} = maps:fold(F, {[], Properties}, KeywordValue),
  V2 = V#{data => Data#{properties => maps:merge(Properties, Properties2)}},
  {Errors, V2};

validate_keyword(dependent_schemas, KeywordValue, V = #{value := Value}) ->
  Schemas = maps:filter(fun (MemberName, _) ->
                            maps:is_key(MemberName, Value)
                        end, KeywordValue),
  F = fun (PropertyName, Schema, {Errors, V2}) ->
          V3 = sub_validator(V2, Value, undefined, Schema,
                             [<<"dependentSchemas">>, PropertyName],
                             #{inherit_data => true}),
          {Errors2, _} = validate1(V3),
          {Errors2 ++ Errors, V3}
      end,
  maps:fold(F, {[], V}, Schemas);

validate_keyword(property_names, KeywordValue, V = #{value := Value}) when
    is_map(Value) ->
  F = fun (Name, _, Errors) ->
          V2 = sub_validator(V, Name, Name, KeywordValue, <<"propertyNames">>),
          {Errors2, _} = validate1(V2),
          Errors2 ++ Errors
      end,
  case maps:fold(F, [], Value) of
    [] ->
      {[], V};
    Errors ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(if_, KeywordValue, V = #{value := Value, data := Data}) ->
  V2 = sub_validator(V, Value, undefined, KeywordValue, <<"if">>,
                     #{inherit_data => true}),
  case validate1(V2) of
    {[], _} ->
      {[], V#{data => Data#{if_ => true}}};
    {_Errors, _} ->
      {[], V#{data => Data#{if_ => false}}}
  end;

validate_keyword(then, KeywordValue, V = #{value := Value,
                                           data := #{if_ := true}}) ->
  V2 = sub_validator(V, Value, undefined, KeywordValue, <<"then">>,
                     #{inherit_data => true}),
  case validate1(V2) of
    {[], _} ->
      {[], V};
    {Errors, _} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(else, KeywordValue, V = #{value := Value,
                                           data := #{if_ := false}}) ->
  V2 = sub_validator(V, Value, undefined, KeywordValue, <<"else">>,
                     #{inherit_data => true}),
  case validate1(V2) of
    {[], _} ->
      {[], V};
    {Errors, _} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(all_of, KeywordValue, V = #{value := Value}) ->
  F = fun (Schema, {I, Errors}) ->
          V2 = sub_validator(V, Value, undefined, Schema,
                             [<<"allOf">>, integer_to_binary(I)]),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  {_, Errors} = lists:foldl(F, {0, []}, KeywordValue),
  {Errors, V};

validate_keyword(any_of, KeywordValue, V = #{value := Value}) ->
  F = fun (Schema, {I, Errors}) ->
          V2 = sub_validator(V, Value, undefined, Schema,
                             [<<"anyOf">>, integer_to_binary(I)]),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  case lists:foldl(F, {0, []}, KeywordValue) of
    {N, Errors} when length(Errors) < N ->
      {[], V};
    {_, Errors} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(one_of, KeywordValue, V = #{value := Value}) ->
  F = fun (Schema, {I, Errors}) ->
          V2 = sub_validator(V, Value, undefined, Schema,
                             [<<"oneOf">>, integer_to_binary(I)]),
          {Errors2, _} = validate1(V2),
          {I+1, Errors2 ++ Errors}
      end,
  case lists:foldl(F, {0, []}, KeywordValue) of
    {N, Errors} when length(Errors) == N-1 ->
      {[], V};
    {_, []} ->
      {[validation_error(V, invalid_value)], V};
    {_, Errors} ->
      {[validation_error(V, {invalid_value, Errors})], V}
  end;

validate_keyword(not_, KeywordValue, V = #{value := Value}) ->
  V2 = sub_validator(V, Value, undefined, KeywordValue, <<"not">>),
  case validate1(V2) of
    {[], _} ->
      {[validation_error(V, invalid_value)], V};
    {_Errors, _} ->
      {[], V}
  end;

validate_keyword(multiple_of, KeywordValue, V = #{value := Value}) when
    is_number(Value) ->
  try
    Quotient = Value / KeywordValue,
    check_true((Quotient - trunc(Quotient)) == 0.0, V)
  catch
    error:badarith ->
      {[validation_error(V, invalid_value)], V}
  end;

validate_keyword(maximum, KeywordValue, V = #{value := Value}) when
    is_number(Value) ->
  check_true(Value =< KeywordValue, V);

validate_keyword(exclusive_maximum, KeywordValue, V = #{value := Value}) when
    is_number(Value) ->
  check_true(Value < KeywordValue, V);

validate_keyword(minimum, KeywordValue, V = #{value := Value}) when
    is_number(Value) ->
  check_true(Value >= KeywordValue, V);

validate_keyword(exclusive_minimum, KeywordValue, V = #{value := Value}) when
    is_number(Value) ->
  check_true(Value > KeywordValue, V);

validate_keyword(max_length, KeywordValue, V = #{value := Value}) when
    is_binary(Value) ->
  check_true(string:length(Value) =< KeywordValue, V);

validate_keyword(min_length, KeywordValue, V = #{value := Value}) when
    is_binary(Value) ->
  check_true(string:length(Value) >= KeywordValue, V);

validate_keyword(pattern, KeywordValue, V = #{value := Value}) when
    is_binary(Value) ->
  case re:run(Value, KeywordValue, [unicode]) of
    {match, _} ->
      {[], V};
    nomatch ->
      {[validation_error(V, invalid_value)], V}
  end;

validate_keyword(max_items, KeywordValue, V = #{value := Value}) when
    is_list(Value) ->
  check_true(length(Value) =< KeywordValue, V);

validate_keyword(min_items, KeywordValue, V = #{value := Value}) when
    is_list(Value) ->
  check_true(length(Value) >= KeywordValue, V);

validate_keyword(unique_items, false, V = #{value := Value}) when
    is_list(Value) ->
  %% Draft 2019-09 Validation 6.4.3. If this keyword has boolean value false,
  %% the instance validates successfully.
  {[], V};
validate_keyword(unique_items, true, V = #{value := Value}) when
    is_list(Value) ->
  check_true(json_schema_utils:unique_values(Value), V);

validate_keyword(max_contains, KeywordValue,
                 V = #{value := Value, data := #{contains := NbMatches}}) when
    is_list(Value) ->
  check_true(NbMatches =< KeywordValue, V);

validate_keyword(min_contains, KeywordValue,
                 V = #{value := Value, data := #{contains := NbMatches}}) when
    is_list(Value) ->
  check_true(NbMatches >= KeywordValue, V);

validate_keyword(max_properties, KeywordValue, V = #{value := Value}) when
    is_map(Value) ->
  check_true(map_size(Value) =< KeywordValue, V);

validate_keyword(min_properties, KeywordValue, V = #{value := Value}) when
    is_map(Value) ->
  check_true(map_size(Value) >= KeywordValue, V);

validate_keyword(required, KeywordValue, V = #{value := Value}) when
    is_map(Value) ->
  check_contains(Value, KeywordValue, V);

validate_keyword(dependent_required, KeywordValue, V = #{value := Value}) when
    is_map(Value) ->
  F = fun (Name, Names, Errors) ->
          case maps:is_key(Name, Value) of
            true ->
              {Errors2, _} = check_contains(Value, Names, V),
              Errors2 ++ Errors;
            false ->
              Errors
          end
      end,
  {maps:fold(F, [], KeywordValue), V};

validate_keyword(format, KeywordValue, V = #{value := Value}) when
    is_binary(Value) ->
  case json_schema_formats:validate(KeywordValue, Value) of
    ok ->
      {[], V};
    {error, Reason} ->
      {[validation_error(V, {invalid_format, Reason})], V}
  end;

validate_keyword(content_media_type, KeywordValue, V = #{value := Value,
                                                         data := Data}) when
    is_binary(Value) ->
  %% XXX We should probably use a real MIME media type parser
  Content = maps:get(content_encoding, Data, Value),
  case string:lowercase(KeywordValue) of
    <<"application/json">> ->
      case json:parse(Content) of
        {ok, _} ->
          {[], V};
        {error, Reason} ->
          {[validation_error(V, {invalid_content, Reason})], V}
      end;
    _ ->
      {[], V}
  end;

validate_keyword(content_encoding, KeywordValue, V = #{value := Value,
                                                       data := Data}) when
    is_binary(Value) ->
  case KeywordValue of
    <<"base64">> ->
      try
        DecodedValue = base64:decode(Value),
        {[], V#{data => Data#{content_encoding => DecodedValue}}}
      catch
        error:_ ->
          {[validation_error(V, {invalid_content_encoding, invalid_data})], V}
      end;
    _ ->
      {[], V}
  end;

validate_keyword(content_schema, _KeywordValue, V = #{value := Value}) when
    is_binary(Value) ->
  %% TODO
  {[], V};

validate_keyword(_Keyword, _KeywordValue, V) ->
  {[], V}.

-spec validation_error(validator(), json_schema:validation_error_reason()) ->
        json_schema:validation_error().
validation_error(#{value_path := ValuePath,
                   schema_path := SchemaPath},
                 Reason) ->
  #{reason => Reason,
    value_path => ValuePath,
    schema_path => SchemaPath}.

-spec value_has_simple_type(json:value(), json_schema:simple_type()) ->
        boolean().
value_has_simple_type(V, array) when is_list(V) ->
  true;
value_has_simple_type(V, boolean) when is_boolean(V) ->
  true;
value_has_simple_type(V, integer) when is_integer(V) ->
  true;
value_has_simple_type(V, integer) when is_float(V) ->
  %% Draft 2019-09 Validation 6.1.1. [...] or "integer" which matches any
  %% number with a zero fractional part
  (V - trunc(V)) =:= 0.0;
value_has_simple_type(V, null) when V =:= null ->
  true;
value_has_simple_type(V, number) when is_number(V) ->
  true;
value_has_simple_type(V, object) when is_map(V) ->
  true;
value_has_simple_type(V, string) when is_binary(V) ->
  true;
value_has_simple_type(_, _) ->
  false.

-spec check_true(boolean(), validator()) ->
        {json_schema:validation_errors(), validator()}.
check_true(true, V) ->
  {[], V};
check_true(false, V) ->
  {[validation_error(V, invalid_value)], V}.

-spec check_contains(#{binary() => json:value()}, [binary()], validator()) ->
        {json_schema:validation_errors(), validator()}.
check_contains(_, [], V) ->
  {[], V};
check_contains(Value, [H | T], V) ->
  case maps:is_key(H, Value) of
    true ->
      check_contains(Value, T, V);
    false ->
      {[validation_error(V, {missing_member, H})], V}
  end.

-spec sub_validator(validator(), json:value(), ChildName,
                    json_schema:schema(), Keyword) -> validator() when
    ChildName :: json_pointer:reference_token() | undefined,
    Keyword :: json_pointer:reference_token()
             | [json_pointer:reference_token()].
sub_validator(V, Value, ChildName, Schema, Keyword) ->
  sub_validator(V, Value, ChildName, Schema, Keyword, #{}).

-spec sub_validator(validator(), json:value(), ChildName,
                    json_schema:schema(), Keyword, Options) -> validator() when
    ChildName :: json_pointer:reference_token() | undefined,
    Keyword :: json_pointer:reference_token()
             | [json_pointer:reference_token()],
    Options :: #{inherit_data => boolean()}.
sub_validator(V = #{value_path := ValuePath,
                    schema_path := SchemaPath,
                    data := Data},
              Value, ChildName, Schema, Keyword, Options) ->
  ValuePath2 = case ChildName of
                 undefined ->
                   ValuePath;
                 _ ->
                   json_pointer:child(ValuePath, ChildName)
               end,
  Data2 = case maps:get(inherit_data, Options, false) of
            true -> Data;
            false -> #{}
          end,
  V#{value => Value,
     value_path => ValuePath2,
     schema => Schema,
     schema_path => json_pointer:child(SchemaPath, Keyword),
     data => Data2}.
