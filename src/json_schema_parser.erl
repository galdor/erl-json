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

-module(json_schema_parser).

-export([parse/1]).

-spec parse(json:value()) ->
        {ok, json_schema:schema()} | {error, json_schema:parsing_error()}.
parse(Value) when is_boolean(Value) ->
  {ok, Value};
parse(Value) when is_map(Value) ->
  try
    F = fun (K, V, Ms) ->
            try
              Pair = parse_keyword(K, V),
              [Pair | Ms]
            catch
              throw:unknown_keyword ->
                %% Draft 2019-09 Core 4.3.1. Unknown keywords SHOULD be
                %% ignored.
                Ms;
              throw:Error ->
                throw(Error#{keyword => K})
            end
        end,
    Members = maps:fold(F, [], Value),
    {ok, maps:from_list(Members)}
  catch
    throw:Error ->
      {error, Error}
  end;
parse(Value) ->
  {error, {invalid_schema, Value}}.

-spec parse_keyword(binary(), json:value()) -> {atom(), term()}.

parse_keyword(<<"$id">>, Value) when is_binary(Value) ->
  case uri:parse(Value) of
    {ok, #{fragment := Fragment}} when byte_size(Fragment) > 0 ->
      throw(#{reason => invalid_keyword,
              secondary_reason => non_empty_fragment,
              value => Value});
    {ok, URI} ->
      {id, URI};
    {error, Reason} ->
      throw(#{reason => invalid_keyword,
              secondary_reason => Reason,
              value => Value})
  end;
parse_keyword(<<"$id">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$schema">>, Value) when is_binary(Value) ->
  parse_uri(schema, Value);
parse_keyword(<<"$schema">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$anchor">>, Value) ->
  case re:run(Value, <<"^[A-Za-z][-A-Za-z0-9.:_]*$">>) of
    nomatch ->
      throw(#{reason => invalid_keyword});
    _ ->
      {id, Value}
  end;

parse_keyword(<<"$ref">>, Value) when is_binary(Value) ->
  parse_uri_reference(ref, Value);
parse_keyword(<<"$ref">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$recursiveRef">>, Value) when is_binary(Value) ->
  parse_uri_reference(recursive_ref, Value);
parse_keyword(<<"$recursiveRef">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$recursiveAnchor">>, Value) when is_boolean(Value) ->
  {recursive_anchor, Value};
parse_keyword(<<"$recursiveAnchor">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$vocabulary">>, Value) when is_map(Value) ->
  F = fun
        (K, V, Acc) when is_boolean(V) ->
          case uri:parse(K) of
            {ok, URI = #{scheme := _}} ->
              Acc#{URI => V};
            {ok, _} ->
              throw(#{reason => invalid_vocabulary_uri,
                      secondary_reason => invalid_uri,
                      value => K});
            {error, Reason} ->
              throw(#{reason => invalid_vocabulary_uri,
                      secondary_reason => Reason,
                      value => K})
          end;
        (_, _, _) ->
          throw(#{reason => invalid_keyword})
      end,
  Vocabulary = maps:fold(F, #{}, Value),
  {vocabulary, Vocabulary};
parse_keyword(<<"$vocabulary">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$comment">>, Value) when is_binary(Value) ->
  {comment, Value};
parse_keyword(<<"$comment">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"$defs">>, Value) when is_map(Value) ->
  Schemas = maps:fold(fun (K, V, Acc) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Acc#{K => Schema};
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, #{}, Value),
  {defs, Schemas};
parse_keyword(<<"$defs">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"title">>, Value) when is_binary(Value) ->
  {title, Value};
parse_keyword(<<"title">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"description">>, Value) when is_binary(Value) ->
  {description, Value};
parse_keyword(<<"description">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"default">>, Value) ->
  {default, Value};

parse_keyword(<<"deprecated">>, Value) when is_boolean(Value) ->
  {deprecated, Value};
parse_keyword(<<"deprecated">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"readOnly">>, Value) when is_boolean(Value) ->
  {read_only, Value};
parse_keyword(<<"readOnly">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"writeOnly">>, Value) when is_boolean(Value) ->
  {write_only, Value};
parse_keyword(<<"writeOnly">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"examples">>, Value) when is_list(Value) ->
  {examples, Value};
parse_keyword(<<"examples">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"additionalItems">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {additional_items, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"unevaluatedItems">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {unevaluated_items, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"items">>, Value) when is_list(Value) ->
  Schemas = lists:map(fun (V) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Schema;
                            {error, Reason} ->
                              throw({invalid_keyword_schema, V, Reason})
                          end
                      end, Value),
  {items, Schemas};
parse_keyword(<<"items">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {items, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"contains">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {contains, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"additionalProperties">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {additional_properties, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"unevaluatedProperties">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {unevaluated_properties, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"properties">>, Value) when is_map(Value) ->
  Schemas = maps:fold(fun (K, V, Acc) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Acc#{K => Schema};
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, #{}, Value),
  {properties, Schemas};
parse_keyword(<<"properties">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"patternProperties">>, Value) when is_map(Value) ->
  Schemas = maps:fold(fun (K, V, Acc) ->
                          case json_schema_formats:validate_regex(K) of
                            ok ->
                              case parse(V) of
                                {ok, Schema} ->
                                  Acc#{K => Schema};
                                {error, Error} ->
                                  throw(#{reason => invalid_keyword_schema,
                                          value => V,
                                          schema_error => Error})
                              end;
                            {error, Reason} ->
                              throw(#{reason => invalid_keyword_format,
                                      secondary_reason => Reason,
                                      value => V})
                          end
                      end, #{}, Value),
  {pattern_properties, Schemas};
parse_keyword(<<"patternProperties">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"dependentSchemas">>, Value) when is_map(Value) ->
  Schemas = maps:fold(fun (K, V, Acc) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Acc#{K => Schema};
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, #{}, Value),
  {dependent_schemas, Schemas};
parse_keyword(<<"dependentSchemas">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"propertyNames">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {property_names, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"if">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {if_, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"then">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {then, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"else">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {else, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"allOf">>, Value) when is_list(Value) ->
  Schemas = lists:map(fun (V) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Schema;
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, Value),
  {all_of, Schemas};
parse_keyword(<<"allOf">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"anyOf">>, Value) when is_list(Value) ->
  Schemas = lists:map(fun (V) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Schema;
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, Value),
  {any_of, Schemas};
parse_keyword(<<"anyOf">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"oneOf">>, Value) when is_list(Value) ->
  Schemas = lists:map(fun (V) ->
                          case parse(V) of
                            {ok, Schema} ->
                              Schema;
                            {error, Error} ->
                              throw(#{reason => invalid_keyword_schema,
                                      value => V,
                                      schema_error => Error})
                          end
                      end, Value),
  {one_of, Schemas};
parse_keyword(<<"oneOf">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"not">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {not_, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(<<"multipleOf">>, Value) when is_number(Value), Value > 0 ->
  {multiple_of, Value};
parse_keyword(<<"multipleOf">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"maximum">>, Value) when is_number(Value) ->
  {maximum, Value};
parse_keyword(<<"maximum">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"exclusiveMaximum">>, Value) when is_number(Value) ->
  {exclusive_maximum, Value};
parse_keyword(<<"exclusiveMaximum">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"minimum">>, Value) when is_number(Value) ->
  {minimum, Value};
parse_keyword(<<"minimum">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"exclusiveMinimum">>, Value) when is_number(Value) ->
  {exclusive_minimum, Value};
parse_keyword(<<"exclusiveMinimum">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"maxLength">>, Value) when is_number(Value), Value >= 0 ->
  {max_length, Value};
parse_keyword(<<"maxLength">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"minLength">>, Value) when is_number(Value), Value >= 0 ->
  {min_length, Value};
parse_keyword(<<"minLength">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"pattern">>, Value) when is_binary(Value) ->
  case json_schema_formats:validate_regex(Value) of
    ok ->
      {pattern, Value};
    {error, Reason} ->
      throw(#{reason => invalid_keyword_format,
              secondary_reason => Reason,
              value => Value})
  end;
parse_keyword(<<"pattern">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"maxItems">>, Value) when is_number(Value), Value >= 0 ->
  {max_items, Value};
parse_keyword(<<"maxItems">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"minItems">>, Value) when is_number(Value), Value >= 0 ->
  {min_items, Value};
parse_keyword(<<"minItems">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"uniqueItems">>, Value) when is_boolean(Value) ->
  {unique_items, Value};
parse_keyword(<<"uniqueItems">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"maxContains">>, Value) when is_number(Value), Value >= 0 ->
  {max_contains, Value};
parse_keyword(<<"maxContains">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"minContains">>, Value) when is_number(Value), Value >= 0 ->
  {min_contains, Value};
parse_keyword(<<"minContains">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"maxProperties">>, Value) when is_number(Value), Value >= 0 ->
  {max_properties, Value};
parse_keyword(<<"maxProperties">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"minProperties">>, Value) when is_number(Value), Value >= 0 ->
  {min_properties, Value};
parse_keyword(<<"minProperties">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"required">>, Value) when is_list(Value) ->
  %% TODO Property names must be unique
  Properties = lists:map(fun (V) when is_binary(V) -> V;
                             (_) -> throw(#{reason => invalid_keyword})
                         end, Value),
  {required, Properties};
parse_keyword(<<"required">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"dependentRequired">>, Value) when is_map(Value) ->
  F = fun
        (K, V, Acc) when is_list(V) ->
          %% TODO Property names must be unique
          Ps = lists:map(fun (V2) when is_binary(V2) -> V2;
                             (_) -> throw(#{reason => invalid_keyword})
                         end, V),
          Acc#{K => Ps};
        (_K, _V, _Acc) ->
          throw(#{reason => invalid_keyword})
      end,
  Properties = maps:fold(F, #{}, Value),
  {dependent_required, Properties};
parse_keyword(<<"dependentRequired">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"const">>, Value) ->
  {const, Value};

parse_keyword(<<"enum">>, Value) when is_list(Value) ->
  {enum, Value};
parse_keyword(<<"enum">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"type">>, Value) when is_binary(Value) ->
  case parse_simple_type(Value) of
    {ok, Type} ->
      {type, Type};
    error ->
      throw(#{reason => invalid_simple_type, value => Value})
  end;
parse_keyword(<<"type">>, Value) when is_list(Value) ->
  Types = lists:map(fun (V) ->
                        case parse_simple_type(V) of
                          {ok, Type} ->
                            Type;
                          error ->
                            throw(#{reason => invalid_simple_type, value => V})
                        end
                    end, Value),
  {type, Types};
parse_keyword(<<"type">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"format">>, Value) when is_binary(Value) ->
  {format, Value};
parse_keyword(<<"format">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"contentMediaType">>, Value) when is_binary(Value) ->
  {content_media_type, Value};
parse_keyword(<<"contentMediaType">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"contentEncoding">>, Value) when is_binary(Value) ->
  {content_encoding, Value};
parse_keyword(<<"contentEncoding">>, _) ->
  throw(#{reason => invalid_keyword});

parse_keyword(<<"contentSchema">>, Value) ->
  case parse(Value) of
    {ok, Schema} ->
      {content_schema, Schema};
    {error, Error} ->
      throw(#{reason => invalid_keyword_schema,
              value => Value,
              schema_error => Error})
  end;

parse_keyword(_, _) ->
  throw(unknown_keyword).

-spec parse_uri(atom(), binary()) -> {atom(), uri:uri()}.
parse_uri(Keyword, Value) ->
  case uri:parse(Value) of
    {ok, URI = #{scheme := _}} ->
      {Keyword, URI};
    {ok, _} ->
      throw(#{reason => invalid_keyword,
              secondary_reason => invalid_uri,
              value => Value});
    {error, Reason} ->
      throw(#{reason => invalid_keyword,
              secondary_reason => Reason,
              value => Value})
  end.

-spec parse_uri_reference(atom(), binary()) -> {atom(), uri:uri()}.
parse_uri_reference(Keyword, Value) ->
  case uri:parse(Value) of
    {ok, URI} ->
      {Keyword, URI};
    {error, Reason} ->
      throw(#{reason => invalid_keyword,
              secondary_reason => Reason,
              value => Value})
  end.

-spec parse_simple_type(binary()) -> {ok, json_schema:simple_type()} | error.
parse_simple_type(<<"array">>) ->
  {ok, array};
parse_simple_type(<<"boolean">>) ->
  {ok, boolean};
parse_simple_type(<<"integer">>) ->
  {ok, integer};
parse_simple_type(<<"null">>) ->
  {ok, null};
parse_simple_type(<<"number">>) ->
  {ok, number};
parse_simple_type(<<"object">>) ->
  {ok, object};
parse_simple_type(<<"string">>) ->
  {ok, string};
parse_simple_type(_) ->
  error.
