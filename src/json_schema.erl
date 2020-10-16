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

-module(json_schema).

-export([parse/1, validate/2, validate/3]).

-export_type([simple_type/0,
              schema/0, options/0,
              parsing_error/0, parsing_error_reason/0,
              validation_error/0, validation_errors/0,
              validation_error_reason/0]).

-type re_mp() :: {re_pattern, _, _, _, _}. % mp/0 is not exported by re
-type re() :: binary() | re_mp().

-type simple_type() :: array | boolean | integer | null | number | object
                     | string.

-type schema() :: boolean()
                | #{id => uri:uri(),
                    schema => uri:uri(),
                    anchor => binary(),
                    ref => uri:uri(),
                    recursive_ref => uri:uri(),
                    recursive_anchor => boolean(),
                    vocabulary => #{uri:uri() := boolean()},
                    comment => binary(),
                    defs => #{binary() := schema()},
                    %% Backward compatibility
                    definitions => #{binary() := schema()},
                    dependencies => #{binary() := (schema() | [binary()])},
                    %% Metadata
                    title => binary(),
                    description => binary(),
                    default => json:value(),
                    deprecated => boolean(),
                    read_only => boolean(),
                    write_only => boolean(),
                    examples => [json:value()],
                    %% Applicators
                    additional_items => schema(),
                    unevaluated_items => schema(),
                    items => schema() | [schema()],
                    contains => schema(),
                    additional_properties => schema(),
                    unevaluated_properties => schema(),
                    properties => #{binary() := schema()},
                    pattern_properties => #{re() := schema()},
                    dependent_schemas => #{binary() := schema()},
                    property_names => schema(),
                    if_ => schema(),
                    then => schema(),
                    else => schema(),
                    all_of => [schema()],
                    any_of => [schema()],
                    one_of => [schema()],
                    not_ => schema(),
                    %% Validation
                    multiple_of => number(),
                    maximum => number(),
                    exclusive_maximum => number(),
                    minimum => number(),
                    exclusive_minimum => number(),
                    max_length => non_neg_integer(),
                    min_length => non_neg_integer(),
                    pattern => re(),
                    max_items => non_neg_integer(),
                    min_items => non_neg_integer(),
                    unique_items => boolean(),
                    max_contains => non_neg_integer(),
                    min_contains => non_neg_integer(),
                    max_properties => non_neg_integer(),
                    min_properties => non_neg_integer(),
                    required => [binary()],
                    dependent_required => #{binary() := [binary()]},
                    const => json:value(),
                    enum => [json:value()],
                    type => simple_type() | [simple_type()],
                    %% Format
                    format => binary(),
                    %% Content
                    content_media_type => binary(),
                    content_encoding => binary(),
                    content_schema => schema()}.

-type options() :: #{base_uri => binary() | uri:uri()}.

-type parsing_error() :: #{reason := parsing_error_reason(),
                           secondary_reason => term(),
                           keyword => binary(),
                           value => json:value(),
                           schema_error => parsing_error()}.
-type parsing_error_reason() :: invalid_schema
                              | unknown_keyword
                              | invalid_keyword
                              | invalid_keyword_schema
                              | invalid_keyword_format
                              | invalid_simple_type
                              | invalid_vocabulary_uri.

-type validation_error() :: #{reason := validation_error_reason(),
                              value_path => json_pointer:pointer(),
                              schema_path => json_pointer:pointer(),
                              keyword => atom()}.
-type validation_errors() :: [validation_error()].
-type validation_error_reason() :: invalid_value
                                 | {invalid_value, validation_errors()}
                                 | {missing_member, binary()}
                                 | {invalid_format, term()}
                                 | {invalid_content_encoding, term()}
                                 | {invalid_content, term()}.

-spec parse(json:value()) -> {ok, schema()} | {error, parsing_error()}.
parse(Value) ->
  json_schema_parser:parse(Value).

-spec validate(schema(), json:value()) ->
        ok | {error, validation_errors()}.
validate(Schema, Value) ->
  validate(Schema, Value, #{}).

-spec validate(schema(), json:value(), options()) ->
        ok | {error, validation_errors()}.
validate(Schema, Value, Options) ->
  json_schema_validator:validate(Schema, Value, Options).
