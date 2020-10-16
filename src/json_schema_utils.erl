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

-module(json_schema_utils).

-export([unique_values/1]).

-spec unique_values([json:value()]) -> boolean().
unique_values(Values) ->
  unique_values(Values, #{}).

-spec unique_values([json:value()], #{}) -> boolean().
unique_values([], _) ->
  true;
unique_values([H | T], Table) ->
  Key = unicity_key(H),
  case maps:is_key(Key, Table) of
    true ->
      false;
    false ->
      unique_values(T, Table#{Key => true})
  end.

-spec unicity_key(json:value()) -> json:value().
unicity_key(V) when is_list(V) ->
  lists:map(fun unicity_key/1, V);
unicity_key(V) when is_map(V) ->
  maps:fold(fun (K, V2, Acc) ->
                Acc#{K => unicity_key(V2)}
            end, #{}, V);
unicity_key(V) when is_float(V) ->
  TV = trunc(V),
  case V - TV of
    0.0 -> TV;
    _ -> V
  end;
unicity_key(V) ->
  V.
