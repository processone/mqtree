%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2024 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(mqtree).
-on_load(load_nif/0).

%% API
-export([new/0, insert/2, delete/2, match/2, refc/2,
         clear/1, size/1, is_empty/1]).
-export([register/2, unregister/1, whereis/1, registered/0]).
%% For debugging
-export([dump/1, to_list/1]).

-type path() :: iodata().
-opaque tree() :: reference().
-export_type([tree/0, path/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new() -> tree().
new() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec insert(tree(), path()) -> ok.
insert(_Tree, _Path) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec delete(tree(), path()) -> ok.
delete(_Tree, _Path) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec match(tree(), path()) -> [binary()].
match(_Tree, _Path) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec refc(tree(), path()) -> non_neg_integer().
refc(_Tree, _Path) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec clear(tree()) -> ok.
clear(_Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec size(tree()) -> non_neg_integer().
size(_Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec is_empty(tree()) -> boolean().
is_empty(_Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec register(atom(), tree()) -> ok.
register(_Name, _Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec unregister(atom()) -> ok.
unregister(_Name) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec whereis(atom()) -> tree() | undefined.
whereis(_Name) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec registered() -> [atom()].
registered() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% For testing/debugging
%%%===================================================================
-type tree_node() :: {string(), string() | none,
                      non_neg_integer(), [tree_node()]}.

-spec dump(tree()) -> [tree_node()].
dump(_Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec to_list(tree()) -> [{binary(), non_neg_integer()}].
to_list(_Tree) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_nif() ->
    case os:getenv("COVERALLS") of
        "true" -> ok;
        _ -> load_nif2()
    end.
load_nif2() ->
    Path = p1_nif_utils:get_so_path(?MODULE, [?MODULE], atom_to_list(?MODULE)),
    case erlang:load_nif(Path, 0) of
        ok -> ok;
        {error, {upgrade, _}} -> ok;
        {error, {Reason, Text}} ->
            error_logger:error_msg("Failed to load NIF ~s: ~s (~p)",
                                   [Path, Text, Reason]),
            erlang:nif_error(Reason)
    end.
