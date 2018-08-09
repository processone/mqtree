%%%-------------------------------------------------------------------
%%%
%%% ejabberd Business Edition, Copyright (C) 2002-2018   ProcessOne
%%%
%%% The ejabberd software is the exclusive property of the licensor
%%% ProcessOne. It is protected by the law on copyright and
%%% international conventions. As a result, the dealer
%%% recognizes that will make every effort to ensure the confidentiality
%%% on the software. It is recalled that a violation of the rights of
%%% authors of the software is an infringement and that any
%%% counterfeit is punishable in France by Article L339-2 of the Code of
%%% Intellectual property and punishable by three years imprisonment and
%%% 300000 euros.
%%%
%%% Any infringement liable to be so qualified and
%%% would be caused by third parties and whose dealer has knowledge
%%% should be terminated by the licensor that it will make its case
%%% personal conduct of the proceedings. Any allegation of infringement
%%% formed against the dealer because of the use of the Software will
%%% be brought to the knowledge of the licensor which will assist
%%% in defense of the dealer in the manner and form that
%%% see fit and fix alone.
%%%
%%%----------------------------------------------------------------------
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
    Path = p1_nif_utils:get_so_path(?MODULE, [?MODULE], atom_to_list(?MODULE)),
    case erlang:load_nif(Path, 0) of
        ok -> ok;
        {error, {upgrade, _}} -> ok;
        {error, {Reason, Text}} ->
            error_logger:error_msg("Failed to load NIF ~s: ~s (~p)",
                                   [Path, Text, Reason]),
            erlang:nif_error(Reason)
    end.
