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
-module(mqtree_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertTree(L),
	case L of
	    [] ->
		?assertEqual([], mqtree:dump(T)),
		?assertEqual([], mqtree:to_list(T));
	    _ ->
		?assertEqual(L, lists:sort(mqtree:to_list(T)))
	end).

-define(assertInsert(E),
	?assertEqual(ok, mqtree:insert(T, E))).

-define(assertDelete(E),
	?assertEqual(ok, mqtree:delete(T, E))).

%%%===================================================================
%%% Tests
%%%===================================================================
new_test() ->
    T = mqtree:new(),
    ?assertTree([]).

insert_test() ->
    T = mqtree:new(),
    Path = <<"/a/b/c">>,
    ?assertInsert(Path),
    ?assertTree([{Path, 1}]).

is_empty_test() ->
    T = mqtree:new(),
    ?assert(mqtree:is_empty(T)),
    ?assertInsert(<<"/">>),
    ?assert(not mqtree:is_empty(T)).

insert_then_delete_test() ->
    T = mqtree:new(),
    Path = <<"a/b">>,
    ?assertInsert(Path),
    ?assertDelete(Path),
    ?assertTree([]).

insert_empty_then_delete_empty_test() ->
    T = mqtree:new(),
    ?assertInsert(<<>>),
    ?assertTree([]),
    ?assertDelete(<<>>),
    ?assertTree([]).

insert_then_delete_empty_test() ->
    T = mqtree:new(),
    Path = <<"/a/b">>,
    ?assertInsert(Path),
    ?assertTree([{Path, 1}]),
    ?assertDelete(<<>>),
    ?assertTree([{Path, 1}]).

insert_then_delete_shuffle_test() ->
    T = mqtree:new(),
    Check = lists:sort(rand_paths()),
    lists:foldl(
      fun(insert, Refc) ->
	      lists:foreach(
		fun(Path) -> ?assertInsert(Path) end,
		rand_paths()),
	      Refc1 = Refc+1,
	      ?assertTree([{P, Refc1} || P <- Check]),
	      Refc1;
	 (delete, Refc) ->
	      lists:foreach(
		fun(Path) -> ?assertDelete(Path) end,
		rand_paths()),
	      Refc1 = Refc-1,
	      case Refc1 of
		  0 ->
		      ?assertTree([]);
		  _ ->
		      ?assertTree([{P, Refc1} || P <- Check])
	      end,
	      Refc1
      end, 0, rand_funs()).

refc_test() ->
    T = mqtree:new(),
    lists:foreach(
      fun(Refc) ->
	      lists:foreach(
		fun(P) ->
			?assertEqual(Refc, mqtree:refc(T, P)),
			?assertInsert(P)
		end, rand_paths())
      end, lists:seq(0, 5)),
    lists:foreach(
      fun(Refc) ->
	      lists:foreach(
		fun(P) ->
			?assertDelete(P),
			?assertEqual(Refc, mqtree:refc(T, P))
		end, rand_paths())
      end, lists:seq(5, 0, -1)).

clear_test() ->
    T = mqtree:new(),
    lists:foreach(
      fun(_) ->
	      lists:foreach(fun(P) -> ?assertInsert(P) end, rand_paths()),
	      ?assertEqual(ok, mqtree:clear(T)),
	      ?assertTree([])
      end, lists:seq(1, 10)).

clear_empty_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:clear(T)),
    ?assertTree([]).

size_test() ->
    T = mqtree:new(),
    ?assertEqual(0, mqtree:size(T)),
    Paths = rand_paths(),
    lists:foreach(
      fun(_) ->
	      lists:foreach(fun(P) -> ?assertInsert(P) end, rand_paths()),
	      ?assert(mqtree:size(T) == length(Paths))
      end, [1,2,3]),
    ?assertEqual(ok, mqtree:clear(T)),
    ?assertEqual(0, mqtree:size(T)).

delete_non_existent_test() ->
    T = mqtree:new(),
    lists:foreach(
      fun(_) ->
	      lists:foreach(fun(P) -> ?assertDelete(P) end, rand_paths()),
	      ?assertTree([])
      end, lists:seq(1, 10)).

insert_then_delete_non_existent_test() ->
    T = mqtree:new(),
    Inserts = rand_paths("@$%&*"),
    Check = [{P, 1} || P <- lists:sort(Inserts)],
    lists:foreach(fun(P) -> ?assertInsert(P) end, Inserts),
    lists:foreach(
      fun(_) ->
	      lists:foreach(fun(P) -> ?assertDelete(P) end, rand_paths()),
	      ?assertTree(Check)
      end, lists:seq(1, 10)).

match_all_test() ->
    T = mqtree:new(),
    lists:foreach(
      fun(_) ->
	      ?assertInsert("#"),
	      lists:foreach(
		fun(P) ->
			?assertEqual([<<"#">>], mqtree:match(T, P))
		end, rand_paths())
      end, lists:seq(1, 10)).

match_none_test() ->
    T = mqtree:new(),
    lists:foreach(
      fun(P) ->
	      ?assertEqual([], mqtree:match(T, P))
      end, rand_paths()).

match_exact_test() ->
    T = mqtree:new(),
    lists:foreach(fun(P) -> ?assertInsert(P) end, rand_paths()),
    lists:foreach(
      fun(P) ->
	      ?assertEqual([P], mqtree:match(T, P))
      end, rand_paths()).

match_tail_test() ->
    T = mqtree:new(),
    Filter = <<"a/b/#">>,
    ?assertInsert(Filter),
    ?assertEqual([], mqtree:match(T, "a/bc")),
    ?assertEqual([Filter], mqtree:match(T, "a/b")),
    ?assertEqual([Filter], mqtree:match(T, "a/b/")),
    ?assertEqual([Filter], mqtree:match(T, "a/b/c")),
    ?assertEqual([Filter], mqtree:match(T, "a/b/c/d")).

match_plus_test() ->
    T = mqtree:new(),
    Filter = lists:sort([<<A, $/, B>> || A<-"+a", B<-"+b"]),
    lists:foreach(fun(P) -> ?assertInsert(P) end, Filter),
    ?assertEqual([<<"+/+">>], mqtree:match(T, "/")),
    ?assertEqual([<<"+/+">>], mqtree:match(T, "x/")),
    ?assertEqual([<<"+/+">>], mqtree:match(T, "/y")),
    ?assertEqual([<<"+/+">>], mqtree:match(T, "x/y")),
    ?assertEqual([<<"+/+">>, <<"a/+">>], mqtree:match(T, "a/")),
    ?assertEqual([<<"+/+">>, <<"a/+">>], mqtree:match(T, "a/y")),
    ?assertEqual([<<"+/+">>, <<"+/b">>], mqtree:match(T, "/b")),
    ?assertEqual([<<"+/+">>, <<"+/b">>], mqtree:match(T, "x/b")),
    ?assertEqual(Filter, lists:sort(mqtree:match(T, "a/b"))).

whereis_non_existent_test() ->
    ?assertEqual(undefined, mqtree:whereis(test_tree)).

unregister_non_existent_test() ->
    ?assertError(badarg, mqtree:unregister(test_tree)).

register_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree, T)),
    ?assertEqual(T, mqtree:whereis(test_tree)),
    ?assertEqual(ok, mqtree:unregister(test_tree)).

double_register_same_tree_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree, T)),
    ?assertError(badarg, mqtree:register(test_tree, T)),
    ?assertEqual(ok, mqtree:unregister(test_tree)).

double_register_another_tree_test() ->
    T1 = mqtree:new(),
    T2 = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree, T1)),
    ?assertError(badarg, mqtree:register(test_tree, T2)),
    ?assertEqual(ok, mqtree:unregister(test_tree)).

unregister_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree, T)),
    ?assertEqual(ok, mqtree:unregister(test_tree)),
    ?assertEqual(undefined, mqtree:whereis(test_tree)).

double_unregister_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree, T)),
    ?assertEqual(ok, mqtree:unregister(test_tree)),
    ?assertError(badarg, mqtree:unregister(test_tree)).

rename_test() ->
    T = mqtree:new(),
    ?assertEqual(ok, mqtree:register(test_tree_1, T)),
    ?assertEqual(ok, mqtree:register(test_tree_2, T)),
    ?assertEqual(undefined, mqtree:whereis(test_tree_1)),
    ?assertError(badarg, mqtree:unregister(test_tree_1)),
    ?assertEqual(T, mqtree:whereis(test_tree_2)),
    ?assertEqual(ok, mqtree:unregister(test_tree_2)).

register_undefined_test() ->
    T = mqtree:new(),
    ?assertError(badarg, mqtree:register(undefined, T)).

registered_test() ->
    Names = [list_to_atom("test_tree_" ++ integer_to_list(I))
	     || I <- lists:seq(1, 9)],
    lists:foldl(
      fun(Name, Acc) ->
	      ?assertEqual(Acc, lists:sort(mqtree:registered())),
	      T = mqtree:new(),
	      ?assertEqual(ok, mqtree:register(Name, T)),
	      [Name|Acc]
      end, [], lists:reverse(Names)),
    lists:foldl(
      fun(_, [Name|Acc]) ->
	      ?assertEqual(ok, mqtree:unregister(Name)),
	      ?assertEqual(Acc, lists:sort(mqtree:registered())),
	      Acc
      end, Names, Names).

%%%===================================================================
%%% Internal functions
%%%===================================================================
rand_paths() ->
    rand_paths("/abcd").

rand_paths(Set) ->
    L1 = [{p1_rand:uniform(), <<A>>} || A<-Set],
    L2 = [{p1_rand:uniform(), <<A,B>>} || A<-Set, B<-Set],
    L3 = [{p1_rand:uniform(), <<A,B,C>>} || A<-Set, B<-Set, C<-Set],
    L4 = [{p1_rand:uniform(), <<A,B,C,D>>} || A<-Set, B<-Set, C<-Set, D<-Set],
    L5 = [{p1_rand:uniform(), <<A,B,C,D,E>>} || A<-Set, B<-Set, C<-Set, D<-Set, E<-Set],
    [Path || {_, Path} <- lists:keysort(1, L1++L2++L3++L4++L5)].

rand_funs() ->
    lists:flatmap(
      fun(_) ->
	      I = p1_rand:uniform(5),
	      Inserts = lists:duplicate(I, insert),
	      Deletes = lists:duplicate(I, delete),
	      Inserts ++ Deletes
      end, [1,2,3,4,5]).
