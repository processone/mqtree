mqtree: Index tree for MQTT topic filters
====================================================

mqtree is an Erlang NIF implementation of N-ary tree to keep MQTT
topic filters for efficient matching.

# System requirements

To compile mqtree you need:

 - GNU Make.
 - GCC.
 - Erlang/OTP 17.5 or higher.

# Compiling

```
$ git clone https://github.com/processone/mqtree.git
$ cd mqtree
$ make
```

# API

## new/0
```erlang
-spec new() -> tree().
```
Creates new tree. Note that the tree is mutable just like ETS.
The created tree gets destroyed when it's garbage collected.

## insert/2
```erlang
-spec insert(Tree :: tree(), Filter :: iodata()) -> ok.
```
Inserts `Filter` into `Tree` and increases its reference counter.
The reference counter is increased every time when the same
filter is inserted into the tree. The reference counter is decreased
when the filter is deleted, see [delete/2](#delete2).

**NOTE**: no checks are performed on the filter being inserted:
it's up to the caller to check if the filter conforms to the MQTT
specification.

## delete/2
```erlang
-spec delete(Tree :: tree(), Filter :: iodata()) -> ok.
```
Deletes `Filter` from `Tree` and decreases its reference counter.
Nothing is done if the filter is not found in the tree.

**NOTE**: no checks are performed on the filter being deleted:
it's up to the caller to check if the filter conforms to the MQTT
specification.

## match/2
```erlang
-spec match(Tree :: tree(), Path :: iodata()) -> [binary()].
```
Finds filters in `Tree` matching `Path` according to the MQTT
specification.

**NOTE**: no checks are performed on the path being matched:
it's up to the caller to check if the path conforms to the MQTT
specification.

## refc/2
```erlang
-spec refc(Tree :: tree(), Filter :: iodata()) -> non_neg_intger().
```
Returns the reference counter of `Filter` in `Tree`. In particular,
zero (0) is returned if the filter is not found in the tree.

**NOTE**: no checks are performed on the filter being searched:
it's up to the caller to check if the filter conforms to the MQTT
specification.

## clear/1
```erlang
-spec clear(Tree :: tree()) -> ok.
```
Deletes all filters from `Tree`.

## size/1
```erlang
-spec size(Tree :: tree()) -> ok.
```
Returns the size of `Tree`. That is, the number of filters in the
tree (irrespective of their reference counters).

## is_empty/1
```erlang
-spec is_empty(Tree :: tree()) -> boolean().
```
Returns `true` if `Tree` holds no filters. Returns `false` otherwise.
