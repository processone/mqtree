/*
 * @author Evgeny Khramtsov <ekhramtsov@process-one.net>
 * @copyright (C) 2002-2024 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <erl_nif.h>
#include <stdio.h>
#include <errno.h>
#include "uthash.h"

void __free(void *ptr, size_t size) {
  enif_free(ptr);
}

#undef uthash_malloc
#undef uthash_free
#define uthash_malloc enif_alloc
#define uthash_free __free

/****************************************************************
 *               Structures/Globals definitions                 *
 ****************************************************************/
typedef struct __tree_t {
  char *key;
  char *val;
  int refc;
  struct __tree_t *sub;
  UT_hash_handle hh;
} tree_t;

typedef struct {
  tree_t *tree;
  char *name;
  ErlNifRWLock *lock;
} state_t;

typedef struct {
  char *name;
  state_t *state;
  UT_hash_handle hh;
} registry_t;

static ErlNifResourceType *tree_state_t = NULL;
static registry_t *registry = NULL;
static ErlNifRWLock *registry_lock = NULL;

/****************************************************************
 *                   MQTT Tree Manipulation                     *
 ****************************************************************/
tree_t *tree_new(char *key, size_t len) {
  tree_t *tree = enif_alloc(sizeof(tree_t));
  if (tree) {
    memset(tree, 0, sizeof(tree_t));
    if (key && len) {
      tree->key = enif_alloc(len);
      if (tree->key) {
        memcpy(tree->key, key, len);
      } else {
        enif_free(tree);
        tree = NULL;
      }
    }
  }
  return tree;
}

void tree_free(tree_t *t) {
  tree_t *found, *iter;
  if (t) {
    enif_free(t->key);
    enif_free(t->val);
    HASH_ITER(hh, t->sub, found, iter) {
      HASH_DEL(t->sub, found);
      tree_free(found);
    }
    memset(t, 0, sizeof(tree_t));
    enif_free(t);
  }
}

void tree_clear(tree_t *root) {
  tree_t *found, *iter;
  HASH_ITER(hh, root->sub, found, iter) {
    HASH_DEL(root->sub, found);
    tree_free(found);
  }
}

int tree_add(tree_t *root, char *path, size_t size) {
  int i = 0;
  size_t len;
  tree_t *t = root;
  tree_t *found, *new;

  while (i<=size) {
    len = strlen(path+i) + 1;
    HASH_FIND_STR(t->sub, path+i, found);
    if (found) {
      i += len;
      t = found;
    } else {
      new = tree_new(path+i, len);
      if (new) {
        HASH_ADD_STR(t->sub, key, new);
        i += len;
        t = new;
      } else
        return ENOMEM;
    }
  }

  if (!t->val) {
    t->val = enif_alloc(size+1);
    if (t->val) {
      t->val[size] = 0;
      for (i=0; i<size; i++) {
        char c = path[i];
        t->val[i] = c ? c : '/';
      }
    } else
      return ENOMEM;
  }
  t->refc++;
  return 0;
}

int tree_del(tree_t *root, char *path, size_t i, size_t size) {
  tree_t *found;

  if (i<=size) {
    HASH_FIND_STR(root->sub, path+i, found);
    if (found) {
      i += strlen(path+i) + 1;
      int deleted = tree_del(found, path, i, size);
      if (deleted) {
        HASH_DEL(root->sub, found);
        tree_free(found);
      }
    }
  } else if (root->refc) {
    root->refc--;
    if (!root->refc) {
      enif_free(root->val);
      root->val = NULL;
    }
  }

  return !root->refc && !root->sub;
}

void tree_size(tree_t *tree, size_t *size) {
  tree_t *found, *iter;

  HASH_ITER(hh, tree->sub, found, iter) {
    if (found->refc) (*size)++;
    tree_size(found, size);
  }
}

int tree_refc(tree_t *tree, char *path, size_t i, size_t size) {
  tree_t *found;

  if (i<=size) {
    HASH_FIND_STR(tree->sub, path+i, found);
    if (found) {
      i += strlen(path+i) + 1;
      return tree_refc(found, path, i, size);
    } else {
      return 0;
    }
  } else
    return tree->refc;
}

/****************************************************************
 *                        Registration                          *
 ****************************************************************/
void delete_registry_entry(registry_t *entry) {
  /* registry_lock must be RW-locked! */
  HASH_DEL(registry, entry);
  entry->state->name = NULL;
  enif_release_resource(entry->state);
  enif_free(entry->name);
  enif_free(entry);
}

int register_tree(char *name, state_t *state) {
  registry_t *entry, *found;

  entry = enif_alloc(sizeof(registry_t));
  if (!entry) return ENOMEM;

  entry->name = enif_alloc(strlen(name) + 1);
  if (!entry->name) {
    enif_free(entry);
    return ENOMEM;
  }

  entry->state = state;
  strcpy(entry->name, name);
  enif_rwlock_rwlock(registry_lock);
  HASH_FIND_STR(registry, name, found);
  if (found) {
    enif_rwlock_rwunlock(registry_lock);
    enif_free(entry->name);
    enif_free(entry);
    return EINVAL;
  } else {
    if (state->name) {
      /* Unregistering previously registered name */
      HASH_FIND_STR(registry, state->name, found);
      if (found)
        delete_registry_entry(found);
    }
    enif_keep_resource(state);
    HASH_ADD_STR(registry, name, entry);
    state->name = entry->name;
    enif_rwlock_rwunlock(registry_lock);
    return 0;
  }
}

int unregister_tree(char *name) {
  registry_t *entry;
  int ret;

  enif_rwlock_rwlock(registry_lock);
  HASH_FIND_STR(registry, name, entry);
  if (entry) {
    delete_registry_entry(entry);
    ret = 0;
  } else {
    ret = EINVAL;
  }
  enif_rwlock_rwunlock(registry_lock);

  return ret;
}

/****************************************************************
 *                        NIF helpers                           *
 ****************************************************************/
static ERL_NIF_TERM cons(ErlNifEnv *env, char *str, ERL_NIF_TERM tail)
{
  if (str) {
    size_t len = strlen(str);
    ERL_NIF_TERM head;
    unsigned char *buf = enif_make_new_binary(env, len, &head);
    if (buf) {
      memcpy(buf, str, len);
      return enif_make_list_cell(env, head, tail);
    }
  }
  return tail;
}

static void match(ErlNifEnv *env, tree_t *root,
                  char *path, size_t i, size_t size, ERL_NIF_TERM *acc)
{
  tree_t *found;
  size_t len = 0;

  if (i<=size) {
    HASH_FIND_STR(root->sub, path+i, found);
    if (found) {
      len = strlen(path+i) + 1;
      match(env, found, path, i+len, size, acc);
    };
    if (i || path[0] != '$') {
      HASH_FIND_STR(root->sub, "+", found);
      if (found) {
	len = strlen(path+i) + 1;
	match(env, found, path, i+len, size, acc);
      }
      HASH_FIND_STR(root->sub, "#", found);
      if (found) {
	*acc = cons(env, found->val, *acc);
      }
    }
  } else {
    *acc = cons(env, root->val, *acc);
    HASH_FIND_STR(root->sub, "#", found);
    if (found)
      *acc = cons(env, found->val, *acc);
  }
}

static void to_list(ErlNifEnv *env, tree_t *root, ERL_NIF_TERM *acc)
{
  tree_t *found, *iter;

  HASH_ITER(hh, root->sub, found, iter) {
    if (found->val) {
      size_t len = strlen(found->val);
      ERL_NIF_TERM refc = enif_make_int(env, found->refc);
      ERL_NIF_TERM val;
      unsigned char *buf = enif_make_new_binary(env, len, &val);
      if (buf) {
        memcpy(buf, found->val, len);
        *acc = enif_make_list_cell(env, enif_make_tuple2(env, val, refc), *acc);
      }
    };
    to_list(env, found, acc);
  }
}

static ERL_NIF_TERM dump(ErlNifEnv *env, tree_t *tree)
{
  tree_t *found, *iter;
  ERL_NIF_TERM tail, head;

  tail = enif_make_list(env, 0);
  HASH_ITER(hh, tree->sub, found, iter) {
    head = dump(env, found);
    tail = enif_make_list_cell(env, head, tail);
  }
  if (tree->key) {
    ERL_NIF_TERM part, path;
    part = enif_make_string(env, tree->key, ERL_NIF_LATIN1);
    if (tree->val)
      path = enif_make_string(env, tree->val, ERL_NIF_LATIN1);
    else
      path = enif_make_atom(env, "none");
    return enif_make_tuple4(env, part, path, enif_make_int(env, tree->refc), tail);
  } else
    return tail;
}

static ERL_NIF_TERM raise(ErlNifEnv *env, int err)
{
  switch (err) {
  case ENOMEM:
    return enif_raise_exception(env, enif_make_atom(env, "enomem"));
  default:
    return enif_make_badarg(env);
  }
}

void prep_path(char *path, ErlNifBinary *bin) {
  int i;
  unsigned char c;
  path[bin->size] = 0;
  for (i=0; i<bin->size; i++) {
    c = bin->data[i];
    path[i] = (c == '/') ? 0 : c;
  }
}

/****************************************************************
 *                 Constructors/Destructors                     *
 ****************************************************************/
static state_t *init_tree_state(ErlNifEnv *env) {
  state_t *state = enif_alloc_resource(tree_state_t, sizeof(state_t));
  if (state) {
    memset(state, 0, sizeof(state_t));
    state->tree = tree_new(NULL, 0);
    state->lock = enif_rwlock_create("mqtree_lock");
    if (state->tree && state->lock)
      return state;
    else
      enif_release_resource(state);
  }
  return NULL;
}

static void destroy_tree_state(ErlNifEnv *env, void *data) {
  state_t *state = (state_t *) data;
  if (state) {
    tree_free(state->tree);
    if (state->lock) enif_rwlock_destroy(state->lock);
  }
  memset(state, 0, sizeof(state_t));
}

/****************************************************************
 *                      NIF definitions                         *
 ****************************************************************/
static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM max) {
  registry_lock = enif_rwlock_create("mqtree_registry");
  if (registry_lock) {
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    tree_state_t = enif_open_resource_type(env, NULL, "mqtree_state",
                                           destroy_tree_state,
                                           flags, NULL);
    return 0;
  }
  return ENOMEM;
}

static void unload(ErlNifEnv* env, void* priv) {
  if (registry_lock) {
    enif_rwlock_destroy(registry_lock);
    registry_lock = NULL;
  }
}

static ERL_NIF_TERM new_0(ErlNifEnv* env, int argc,
                          const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;
  state_t *state = init_tree_state(env);
  if (state) {
    result = enif_make_resource(env, state);
    enif_release_resource(state);
  } else
    result = raise(env, ENOMEM);

  return result;
}

static ERL_NIF_TERM insert_2(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[])
{
  state_t *state;
  ErlNifBinary path_bin;

  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state) ||
      !enif_inspect_iolist_as_binary(env, argv[1], &path_bin))
    return raise(env, EINVAL);

  if (!path_bin.size)
    return enif_make_atom(env, "ok");

  char path[path_bin.size+1];
  prep_path(path, &path_bin);
  enif_rwlock_rwlock(state->lock);
  int ret = tree_add(state->tree, path, path_bin.size);
  enif_rwlock_rwunlock(state->lock);

  if (!ret)
    return enif_make_atom(env, "ok");
  else
    return raise(env, ret);
}

static ERL_NIF_TERM delete_2(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[])
{
  state_t *state;
  ErlNifBinary path_bin;

  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state) ||
      !enif_inspect_iolist_as_binary(env, argv[1], &path_bin))
    return raise(env, EINVAL);

  if (!path_bin.size)
    return enif_make_atom(env, "ok");

  char path[path_bin.size+1];
  prep_path(path, &path_bin);
  enif_rwlock_rwlock(state->lock);
  tree_del(state->tree, path, 0, path_bin.size);
  enif_rwlock_rwunlock(state->lock);

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM match_2(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[])
{
  state_t *state;
  ErlNifBinary path_bin;
  ERL_NIF_TERM result = enif_make_list(env, 0);

  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state) ||
      !enif_inspect_iolist_as_binary(env, argv[1], &path_bin))
    return raise(env, EINVAL);

  if (!path_bin.size)
    return result;

  char path[path_bin.size+1];
  prep_path(path, &path_bin);
  enif_rwlock_rlock(state->lock);
  match(env, state->tree, path, 0, path_bin.size, &result);
  enif_rwlock_runlock(state->lock);

  return result;
}

static ERL_NIF_TERM refc_2(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[])
{
  state_t *state;
  ErlNifBinary path_bin;

  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state) ||
      !enif_inspect_iolist_as_binary(env, argv[1], &path_bin))
    return raise(env, EINVAL);

  if (!path_bin.size)
    return enif_make_int(env, 0);

  char path[path_bin.size+1];
  prep_path(path, &path_bin);
  enif_rwlock_rlock(state->lock);
  int refc = tree_refc(state->tree, path, 0, path_bin.size);
  enif_rwlock_runlock(state->lock);

  return enif_make_int(env, refc);
}

static ERL_NIF_TERM clear_1(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[])
{
  state_t *state;
  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  enif_rwlock_rwlock(state->lock);
  tree_clear(state->tree);
  enif_rwlock_rwunlock(state->lock);

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM size_1(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[])
{
  state_t *state;
  size_t size = 0;
  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  enif_rwlock_rlock(state->lock);
  tree_size(state->tree, &size);
  enif_rwlock_runlock(state->lock);

  return enif_make_uint64(env, (ErlNifUInt64) size);
}

static ERL_NIF_TERM is_empty_1(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[])
{
  state_t *state;
  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  enif_rwlock_rlock(state->lock);
  char *ret = state->tree->sub ? "false" : "true";
  enif_rwlock_runlock(state->lock);

  return enif_make_atom(env, ret);
}

static ERL_NIF_TERM to_list_1(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[])
{
  state_t *state;
  ERL_NIF_TERM result = enif_make_list(env, 0);

  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  enif_rwlock_rlock(state->lock);
  to_list(env, state->tree, &result);
  enif_rwlock_runlock(state->lock);

  return result;
}

static ERL_NIF_TERM dump_1(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[])
{
  state_t *state;
  if (!enif_get_resource(env, argv[0], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  enif_rwlock_rlock(state->lock);
  ERL_NIF_TERM result = dump(env, state->tree);
  enif_rwlock_runlock(state->lock);

  return result;
}

static ERL_NIF_TERM register_2(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[])
{
  state_t *state;
  unsigned int len;
  int ret;

  if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1) ||
      !enif_get_resource(env, argv[1], tree_state_t, (void *) &state))
    return raise(env, EINVAL);

  char name[len+1];
  enif_get_atom(env, argv[0], name, len+1, ERL_NIF_LATIN1);
  if (!strcmp(name, "undefined"))
    return raise(env, EINVAL);

  ret = register_tree(name, state);
  if (ret)
    return raise(env, ret);
  else
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM unregister_1(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[])
{
  unsigned int len;
  int ret;

  if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1))
    return raise(env, EINVAL);

  char name[len+1];
  enif_get_atom(env, argv[0], name, len+1, ERL_NIF_LATIN1);
  ret = unregister_tree(name);
  if (ret)
    return raise(env, ret);
  else
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM whereis_1(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[])
{
  unsigned int len;
  registry_t *entry;
  ERL_NIF_TERM result;

  if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1))
    return raise(env, EINVAL);

  char name[len+1];
  enif_get_atom(env, argv[0], name, len+1, ERL_NIF_LATIN1);
  enif_rwlock_rlock(registry_lock);
  HASH_FIND_STR(registry, name, entry);
  if (entry)
    result = enif_make_resource(env, entry->state);
  else
    result = enif_make_atom(env, "undefined");
  enif_rwlock_runlock(registry_lock);

  return result;
}

static ERL_NIF_TERM registered_0(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[])
{
  registry_t *entry, *iter;
  ERL_NIF_TERM result = enif_make_list(env, 0);

  enif_rwlock_rlock(registry_lock);
  HASH_ITER(hh, registry, entry, iter) {
    result = enif_make_list_cell(env, enif_make_atom(env, entry->name), result);
  }
  enif_rwlock_runlock(registry_lock);

  return result;
}

static ErlNifFunc nif_funcs[] =
  {
    {"new", 0, new_0},
    {"insert", 2, insert_2},
    {"delete", 2, delete_2},
    {"match", 2, match_2},
    {"refc", 2, refc_2},
    {"clear", 1, clear_1},
    {"size", 1, size_1},
    {"is_empty", 1, is_empty_1},
    {"to_list", 1, to_list_1},
    {"dump", 1, dump_1},
    {"register", 2, register_2},
    {"unregister", 1, unregister_1},
    {"whereis", 1, whereis_1},
    {"registered", 0, registered_0}
  };

ERL_NIF_INIT(mqtree, nif_funcs, load, NULL, NULL, unload)
