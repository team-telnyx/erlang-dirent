#include "erl_nif.h"

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <errno.h>

// it might be weird, but this is how it works for now
extern char* erl_errno_id(int error);

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_finished;
static ERL_NIF_TERM am_not_on_controlling_process;

typedef struct {
  DIR *dir_stream;
  char *path;
  size_t path_length;
 
  ErlNifPid controlling_process;
  ErlNifMutex *controller_lock;
} dir_context_t;

static ErlNifResourceType* rtype_dir;

static void gc_dir(ErlNifEnv *env, void* data);

static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info) {
  am_ok = enif_make_atom(env, "ok");
  am_error = enif_make_atom(env, "error");
  am_finished = enif_make_atom(env, "finished");
  am_not_on_controlling_process =
      enif_make_atom(env, "not_on_controlling_process");

  rtype_dir = enif_open_resource_type(env, NULL, "gc_dir", gc_dir, ERL_NIF_RT_CREATE, NULL);

  return 0;
}

static void gc_dir(ErlNifEnv *env, void* data) {
  dir_context_t *d = (dir_context_t*)data;

  free(d->path);

  if (d->dir_stream)
    closedir(d->dir_stream);

  (void)env;
}

static int process_check(ErlNifEnv *env, dir_context_t *d) {
  int is_controlling_process;
  ErlNifPid current_process;

  enif_self(env, &current_process);

  enif_mutex_lock(d->controller_lock);

  is_controlling_process = enif_is_identical(
      enif_make_pid(env, &current_process),
      enif_make_pid(env, &d->controlling_process));

  enif_mutex_unlock(d->controller_lock);

  return is_controlling_process;
}

static ERL_NIF_TERM posix_error_to_tuple(ErlNifEnv *env, int posix_errno) {
  ERL_NIF_TERM error = enif_make_atom(env, erl_errno_id(posix_errno));
  return enif_make_tuple2(env, am_error, error);
}

static int has_invalid_null_termination(const ErlNifBinary *path) {
  const char *null_pos, *end_pos;

  null_pos = memchr(path->data, '\0', path->size);
  end_pos = (const char*)&path->data[path->size] - 1;

  if (null_pos == NULL) {
    return 1;
  }

  /* prim_file:internal_name2native sometimes feeds us data that is "doubly"
   * NUL-terminated, so we'll accept any number of trailing NULs so long as
   * they aren't interrupted by anything else. */
  while (null_pos < end_pos && (*null_pos) == '\0') {
    null_pos++;
  }

  return null_pos != end_pos;
}

static ERL_NIF_TERM open_dir(ErlNifEnv *env, int argc,
    const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM result_term;
  ErlNifBinary path;
  dir_context_t *d;

  d = (dir_context_t *) enif_alloc_resource(rtype_dir, sizeof(dir_context_t));
  memset(d, 0, sizeof(dir_context_t));

  if (argc != 1 || !enif_inspect_binary(env, argv[0], &path)) {
    result_term = enif_make_badarg(env);
    goto err;
  }

  if (has_invalid_null_termination(&path)) {
    result_term = enif_make_badarg(env);
    goto err;
  }

  if ((d->path = malloc(path.size)) == NULL) {
    result_term = enif_raise_exception(env, am_error);
    goto err;
  }

  memcpy(d->path, path.data, path.size);
  d->path_length = path.size - 1;

  d->dir_stream = opendir(d->path);
  if (d->dir_stream == NULL) {
    result_term = posix_error_to_tuple(env, errno);
    goto err;
  }

  enif_self(env, &d->controlling_process);
  d->controller_lock = enif_mutex_create("dirent_controller_lock");

  result_term = enif_make_tuple2(env, am_ok,
      enif_make_resource(env, d));

err:
  if (d)
    enif_release_resource(d);

  return result_term;
}

static int is_ignored_name(int name_length, const char *name) {
  if (name_length == 1 && name[0] == '.') {
    return 1;
  } else if(name_length == 2 && memcmp(name, "..", 2) == 0) {
    return 1;
  }

  return 0;
}

static ERL_NIF_TERM read_dir(ErlNifEnv *env, int argc,
    const ERL_NIF_TERM argv[]) {
  struct dirent *dir_entry;
  dir_context_t *d;

  if (argc != 1) {
    return enif_make_badarg(env);
  } else if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)rtype_dir, (void**)&d)) {
    return enif_make_badarg(env);
  } else if(!process_check(env, d)) {
    return enif_raise_exception(env, am_not_on_controlling_process);
  }

  while ((dir_entry = readdir(d->dir_stream)) != NULL) {
    int name_length = strlen(dir_entry->d_name);

    if (!is_ignored_name(name_length, dir_entry->d_name)) {
      unsigned char *name_bytes;
      ERL_NIF_TERM name_term;

      size_t fullpath_length = d->path_length + 1 + name_length;
      name_bytes = enif_make_new_binary(env, fullpath_length, &name_term);

      memcpy(name_bytes, d->path, d->path_length);
      name_bytes[d->path_length] = '/';
      memcpy(name_bytes + d->path_length + 1, dir_entry->d_name, name_length);
 
      return name_term;
    }
  }

  return am_finished;
}

static ERL_NIF_TERM set_controller(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  dir_context_t *d;

  ErlNifPid new_owner;

  if (argc != 2) {
    return enif_make_badarg(env);
  } else if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)rtype_dir, (void**)&d)) {
    return enif_make_badarg(env);
  } else if (!enif_get_local_pid(env, argv[1], &new_owner)) {
    return enif_make_badarg(env);
  } else if(!process_check(env, d)) {
    return enif_raise_exception(env, am_not_on_controlling_process);
  }

  enif_mutex_lock(d->controller_lock);

  d->controlling_process = new_owner;

  enif_mutex_unlock(d->controller_lock);

  return am_ok;
}

static ErlNifFunc nif_funcs[] = {
  {"opendir_nif", 1, open_dir},
  {"readdir_nif", 1, read_dir},
  {"set_controller_nif", 2, set_controller},
};

ERL_NIF_INIT(dirent, nif_funcs, load, NULL, NULL, NULL)
