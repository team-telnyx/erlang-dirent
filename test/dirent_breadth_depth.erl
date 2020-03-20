-module(dirent_breadth_depth).
-include_lib("eunit/include/eunit.hrl").

breadth_depth_test_() ->
  {setup,
   fun () -> string:trim(?cmd("mktemp -d")) end,
   fun (TmpDir) -> ?cmd("rm -rf " ++ TmpDir) end,
   {with,
    [fun depth/1,
     fun long_name/1,
     fun breadth/1]
   }
  }.

depth(TmpDir) ->
  DeepDir = filename:join(TmpDir, "deep"),
  ok = file:make_dir(DeepDir),
  make_deep_directory(DeepDir, 128),
  ?assert(count_dirs(DeepDir) =:= 128).

count_dirs(Path) ->
  {ok, Dir} = dirent:opendir(Path),
  count_dirs(Dir, 0).
count_dirs(Dir, N) ->
  case dirent:readdir(Dir) of
    finished -> N;
    Sub -> N+1+count_dirs(Sub)
  end.

long_name(TmpDir) ->
  LongNameDir = filename:join(TmpDir, "long"),
  ok = file:make_dir(LongNameDir),
  LongName = filename:join(LongNameDir, string:copies("long", 20)),
  file:write_file(LongName, <<>>),
  {ok, Dir} = dirent:opendir(LongNameDir),
  ?assert(dirent:readdir(Dir) =:= LongName).

breadth(TmpDir) ->
  FloodedDir = filename:join(TmpDir, "flooded"),
  ok = file:make_dir(FloodedDir),
  ok = make_flooded_directory(FloodedDir, 10000),
  ?assert(list_files(FloodedDir) =:= 10000).

list_files(Path) ->
  {ok, Dir} = dirent:opendir(Path),
  list_files(Dir, 0).
list_files(Dir, N) ->
  case dirent:readdir(Dir) of
    finished -> N;
    _Name -> list_files(Dir, N+1)
  end.

make_flooded_directory(_Root, 0) -> ok;
make_flooded_directory(Root, N) ->
  Name = base64:encode(crypto:strong_rand_bytes(64)),
  Name0 = string:trim(Name, both, "="),
  File = filename:join(Root, Name0),
  case file:write_file(File, crypto:strong_rand_bytes(64)) of
    ok -> make_flooded_directory(Root, N - 1);
    _Error -> make_flooded_directory(Root, N)
  end.

make_deep_directory(_Root, 0) -> ok;
make_deep_directory(Root, N) ->
  SubDir = filename:join(Root, "sub"),
  ok = file:make_dir(SubDir),
  make_deep_directory(SubDir, N - 1).
