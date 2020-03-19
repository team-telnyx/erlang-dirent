# dirent - Iterative directory listing library

Copyright (c) 2020 Telnyx LLC.

__Version:__ 1.0.0

# dirent

**dirent** is an iterative directory listing for Erlang.

Erlang functions such as `filelib:fold_files/5`, `file:list_dir/1`, `c:ls/0,1`,
etc return files from directories _after_ reading them from the filesystem.
When you have an humongous number of files on a single folder, all these
functions will block for a certain time.

In these cases you may not be interested in returning the full list of files,
but instead you may want to list them _iteratively_, returning each entry after
the another to your process, at the moment they are taken from
[_readdir_](http://man7.org/linux/man-pages/man3/readdir.3.html).

## Installation

Download the sources from our [Github repository](http://github.com/team-telnyx/erlang-dirent)

To build the application simply run `rebar3 compile`.

Or add it to your rebar config add:

```erlang

{deps, [
    ....
    {dirent, ".*", {git, "git://github.com/team-telnyx/dirent.git", {branch, "master"}}}
]}.
```

## Basic usage

The most basic usage of `dirent` is:

```
> {ok, DirRef} = dirent:opendir(".").
{ok,#Ref<0.1857889054.1430650882.66300>}
> PrintDir = fun F(DirRef) ->
>   case dirent:readdir(DirRef) of
>     finished -> ok;
>     {error, Reason} -> {error, Reason};
>     File -> io:format("~s~n", [File]), F(DirRef)
>   end
> end.
#Fun<erl_eval.31.126501267>
> PrintDir(DirRef).
./LICENSE
./MAINTAINERS
./NOTICE
./README.md
./priv
./.gitignore
./c_src
./doc
./rebar.config
./src
ok
```

In the example above the function `dirent:readdir/1` was used. But you can also
use `dirent:readdir_type/1` and take the advantage that some well known
filesystems has the capability to return the file type:

```erlang
recurse_dir(Dir) ->
  {ok, DirRef} = dirent:opendir(Dir),
  list_files(DirRef).

list_files(DirRef) ->
  case dirent:readdir_type(DirRef) of
    finished -> ok;
    {error, Reason} ->
      {error, Reason};
    {Dir, directory} ->
      recurse_dir(Dir),
      list_files(DirRef);
    {File, _type} ->
      io:format("~s~n", [File]),
      list_files(DirRef)
  end.
```

In the example above, there is no need to make a second call to
`file:read_file_info/1,2` which might add considerable overhead when you have
too many files to list.

If invalid unicode characters are found in the file names and the filesystem
doesn't handle that correctly, they will be skipped or `{error,
{no_translation, RawName}}` will be returned.  That's the same behavior of
Erlang's `file:list_dir/1`, and the behavior depends on the `+fn{u|a}{i|e|w}`
emulator switches.

For even more advanced usage, there is also `dirent:readdir_all/1` and
`dirent:readdir_raw/1`. The main difference of these latter functions is
regarding to invalid filenames.

`dirent:readdir_all/1` will attempt to translate the filename to a charlist,
but if not possible, the raw binary will be returned.

`dirent:readdir_raw/1` will not even attempt to translate the filename, the raw
binary representation is always returned.
