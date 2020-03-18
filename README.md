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

```erlang
> {ok, Dir} = dirent:opendir(".").
{ok,#Ref<0.1857889054.1430650882.66300>}
> dirent:readdir(Dir).
"LICENSE"
> dirent:readdir(Dir).
"README.md"
> dirent:readdir(Dir).
"rebar.config"
...
> dirent:readdir(Dir).
finished
```

But you can also use the functions `dirent:foreach/2` and `dirent:fold/3`.

For example, to print all files in the folder, one per line, do:

```
dirent:foreach(".", fun(F) -> io:fwrite("~s~n", [F]), cont end).
```

and if you need to collect all files from a folder in a list, do:

```
dirent:fold(".", fun(F, Tail) -> {cont, [F | Tail]} end, []).
```

The `dirent:foreach/2` function `Fun` can return `cont` or `halt`. In the
former case, the loop will continue reading files, while in the latter case the
loop will halt and the function will return `ok`.

The `dirent:fold/3` function `Fun` can return `{cont, AccOut}` or `{halt,
AccOut}`, likewise the above, but also returning the accumulator, which is
passed to the `{ok, AccOut}` return value.
