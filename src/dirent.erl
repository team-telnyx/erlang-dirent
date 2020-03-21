-module(dirent).

-export([opendir/1, readdir/1, readdir_type/1, readdir_all/1, readdir_raw/1,
         controlling_process/2, fold_files/4, fold_files/5]).

-on_load(init/0).

-export_type([dirent/0]).

-include_lib("kernel/include/file.hrl").

-define(APPNAME, dirent).
-define(LIBNAME, dirent).

%% Public data types.

-type dirent() :: reference(). %% Directory reference.

-type filename() :: file:name(). %% File name.

-type dirname() :: filename(). %% Directory name.

%% @type dtype(). Indicates the file type.
%%
%% <dl>
%%  <dt>`device'</dt>
%%    <dd>This is a block or character device.</dd>
%%  <dt>`directory'</dt>
%%    <dd>This is a directory.</dd>
%%  <dt>`symlink'</dt>
%%    <dd>This is a symbolic link.</dd>
%%  <dt>`regular'</dt>
%%    <dd>This is a regular file.</dd>
%%  <dt>`other'</dt>
%%    <dd>This is a UNIX domain socket or named pipe (FIFO).</dd>
%%  <dt>`undefined'</dt>
%%    <dd>The file type could not be determined.</dd>
%% </dl>
-type dtype() :: device | directory | symlink | regular | other | undefined.

%% @type posix_error(). Common POSIX errors:
%%
%% <dl>
%%   <dt>`enoent'</dt>
%%     <dd>Directory does not exist, or `Path' is an empty string.</dd>
%%   <dt>`eacces'</dt>
%%     <dd>Permission denied.</dd>
%%   <dt>`emfile'</dt>
%%     <dd>The per-process limit on the number of open file descriptors has
%%         been reached.</dd>
%%   <dt>`enfile'</dt>
%%     <dd>The system-wide limit on the total number of open files has been
%%         reached.</dd>
%%   <dt>`enomem'</dt>
%%     <dd>Insufficient memory to complete the operation.</dd>
%%   <dt>`enotdir'</dt>
%%     <dd>`Path' is not a directory.</dd>
%% </dl>
-type posix_error() :: enoent | eacces | emfile | enfile | enomem | enotdir | atom().

% Public functions

%% @doc Opens the given `Path'. Returns `{ok, DirRef}' if successful, otherwise
%% `{error, Reason}'.
-spec opendir(Path) -> DirRef | {error, Reason} when
      Path :: dirname(),
      DirRef :: dirent(),
      Reason :: posix_error().
opendir(Path) ->
  try opendir_nif(encode_path(Path)) of
    {ok, DirRef} -> {ok, DirRef};
    {error, Reason} -> {error, Reason}
  catch
    error:badarg -> {error, badarg}
  end.

%% @doc Lists all files in `DirRef', <strong>except</strong> files with raw
%% names. Returns `F' while iterating over the directory or `finished' when
%% done.
%%
%% If the filename is a `binary()' with characters coded in ISO Latin-1 and the
%% VM was started with parameter `+fnue', the function returns
%% `{error, {no_translation, RawName}}'.
%%
%% If called by any other process than the current controlling process,
%% `{error, not_owner}' is returned.
%%
%% The names are not sorted.
-spec readdir(DirRef) -> F | finished | {error, Reason} when
      DirRef :: dirent(),
      F :: filename(),
      Reason :: {no_translation, RawName} | not_owner,
      RawName :: binary().
readdir(DirRef) ->
  try case readdir_nif(DirRef, false) of
    finished -> finished;
    {error, Reason} -> {error, Reason};
    RawName ->
      case readdir_skip(RawName) of
        skip -> readdir(DirRef);
        {error, Reason} -> {error, Reason};
        F -> F
      end
  end catch error:badarg ->
    {error, badarg}
  end.
readdir_skip(RawName) ->
  case decode_path(RawName) of
    Converted when is_list(Converted) ->
      Converted;

    %% If the filename cannot be converted, return error or ignore with
    %% optional error logger warning depending on +fn{u|a}{i|e|w} emulator
    %% switches.
    {error, ignore} ->
      skip;
    {error, warning} ->
      %% This is equal to calling logger:warning/3 which
      %% we don't want to do from code_server during system boot.
      %% We don't want to call logger:timestamp() either.
      catch logger ! {log,warning,"Non-unicode filename ~p ignored\n", [RawName],
                    #{pid=>self(),
                      gl=>group_leader(),
                      time=>os:system_time(microsecond),
                      error_logger=>#{tag=>warning_msg}}},
      skip;
    {error, _} ->
      {error, {no_translation, RawName}}
  end.

%% @doc Lists all files in `DirRef', <strong>except</strong> files with raw
%% names, returning also the file type.
%%
%% Returns `{F, Dtype}' or `finished' when done. `Dtype' can be used to
%% avoid making a second call to `file:read_file_info/1,2' if the filesystem
%% has support to it in the system `readdir'. If the filesystem does not have
%% support, `Dtype' will be always `undefined'.
%%
%% If the filename is a `binary()' with characters coded in ISO Latin-1 and the
%% VM was started with parameter `+fnue', the function returns
%% `{error, {no_translation, RawName}}'.
%%
%% If called by any other process than the current controlling process,
%% `{error, not_owner}' is returned.
%%
%% The names are not sorted.
-spec readdir_type(DirRef) -> {F, Dtype} | finished | {error, Reason} when
      DirRef :: dirent(),
      F :: filename(),
      Dtype :: dtype(),
      Reason :: {no_translation, RawName} | not_owner,
      RawName :: binary().
readdir_type(DirRef) ->
  try case readdir_nif(DirRef, true) of
    finished -> finished;
    {error, Reason} -> {error, Reason};
    {RawName, Dtype} ->
      case readdir_skip(RawName) of
        skip -> readdir_type(DirRef);
        {error, Reason} -> {error, Reason};
        F -> {F, Dtype}
      end
  end catch error:badarg ->
    {error, badarg}
  end.

%% @doc Lists all files in `DirRef', including files with raw names, returning
%% also the file type.
%%
%% Returns `{F, Dtype}' or `finished' when done. `Dtype' can be used to avoid
%% making a second call to `file:read_file_info/1,2' if the filesystem has
%% support to it in the system `readdir'. If the filesystem does not have
%% support, `Dtype' will be always `undefined'.
%%
%% If Unicode filename translation is in effect and the file system is
%% transparent, filenames that cannot be interpreted as Unicode can be
%% encountered, in which case `F' will represent a raw filename (that is,
%% binary).
%%
%% If called by any other process than the current controlling process,
%% `{error, not_owner}' is returned.
%%
%% The names are not sorted.
-spec readdir_all(DirRef) -> {F, Dtype} | finished | {error, Reason} when
      DirRef :: dirent(),
      F :: filename() | binary(),
      Dtype :: dtype(),
      Reason :: not_owner.
readdir_all(DirRef) ->
  try case readdir_nif(DirRef, true) of
    finished -> finished;
    {error, Reason} -> {error, Reason};
    {RawName, Dtype} ->
      case decode_path(RawName) of
        Converted when is_list(Converted) ->
          {Converted, Dtype};
        {error, _} ->
          {RawName, Dtype}
      end
  end catch error:badarg ->
    {error, badarg}
  end.

%% @doc Lists raw filenames in `DirRef', returning also the file type.
%%
%% No Unicode translation is made on the filename, and it is returned in raw
%% format (binary).
%%
%% Returns `{F, Dtype}' or `finished' when done. `Dtype' can be used to avoid
%% making a second call to `file:read_file_info/1,2' if the filesystem has
%% support to it in the system `readdir'. If the filesystem does not have
%% support, `Dtype' will be always `undefined'.
%%
%% The names are not sorted.
-spec readdir_raw(DirRef) -> {F, Dtype} | finished | {error, Reason} when
      DirRef :: dirent(),
      F :: filename() | binary(),
      Dtype :: dtype(),
      Reason :: not_owner.
readdir_raw(DirRef) ->
  try case readdir_nif(DirRef, true) of
    finished -> finished;
    {error, Reason} -> {error, Reason};
    {RawName, Dtype} -> {RawName, Dtype}
  end catch error:badarg ->
    {error, badarg}
  end.

%% @doc Set controlling affinity.
%% 
%% Once created, `DirRef' is associated to the calling process and reading
%% functions should be executed by the same process. If passing to another
%% process is required, then this function should be called from the process
%% owner, delegating control to another process indicated by `Pid'.
-spec controlling_process(DirRef, Pid) -> 'ok' when
      DirRef :: dirent(),
      Pid :: pid().
controlling_process(DirRef, Pid) ->
  set_controller_nif(DirRef, Pid).

%% @doc Folds function `Fun' over all (regular) files `F' in directory `Path'.
%% If `Recursive' is `true', all subdirectories to `Path' are processed.
%%
%% If Unicode filename translation is in effect and the file system is
%% transparent, filenames that cannot be interpreted as Unicode will be skipped.
%%
%% For more information about raw filenames, see the file module.
-spec fold_files(Path, Recursive, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      Recursive :: boolean(),
      Fun :: fun((F, AccIn) -> AccOut),
      F :: filename() | binary(),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: {no_translation, RawName} | posix_error(),
      RawName :: binary().
fold_files(Path, Recursive, Fun, AccIn) when is_binary(Path) or is_list(Path) ->
  fold_files(Path, Recursive, Fun, AccIn, []).

%% @doc Folds function `Fun' over all files `F' in directory `Path'.  If
%% `Recursive' is `true', all subdirectories to `Path' are processed.
%%
%% If option `all' is passed, then `F' can be any file type, but filenames that
%% cannot be interpreted as Unicode will be passed as `binary'. If option `raw'
%% is passed, then `F' can also be any file type, but all filenames will be
%% `binary'. If no option is passed (`Opts' is an empty list), then it will
%% behave as `fold_files/4'.
-spec fold_files(Path, Recursive, Fun, AccIn, Opts) -> {ok, AccOut} | {halted, AccOut} | {error, Reason} when
      Path :: dirname(),
      Recursive :: boolean(),
      Fun :: fun((F, AccIn) -> AccOut | {halt, AccOut}),
      F :: filename() | binary(),
      AccIn :: term(),
      Opts :: list(Opt),
      Opt :: all | raw,
      AccOut :: term(),
      Reason :: {no_translation, RawName} | posix_error(),
      RawName :: binary().
fold_files(Path, Recursive, Fun, AccIn, Opts) when is_binary(Path) or is_list(Path) ->
  case opendir(Path) of
    {ok, DirRef} -> fold_files_inner(DirRef, Recursive, Fun, AccIn, Opts);
    {error, Reason} -> {error, Reason}
  end.
fold_files_inner(DirRef, Recursive, Fun, AccIn, Opts) when is_reference(DirRef) ->
  case fold_files_read(DirRef, Opts) of
    finished -> {ok, AccIn};
    {error, Reason} -> {error, Reason};
    {Path, directory} when Recursive =:= true ->
      fold_files_depth(Path, DirRef, Recursive, Fun, AccIn, Opts);
    {Path, regular} ->
      fold_files_breadth(Path, DirRef, Recursive, Fun, AccIn, Opts);
    {Path, undefined} ->
      case file:read_file_info(Path) of
        {ok, #file_info{type=directory}} ->
          fold_files_depth(Path, DirRef, Recursive, Fun, AccIn, Opts);
        {ok, _Regular} ->
          fold_files_breadth(Path, DirRef, Recursive, Fun, AccIn, Opts);
        {error, Reason} ->
          {error, Reason}
      end;
    {_Path, _Type} when Opts =:= [] ->
      fold_files_inner(DirRef, Recursive, Fun, AccIn, Opts);
    {Path, _Type} ->
      fold_files_breadth(Path, DirRef, Recursive, Fun, AccIn, Opts)
  end.

fold_files_read(DirRef, []) ->
  readdir_type(DirRef);
fold_files_read(DirRef, [all]) ->
  readdir_all(DirRef);
fold_files_read(DirRef, [raw]) ->
  readdir_raw(DirRef);
fold_files_read(_DirRef, _) ->
  error(badarg).

fold_files_breadth(Path, DirRef, Recursive, Fun, AccIn, Opts) ->
  case Fun(Path, AccIn) of
    {halt, AccOut} ->
      {halted, AccOut};
    AccOut ->
      fold_files_inner(DirRef, Recursive, Fun, AccOut, Opts)
  end.

fold_files_depth(Path, DirRef, Recursive, Fun, AccIn, Opts) ->
  case fold_files(Path, Recursive, Fun, AccIn, Opts) of
    {ok, AccOut} ->
      fold_files_inner(DirRef, Recursive, Fun, AccOut, Opts);
    {halted, AccOut} ->
      {halted, AccOut};
    {error, Reason} ->
      {error, Reason}
  end.

% Private functions

encode_path(Path) ->
  prim_file:internal_name2native(Path).
decode_path(NativePath) when is_binary(NativePath) ->
  prim_file:internal_native2name(NativePath).

% NIF loading

set_controller_nif(_DirRef, _Pid) ->
  not_loaded(?LINE).
opendir_nif(_Path) ->
  not_loaded(?LINE).
readdir_nif(_DirRef, _ReturnDtype) ->
  not_loaded(?LINE).

init() ->
  SoName = case code:priv_dir(?APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", priv])) of
        true ->
          filename:join(["..", priv, ?LIBNAME]);
        _ ->
          filename:join([priv, ?LIBNAME])
      end;
    Dir ->
      filename:join(Dir, ?LIBNAME)
  end,
  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
