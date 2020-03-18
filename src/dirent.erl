-module(dirent).

-export([opendir/1, readdir/1, readdir/2, set_controlling_process/2,
         foreach/2, foreach/3, fold/3, fold/4]).
-on_load(init/0).

-export_type([dirent/0]).

-define(APPNAME, dirent).
-define(LIBNAME, dirent).

%% Public data types.
-type dirent() :: reference().
-type filename() :: file:name().
-type dirname() :: filename().

encode_path(Path) ->
  prim_file:internal_name2native(Path).
decode_path(NativePath) when is_binary(NativePath) ->
  prim_file:internal_native2name(NativePath).

-spec opendir(Path) -> DirRef when
      Path :: dirname(),
      DirRef :: dirent().
opendir(Path) ->
  try opendir_nif(encode_path(Path)) of
    {ok, Dir} -> {ok, Dir};
    {error, Reason} -> {error, Reason}
  catch
    error:badarg -> {error, badarg}
  end.
opendir_nif(_Path) ->
  not_loaded(?LINE).

-spec readdir(Dir) -> filename() | finished when
      Dir :: dirname().
readdir(Dir) ->
  readdir(Dir, true).

-spec readdir(Dir, SkipInvalid) -> filename() | finished when
      Dir :: dirname(),
      SkipInvalid :: boolean().
readdir(Dir, SkipInvalid) ->
  try case readdir_nif(Dir) of
    finished -> finished;
    RawName ->
      case decode_path(RawName) of
        Converted when is_list(Converted) ->
          Converted;
        {error, _} when SkipInvalid =:= false ->
          RawName;

        %% If the filename cannot be converted, return error or ignore with
        %% optional error logger warning depending on +fn{u|a}{i|e|w} emulator
        %% switches.
        {error, ignore} ->
          readdir(Dir, SkipInvalid);
        {error, warning} ->
          %% This is equal to calling logger:warning/3 which
          %% we don't want to do from code_server during system boot.
          %% We don't want to call logger:timestamp() either.
          catch logger ! {log,warning,"Non-unicode filename ~p ignored\n", [RawName],
                        #{pid=>self(),
                          gl=>group_leader(),
                          time=>os:system_time(microsecond),
                          error_logger=>#{tag=>warning_msg}}},
          readdir(Dir, SkipInvalid);
        {error, _} ->
          {error, {no_translation, RawName}}
      end
  end catch error:badarg ->
    {error, badarg}
  end.
readdir_nif(_Dir) ->
  not_loaded(?LINE).

-spec set_controlling_process(Dir, Pid) -> 'ok' when
      Dir :: dirent(),
      Pid :: pid().
set_controlling_process(Dir, Pid) ->
  set_controller_nif(Dir, Pid).
set_controller_nif(_Dir, _Pid) ->
  not_loaded(?LINE).

-spec foreach(Path, Fun) -> ok | {error, Reason} when
      Path :: dirname(),
      Fun :: fun((F :: filename()) -> cont | halt),
      Reason :: term().
foreach(Path, Fun) ->
  foreach(Path, true, Fun).

-spec foreach(Path, SkipInvalid, Fun) -> ok | {error, Reason} when
      Path :: dirname(),
      SkipInvalid :: boolean(),
      Fun :: fun((F :: filename()) -> cont | halt),
      Reason :: term().
foreach(Path, SkipInvalid, Fun) ->
  case opendir(Path) of
    {ok, Dir} -> foreach_1(Dir, SkipInvalid, Path, Fun);
    Error -> Error
  end.
foreach_1(Dir, SkipInvalid, Path, Fun) ->
  case readdir(Dir, SkipInvalid) of
    finished -> ok;
    File ->
      FullName = filename:join(Path, File),
      case Fun(FullName) of
        halt -> ok;
        cont -> foreach_1(Dir, SkipInvalid, Path, Fun)
      end
  end.

-spec fold(Path, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      Fun :: fun((F :: filename()) -> {cont, AccOut} | {halt, AccOut}),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: term().
fold(Path, Fun, AccIn) ->
  fold(Path, true, Fun, AccIn).

-spec fold(Path, SkipInvalid, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      SkipInvalid :: boolean(),
      Fun :: fun((F :: filename()) -> {cont, AccOut} | {halt, AccOut}),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: term().
fold(Path, SkipInvalid, Fun, AccIn) ->
  case opendir(Path) of
    {ok, Dir} -> fold_1(Dir, SkipInvalid, Path, Fun, AccIn);
    Error -> Error
  end.
fold_1(Dir, SkipInvalid, Path, Fun, AccIn) ->
  case readdir(Dir, SkipInvalid) of
    finished -> {ok, AccIn};
    File ->
      FullName = filename:join(Path, File),
      case Fun(FullName) of
        {halt, AccOut0} -> {ok, AccOut0};
        {cont, AccOut1} -> fold_1(Dir, SkipInvalid, Path, Fun, AccOut1)
      end
  end.

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
