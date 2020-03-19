-module(dirent).

-export([opendir/1, readdir/1, readdir/2, readdir/3, set_controlling_process/2,
         foreach/2, foreach/3, foreach/4, fold/3, fold/4, fold/5]).
-on_load(init/0).

-export_type([dirent/0]).

-define(APPNAME, dirent).
-define(LIBNAME, dirent).

%% Public data types.
-type dirent() :: reference().
-type filename() :: file:name().
-type dirname() :: filename().
-type dtype() :: blk | chr | dir | fifo | lnk | reg | sock | unk.

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
  readdir(Dir, false, true).

-spec readdir(Dir, ReturnDtype) -> filename() | finished when
      Dir :: dirname(),
      ReturnDtype :: boolean().
readdir(Dir, ReturnDtype) ->
  readdir(Dir, ReturnDtype, true).

-spec readdir(Dir, ReturnDtype, SkipInvalid) -> filename() | finished when
      Dir :: dirname(),
      SkipInvalid :: boolean(),
      ReturnDtype :: boolean().
readdir(Dir, ReturnDtype, SkipInvalid) ->
  try case readdir_nif(Dir, ReturnDtype) of
    finished -> finished;
    {RawName, Dtype} ->
      case readdir_name(RawName, SkipInvalid) of
        skip -> readdir(Dir, SkipInvalid, ReturnDtype);
        {error, Reason} -> {error, Reason};
        Name -> {Name, Dtype}
      end;
    RawName ->
      case readdir_name(RawName, SkipInvalid) of
        skip -> readdir(Dir, SkipInvalid, ReturnDtype);
        {error, Reason} -> {error, Reason};
        Name -> Name
      end
  end catch error:badarg ->
    {error, badarg}
  end.
readdir_name(RawName, SkipInvalid) ->
  case decode_path(RawName) of
    Converted when is_list(Converted) ->
      Converted;
    {error, _} when SkipInvalid =:= false ->
      RawName;

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
readdir_nif(_Dir, _ReturnDtype) ->
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
  foreach(Path, false, true, Fun).

-spec foreach(Path, ReturnDtype, Fun) -> ok | {error, Reason} when
      Path :: dirname(),
      ReturnDtype :: boolean(),
      Fun :: fun((filename() | {filename(), dtype()}) -> cont | halt),
      Reason :: term().
foreach(Path, ReturnDtype, Fun) ->
  foreach(Path, ReturnDtype, true, Fun).

-spec foreach(Path, ReturnDtype, SkipInvalid, Fun) -> ok | {error, Reason} when
      Path :: dirname(),
      ReturnDtype :: boolean(),
      SkipInvalid :: boolean(),
      Fun :: fun((filename() | {filename(), dtype()}) -> cont | halt),
      Reason :: term().
foreach(Path, ReturnDtype, SkipInvalid, Fun) ->
  case opendir(Path) of
    {ok, Dir} -> foreach_1(Dir, ReturnDtype, SkipInvalid, Fun);
    Error -> Error
  end.
foreach_1(Dir, ReturnDtype, SkipInvalid, Fun) ->
  case readdir(Dir, ReturnDtype, SkipInvalid) of
    finished -> ok;
    Result ->
      case Fun(Result) of
        halt -> ok;
        cont -> foreach_1(Dir, ReturnDtype, SkipInvalid, Fun)
      end
  end.

-spec fold(Path, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      Fun :: fun((F :: filename()) -> {cont, AccOut} | {halt, AccOut}),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: term().
fold(Path, Fun, AccIn) ->
  fold(Path, false, true, Fun, AccIn).

-spec fold(Path, ReturnDtype, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      ReturnDtype :: boolean(),
      Fun :: fun((F :: filename()) -> {cont, AccOut} | {halt, AccOut}),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: term().
fold(Path, ReturnDtype, Fun, AccIn) ->
  fold(Path, ReturnDtype, true, Fun, AccIn).

-spec fold(Path, ReturnDtype, SkipInvalid, Fun, AccIn) -> {ok, AccOut} | {error, Reason} when
      Path :: dirname(),
      ReturnDtype :: boolean(),
      SkipInvalid :: boolean(),
      Fun :: fun((F :: filename()) -> {cont, AccOut} | {halt, AccOut}),
      AccIn :: term(),
      AccOut :: term(),
      Reason :: term().
fold(Path, ReturnDtype, SkipInvalid, Fun, AccIn) ->
  case opendir(Path) of
    {ok, Dir} -> fold_1(Dir, ReturnDtype, SkipInvalid, Fun, AccIn);
    Error -> Error
  end.
fold_1(Dir, ReturnDtype, SkipInvalid, Fun, AccIn) ->
  case readdir(Dir, ReturnDtype, SkipInvalid) of
    finished -> {ok, AccIn};
    File ->
      case Fun(File, AccIn) of
        {halt, AccOut0} -> {ok, AccOut0};
        {cont, AccOut1} -> fold_1(Dir, ReturnDtype, SkipInvalid, Fun, AccOut1)
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
