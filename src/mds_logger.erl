% This is a process logger implementation. Logger starts as standalone process, all log/4 calls are asyncronous.
-module(mds_logger).

-define(ERROR,   0).
-define(WARNING, 1).
-define(INFO,    2).
-define(DEBUG,   3).
-define(MAX_FSIZE, 1024*1024).
-define(TIMEOUT, 1000).

-export([start_link/2, start/2, stop/1, log/3, init/1, log/4]).

-import(mds_utils, [get_param/2, get_param/3]).

-record(state, {logger_name, working_dir, log_dir, file_name, file_descr, max_fsize, log_level, ealarm = false}).

%% Log structure:
%%   root_dir  - root dir
%%       |
%%      working_dir - working dir
%%           |
%%         DDMMYY dir - current  dir
%%             |
%%           MMDDHHMMSS.err - log file
%%
%% start(Name, Opts) -> {ok, Pid}
%% Types
%%    Name = atom().
%%    Opts = [Option]
%%    Option = {root_dir, String} | {log_level, atom()} | {max_fsize, Integer()} | {working_dir, String} |
%%             {enable_alarm, bool()}
%%      root_dir - root dir when all logging directory will be crealted
%%      log_level - default info. Possible values: error, warning, info, debug. level  of logging. If log level of logged
%%         information is greater than log_level, it will be omitted
%%      max_fsize - size of log file. If size of file is greater, than defined, the log will be written into new log
%%         file. Default 1024 bytes.
%%      working_dir - directory in with all log files of instantiated logger will be placed.
%%      enable_alarm - if true each error and warning will be reported using alarm_handler. Default is off.
start_link(Name, Opts) ->
   Pid = spawn_link(mds_logger, init, [[{name, Name} | Opts]]),
   {ok, Pid}.

start(Name, Opts) ->
   Pid = spawn(mds_logger, init, [[{name, Name} | Opts]]),
   {ok, Pid}.

stop(Pid) when is_pid(Pid) ->
   Pid ! stop;

stop(LoggerName) ->
   case is_process_alive(whereis(LoggerName)) of
      true ->
         LoggerName ! stop,
         ok;
      false ->
         ok
   end.

init(Opts) ->
   process_flag(trap_exit, true),
   LoggerName  = get_param('name',       Opts             ),
   RootDir     = get_param(root_dir,     Opts, "."        ),
   LogLevel    = get_param(log_level,    Opts, info       ),
   MaxFSize    = get_param(max_fsize,    Opts, ?MAX_FSIZE ),
   WorkingDir  = get_param(working_dir,  Opts, LoggerName ),
   EnableAlarm = get_param(enable_alarm, Opts, false),
   {LogDir, FileName, FileHandler} = init_dir(filename:join(RootDir, WorkingDir)),
   State = #state
   {
      logger_name = LoggerName,
      working_dir = filename:join(RootDir, WorkingDir),
      log_dir = LogDir,
      file_name = FileName,
      file_descr = FileHandler,
      max_fsize = MaxFSize,
      log_level = toInt(LogLevel),
      ealarm = EnableAlarm
   },
   loop(State).

loop(State) ->
   receive
      stop ->
         file:close(State#state.file_descr),
         error_logger:info_msg("Logger ~s stopped.", [State#state.logger_name]);
         LogMsg = {data, _Level, _Format, _Data} ->
         NewState = write_log(LogMsg, State),
         loop(NewState);
      {'EXIT', Pid, normal} ->
         LoggerName = State#state.logger_name,
         NewState = write_log({data, ?INFO, "Logger ~p stopped: parent process ~p was closed.", [LoggerName, Pid]}, State),
         self() ! stop,
         loop(NewState);
      {'EXIT', Pid, _} ->
         LoggerName = State#state.logger_name,
         NewState = write_log({data, ?ERROR, "Logger ~p stopped: parent process ~p died.", [LoggerName, Pid]}, State),
         self() ! stop,
         loop(NewState);
      Msg ->
         LoggerName = State#state.logger_name,
         error_logger:error_msg("Logger ~p: unexpected message ~p received.", [LoggerName, Msg]),
         loop(State)
   after ?TIMEOUT ->
      NewLogDir = getLogDir(),
      case State#state.log_dir =/= NewLogDir of
        true ->
           file:close(State#state.file_descr),
           {NewLogDir, NewFileName, NewFileHandler} = init_dir(State#state.working_dir),
           loop(State#state{log_dir = NewLogDir, file_name = NewFileName, file_descr = NewFileHandler});
        false ->
           loop(State)
      end
   end.

write_log({data, Level, Format, Data}, State) ->
   NewLogDir = getLogDir(),
   NewState =
      case State#state.log_dir =/= NewLogDir of
         true ->
            file:close(State#state.file_descr),
            {NewLogDir, NewFileName, NewFileHandler} = init_dir(State#state.working_dir),
            State#state{log_dir = NewLogDir, file_name = NewFileName, file_descr = NewFileHandler};
         false ->
            State
      end,
   if
      Level =< NewState#state.log_level ->
         Now = getTime(),
         Text = lists:flatten(io_lib:format(Format, Data)),
         send_alarm(Level, Now, NewState#state.logger_name, Text, NewState#state.ealarm),
         io:format(NewState#state.file_descr, getFormat(Level), [Now, Text]),
         file:sync(NewState#state.file_descr),
         FSize = filelib:file_size(filename:join([NewState#state.working_dir, NewState#state.log_dir, NewState#state.file_name])),
         if
            FSize >= NewState#state.max_fsize ->
               file:close(NewState#state.file_descr),
               NewFileName1 = getFileName(NewState#state.file_name),
               {ok, NewFileHandler1} = file:open(filename:join([NewState#state.working_dir, NewState#state.log_dir,
                        NewFileName1]), [write]),
               NewState#state{file_name = NewFileName1, file_descr = NewFileHandler1};
            true ->
               NewState
         end;
      true ->
         NewState
   end.

%=====================================================================
% helpers
%=====================================================================

%% log(Logger, LogLevel, Text) -> ok
%%    Logger = atom()
%%    LogLevel = error | warning | info | debug
%%    Text = string()
log(Logger, LogLevel, Text) ->
   try case io_lib:printable_list(Text) of
         true ->
            Logger ! {data, toInt(LogLevel), Text, []},
            ok;
         false ->
            Logger ! {data, toInt(LogLevel), io_lib:format("~p", [Text]), []},
            ok
         end of
      _ -> ok
   catch
      _:Err -> Err
   end.
%% log(Logger, LogLevel, Format, Data) -> ok
%%    Logger = atom()
%%    LogLevel = error | warning | info | debug
%%    Format = string()
%%    Data = [term()]
log(Logger, LogLevel, Format, Data) ->
   try Logger ! {data, toInt(LogLevel), Format, Data} of
      _ -> ok
   catch
      _:Err -> Err
   end.

%=====================================================================
% utils
%=====================================================================

toInt(error)   -> ?ERROR;
toInt(warning) -> ?WARNING;
toInt(info)    -> ?INFO;
toInt(debug)   -> ?DEBUG;
toInt(_)       -> throw(wrong_log_level).

getFormat(?ERROR)    -> "~s ERROR: ~s~n";
getFormat(?WARNING)  -> "~s WARNING: ~s~n";
getFormat(?INFO)     -> "~s INFO: ~s~n";
getFormat(?DEBUG)    -> "~s DEBUG: ~s~n".

init_dir(WorkingDir) ->
   LogDir = getLogDir(),
   filelib:ensure_dir(filename:join([WorkingDir, LogDir])),
   FileName = getFileName(checkFileName(WorkingDir, LogDir)),
   FullPath = filename:join([WorkingDir, LogDir, FileName]),
   filelib:ensure_dir(FullPath),
   {ok, FileHandler} = file:open(FullPath, [write]),
   {LogDir, FileName, FileHandler}.

checkFileName(WorkingDir, LogDir) ->
   Res = lists:sort(
         filelib:fold_files(
            filename:join([WorkingDir, LogDir]),
            ".*.err",
            false,
            fun(F, AccIn) -> [filename:basename(F) | AccIn] end,
            [])),
   checkFileName(Res).

checkFileName(List) when (is_list(List) == true) and (length(List) > 0) ->
   lists:last(List);
checkFileName([]) -> [].

getTime() ->
   Now = {_, _, MicroSec} = now(),
   {{_,_,_},{HH,MM,SS}} = calendar:now_to_local_time(Now),
   lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B", [HH, MM, SS, MicroSec div 1000])).

getFileName(OldFileName) ->
   {{_,MM,DD},{HH,MIN,_}} = calendar:local_time(),
   NewBaseName = lists:flatten(io_lib:format("~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [MM,DD,HH,MIN])),
   {ok, MP} = re:compile("([0-9]{8})\.([0-9]{0,1})\.{0,1}err"),
   Ret = re:run(OldFileName, MP),
   case Ret of
      {match,[_, {A,B}, {C,D}]} ->
         OldBaseName = string:substr(OldFileName, A + 1, B),
         if
            OldBaseName =:= NewBaseName ->
               if
                  D =:= 0 ->
                     lists:flatten(io_lib:format("~s.1.err", [NewBaseName]));
                  true ->
                     {I, _} = string:to_integer(string:substr(OldFileName, C + 1, D)),
                     lists:flatten(io_lib:format("~s.~p.err", [NewBaseName, I + 1]))
               end;
            true ->
               lists:flatten(io_lib:format("~s.err", [NewBaseName]))
         end;
      nomatch ->
         lists:flatten(io_lib:format("~s.err", [NewBaseName]))
   end.

getLogDir() ->
   {{YY,MM,DD},_} = calendar:local_time(),
   io_lib:format("~2.10.0B~2.10.0B~2.10.0B", [YY-2000,MM,DD]).

send_alarm(?ERROR, Now, From, Text, true) ->
   error_logger:error_report(alarm, {error, Now, erlang:atom_to_list(From), Text});
send_alarm(?WARNING, Now, From, Text, true) ->
   error_logger:warning_report(alarm, {warning, Now, erlang:atom_to_list(From), Text});
send_alarm(_, _, _, _, _) ->
   ok.
