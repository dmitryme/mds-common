% It is a wrapper around gen_server. Instead of exporting handle_cast, handle_call, handle_info, etc it exported only
% on_start, on_msg, on_amsg, on_info. on_stop. All other gen_server exports are omitted, but can be added in future if
% needed. The main purpose of this module is to hide some initialization like start server logger, read server
% configuration, etc.
% on_start - calls when server starts
% on_msg - is a handle_call
% on_amsg - is a handle_cast
% on_info - is a handle_info
% on_stop - calls when server is going to stop

-module(mds_gen_server).
-behaviour(gen_server).

-import(mds_utils, [get_param/2, get_param/3]).

-record(log_info, {lname, lpid, icount = 1}).

-record(state, {module, options, linfo = undefined, mod_state = undefined}).

-export([
      start/2,
      start/3,
      stop/1,
      init/1,
      terminate/2,
      handle_call/3,
      handle_cast/2,
      handle_info/2,
      code_change/3,
      call/2,
      call/3,
      cast/2,
      log/3,
      log/4
   ]).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
   [
      {on_start, 1},
      {on_msg, 3},
      {on_amsg, 2},
      {on_info, 2},
      {on_stop, 2}
   ];
behaviour_info(_Other) ->
   undefined.

-define(LOGGER_NAME(Mod), atom_to_list(Mod) ++ "_logger").

%% start({global, Name}, Mod, Opts) -> Pid()
%% Types:
%%    Name = atom().
%%    Mod = atom() - callback module name
%%    Opts = [ServerOptions | LoggerOptions | UserDefOpts].
%%    ServerOptions = {mds_server, [{root_dir, String()}, {working_dir, String()}]}
%%    LoggerOptions = {mds_logger, [<see mds_logger for details>]}
%%    UserDefOptions = {atom(), [Option]}
%%    Option = term()
start({global, Name}, Module, Opts) ->
   gen_server:start_link({global, Name}, ?MODULE, [#state{module = Module, options = Opts}], []);

start({local, Name}, Module, Opts) ->
   gen_server:start_link({local, Name}, ?MODULE, [#state{module = Module, options = Opts}], []).

start({global, Name}, Opts) ->
   gen_server:start_link({global, Name}, ?MODULE, [#state{module = Name, options = Opts}], []);

start({local, Name}, Opts) ->
   gen_server:start_link({local, Name}, ?MODULE, [#state{module = Name, options = Opts}], []).

stop(Mod) ->
   gen_server:call(Mod, stop),
   ok.

init([State]) ->
   Mod = State#state.module,
   LoggerName = mds_utils:list_to_atom(?LOGGER_NAME(Mod)),

   %% normalize mds_server options. Add root_dir, working_dir if needed.
   ServerOpts = dict:from_list(get_param(mds_server, State#state.options, [{root_dir, "."}, {working_dir,
               atom_to_list(Mod)}])),
   WorkingDir = get_param(working_dir, dict:to_list(ServerOpts), atom_to_list(Mod)),
   NormServerOpts = dict:store(working_dir, WorkingDir, ServerOpts),

   % normalize mds_logger options
   LoggerOpts = dict:from_list(get_param(mds_logger, State#state.options, [])),
   NormLoggerOpts = dict:merge(fun(_Key, A, _B) -> A end, LoggerOpts, NormServerOpts),
   process_flag(trap_exit, true),
   {ok, Pid} = mds_logger:start_link(LoggerName, dict:to_list(NormLoggerOpts)),
   register(LoggerName, Pid),

   AllOpts = dict:from_list(State#state.options),
   NewOpts = dict:store(mds_server, dict:to_list(NormServerOpts), AllOpts),
   NewOptsL = dict:store(mds_logger, dict:to_list(NormLoggerOpts), NewOpts),
   NewState = State#state{options = dict:to_list(NewOptsL)},
   Res = Mod:on_start(NewState#state.options),
   case Res of
     {ok, ModState} ->
        {ok, NewState#state{linfo = #log_info{lname = LoggerName, lpid = Pid}, mod_state = ModState}};
     {ok, ModState, Param} ->
        {ok, NewState#state{linfo = #log_info{lname = LoggerName, lpid = Pid}, mod_state = ModState}, Param};
     _ -> Res
   end.

call({global, Mod}, Msg) ->
   gen_server:call({global, Mod}, Msg);

call(Mod, Msg) ->
   gen_server:call(Mod, Msg).

call({global, Mod}, Msg, Timeout) ->
   gen_server:call({global, Mod}, Msg, Timeout);

call(Mod, Msg, Timeout) ->
   gen_server:call(Mod, Msg, Timeout).

cast({global, Mod}, Msg) ->
   gen_server:cast({global, Mod}, Msg);

cast(Mod, Msg) ->
   gen_server:cast(Mod, Msg).

code_change(_, State, _) ->
   {ok, State}.

terminate(Status, State) ->
   Mod = State#state.module,
   Mod:on_stop(Status, State#state.mod_state),
   Linfo = State#state.linfo,
   mds_logger:stop(Linfo#log_info.lname),
   ok.

% ==================================================================================
% gen_server callbacks
% ==================================================================================
handle_cast(Msg, State) ->
   Mod = State#state.module,
   Res = Mod:on_amsg(Msg, State#state.mod_state),
   case Res of
      {Status, ModState} ->
         {Status, State#state{mod_state = ModState}};
      {Status, ModState, Timeout} ->
         {Status, State#state{mod_state = ModState}, Timeout};
      Unknown ->
         throw({unknown_return, Unknown})
   end.

handle_call(stop, _, State) ->
   {stop, normal, stopped, State};

handle_call(Msg, Who = {_Pid, _Tag}, State) ->
   Mod = State#state.module,
   Res = Mod:on_msg(Msg, Who, State#state.mod_state),
   case Res of
      {reply, Reply, ModState} ->
         {reply, Reply, State#state{mod_state = ModState}};
      {reply, Reply, ModState, Timeout} ->
         {reply, Reply, State#state{mod_state = ModState}, Timeout};
      {noreply, ModState} ->
         {noreply, State#state{mod_state = ModState}};
      {noreply, ModState, Timeout} ->
         {noreply, State#state{mod_state = ModState}, Timeout};
      {stop, Reason, Reply, ModState} ->
         {stop, Reason, Reply, State#state{mod_state = ModState}};
      {stop, Reason, ModState} ->
         {stop, Reason, State#state{mod_state = ModState}};
      Unknown ->
         throw({unknown_return, Unknown})
   end.

handle_info({'EXIT', From, Reason}, State)
   when ((State#state.linfo)#log_info.lpid == From) and (Reason == normal) ->
   error_logger:info_msg("Logger <~p> exited with status <~p>.", [(State#state.linfo)#log_info.lname, Reason]),
   {noreply, State};

handle_info(Msg, State) ->
  Mod = State#state.module,
  Res = Mod:on_info(Msg, State#state.mod_state),
  case Res of
     {Status, ModState} ->
        {Status, State#state{mod_state = ModState}};
     {Status, ModState, Timeout} ->
        {Status, State#state{mod_state = ModState}, Timeout};
     Unknown ->
        throw({uknown_return, Unknown})
  end.

get_existing_log_mod(Mod) ->
   list_to_existing_atom(?LOGGER_NAME(Mod)).

log(Mod, Level, Format, Data) ->
   mds_logger:log(get_existing_log_mod(Mod), Level, Format, Data).

log(Mod, Level, Text) ->
   mds_logger:log(get_existing_log_mod(Mod), Level, Text).
