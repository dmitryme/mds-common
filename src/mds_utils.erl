-module(mds_utils).

-export([
      get_param/2
      ,get_param/3
      ,list_to_atom/1
      ,epoch_time/0
      ,send_email/3
      ,now_to_string/1]).

% get_param(Key, Opts) -> term()
%     Key = atom()
%     Opts = [Option]
%     Option = {atom(), term()}
get_param(Key, Opts) ->
   case proplists:get_value(Key, Opts) of
      undefined -> throw({no_prop, Key, Opts});
      Val -> Val
   end.

% get_param(Key, Opts, DefVal) -> term() | DefVal
%     DefVal = term()
%     Key = atom()
%     Opts = [Option]
%     Option = {atom(), term()}
get_param(Key, Opts, DefVal) ->
   proplists:get_value(Key, Opts, DefVal).

% list_to_atom(List) -> atom()
% returns existing atom if any, else new atom
%   List = List()
list_to_atom(List) ->
   try erlang:list_to_existing_atom(List) of
      Atom -> Atom
   catch
      error:badarg -> erlang:list_to_atom(List)
   end.

% returns epoc time: count of seconds since Epoc (1/1/1970)
% epoch_time -> Int()
epoch_time() ->
   {MSec, Sec, _MicroSec} = erlang:now(),
   MSec * 1000000 + Sec.

% sends a email using mailx
% send_email(To, From, Subj, Text) ->
%  To = String(),
%  Subj = String(),
%  Body = Binary()
send_email(To, Subj, Body) ->
   Cmd = io_lib:format("echo '~s' | mailx -s '~s' ~s", [Body, Subj, To]),
   Res = os:cmd(lists:flatten(Cmd)),
   case os:cmd("echo $?") of
      "0\n" ->
         ok;
      [_] ->
         {error, Res}
   end.

now_to_string(Now = {_, _, MicroSec}) ->
   {{YYYY, M, DD},{HH, MM, SS}} = calendar:now_to_datetime(Now),
   lists:flatten(io_lib:format("~2.10.0B.~2.10.0B.~4.10.0B ~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
         [DD, M, YYYY, HH, MM, SS, MicroSec div 1000])).
