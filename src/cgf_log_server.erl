%%% cgf_log_server.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021-2024 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module provides an event manager for a named log in the
%%% 	{@link //cgf. cgf} application.
%%%
-module(cgf_log_server).
-copyright('Copyright (c) 2021-2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_server).

% export the gen_server call backs
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, handle_continue/2,
		code_change/3, format_status/2]).
% export the cgf_log_server private api
-export([codec_worker/5]).

-include_lib("kernel/include/logger.hrl").

-record(state,
		{sup :: pid(),
		job_sup :: pid() | undefined,
		log :: disk_log:log(),
		codec :: {Module :: atom(), Function :: atom()} | undefined,
		process = false :: boolean(),
		format = internal :: internal | external,
		dopts :: [tuple()] | undefined}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The gen_server callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State :: state()}
				| {ok, State :: state(), Timeout :: timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term()} | ignore.
%% @see //stdlib/gen_server:init/1
%% @private
init([Sup | Options]) ->
	{ok, LogDir} = application:get_env(log_dir),
	State = #state{sup = Sup},
	{State1, Options1} = case lists:keytake(codec, 1, Options) of
		{value, {codec, {M, F}}, O1} when is_atom(M), is_atom(F) ->
			{State#state{codec = {M, F}}, O1};
		false ->
			{State, Options}
	end,
	{State2, Options2} = case lists:keytake(process, 1, Options1) of
		{value, {process, Process}, O2} when is_boolean(Process) ->
			{State1#state{process = Process}, O2};
		false ->
			{State1, Options1}
	end,
	Options3 = case lists:keytake(file, 1, Options2) of
		{value, {file, File}, O3} when is_list(File) ->
			case filename:pathtype(File) of
				absolute ->
					Options2;
				relative ->
					[{file, filename:join(LogDir, File)} | O3]
			end;
		false ->
			case lists:keyfind(name, 1, Options2) of
				{_, Name} when is_atom(Name) ->
					File = filename:join(LogDir, atom_to_list(Name)),
					[{file, File} | Options2];
				{_, Name} when is_list(Name) ->
					File = filename:join(LogDir, Name),
					[{file, File} | Options2]
			end
	end,
	State3 = case lists:keyfind(format, 1, Options3) of
		{format, Format} ->
			State2#state{format = Format};
		false ->
			State2
	end,
	case disk_log:open(Options3) of
		{ok, Log} ->
			process_flag(trap_exit, true),
			State4 = State3#state{log = Log, dopts = Options3},
			{ok, State4, {continue, init}};
		{repaired, Log, {recovered, Rec}, {badbytes, Bad}} ->
			?LOG_WARNING([{?MODULE, init}, {disk_log, Log},
					{recovered, Rec}, {badbytes, Bad}]),
			process_flag(trap_exit, true),
			State4 = State3#state{log = Log, dopts = Options3},
			{ok, State4, {continue, init}};
		{error, Reason} ->
			{stop, Reason}
	end.

-spec handle_call(Request, From, State) -> Result
	when
		Request :: term(),
		From :: {pid(), Tag :: any()},
		State :: state(),
		Result :: {reply, Reply :: term(), NewState :: state()}
				| {reply, Reply :: term(), NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), Reply :: term(), NewState :: state()}
				| {stop, Reason :: term(), NewState :: state()}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
handle_call({log, Term} = _Request, _From,
		#state{process = false, format = internal,
		log = Log, codec = undefined} = State) ->
	case disk_log:log(Log, Term) of
		ok ->
			{reply, ok, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call({blog, Bytes}, _From,
		#state{process = false, format = external,
		log = Log, codec = undefined} = State) ->
	case disk_log:blog(Log, [Bytes | [$\r, $\n]]) of
		ok ->
			{reply, ok, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call({log, Term}, _From,
		#state{process = false, format = internal,
		log = Log, codec = {Module, Function}} = State) ->
	case catch Module:Function(Term) of
		{'EXIT', Reason} ->
			{reply, {error, Reason}, State};
		LogItem ->
			case disk_log:log(Log, LogItem) of
				ok ->
					{reply, ok, State};
				{error, Reason} ->
					{reply, {error, Reason}, State}
			end
	end;
handle_call({blog, Term}, _From,
		#state{process = false, format = external,
		log = Log, codec = {Module, Function}} = State) ->
	case catch Module:Function(Term) of
		{'EXIT', Reason} ->
			{reply, {error, Reason}, State};
		Bytes ->
			case disk_log:blog(Log, [Bytes | [$\r, $\n]]) of
				ok ->
					{reply, ok, State};
				{error, Reason} ->
					{reply, {error, Reason}, State}
			end
	end;
handle_call({Operation, Term}, _From,
		#state{process = true, job_sup = Sup,
		log = Log, codec = {Module, Function}} = State) ->
	case supervisor:start_child(Sup,
			[[Log, Operation, Module, Function, Term]]) of
		{ok, _Child} ->
			{reply, ok, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call(supervisor, _From, #state{sup = Supervisor} = State) ->
	{reply, {ok, Supervisor}, State}.

-spec handle_cast(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @@see //stdlib/gen_server:handle_cast/2
%% @private
handle_cast({alog, Term} = _Request,
		#state{process = false, format = internal,
		log = Log, codec = undefined} = State) ->
	disk_log:alog(Log, Term),
	{noreply, State};
handle_cast({balog, Bytes},
		#state{process = false, format = external,
		log = Log, codec = undefined} = State) ->
	disk_log:balog(Log, [Bytes | [$\r, $\n]]),
	{noreply, State};
handle_cast({alog, Term},
		#state{process = false, format = internal,
		log = Log, codec = {Module, Function}} = State) ->
	case catch Module:Function(Term) of
		{'EXIT', _Reason} ->
			{noreply, State};
		LogTerm ->
			disk_log:alog(Log, LogTerm),
			{noreply, State}
	end;
handle_cast({balog, Term},
		#state{process = false, format = external,
		log = Log, codec = {Module, Function}} = State) ->
	case catch Module:Function(Term) of
		{'EXIT', _Reason} ->
			{noreply, State};
		Bytes ->
			disk_log:balog(Log, [Bytes | [$\r, $\n]]),
			{noreply, State}
	end;
handle_cast({Operation, Term},
		#state{process = true, job_sup = Sup,
		log = Log, codec = {Module, Function}} = State) ->
	supervisor:start_child(Sup,
			[[Log, Operation, Module, Function, Term]]),
	{noreply, State}.

-spec handle_continue(Continue, State) -> Result
	when
		Continue :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle continued execution.
handle_continue(init, #state{sup = Sup} = State) ->
	Children = supervisor:which_children(Sup),
	{_, JobSup, _, _} = lists:keyfind(cgf_log_job_sup, 1, Children),
	{noreply, State#state{job_sup = JobSup}}.

-spec handle_info(Info, State) -> Result
	when
		Info :: timeout | term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle a received message.
%% @@see //stdlib/gen_server:handle_info/2
%% @private
handle_info(Info, State) ->
	{stop, Info, State}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
%% @see //stdlib/gen_server:terminate/3
%% @private
terminate(_Reason, #state{log = Log} = _State) ->
	terminate1(Log, disk_log:sync(Log)).
%% @hidden
terminate1(Log, ok) ->
	terminate2(Log, disk_log:close(Log));
terminate1(Log, {error, Reason}) ->
	terminate2(Log, {error, Reason}).
%% @hidden
terminate2(_Log, ok) ->
	ok;
terminate2(Log, {error, Reason}) ->
	Descr = lists:flatten(disk_log:format_error(Reason)),
	Trunc = lists:sublist(Descr, length(Descr) - 1),
	?LOG_ERROR([{?MODULE, terminate}, {disk_log, Log},
			{description, Trunc}, {error, Reason}]).

-spec code_change(OldVersion, State, Extra) -> Result
	when
		OldVersion :: term() | {down, term()},
		State :: state(),
		Extra :: term(),
		Result :: {ok, NewState :: state()} | {error, Reason :: term()}.
%% @see //stdlib/gen_server:code_change/3
%% @private
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

-spec format_status(Opt, StatusData) -> Status
	when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().
%% @see //stdlib/gen_server:format_status/3
%% @private
format_status(_Opt, [_PDict, State] = _StatusData) ->
	[{data, [{"State", State}]}].

%%----------------------------------------------------------------------
%%  The cgf_log_server private API
%%----------------------------------------------------------------------

-spec codec_worker(Log, Operation, Module, Function, Term) -> Result
	when
		Log :: disk_log:log(),
		Operation :: log | blog | alog | balog,
		Module :: atom(),
		Function :: atom(),
		Term :: term(),
		Result :: {stop, normal} | {error, Reason},
		Reason :: term().
%% @doc Child worker of the `cgf_log_job_sup' supervisor.
%% @private
codec_worker(Log, Operation, Module, Function, Term)
		when Operation == log; Operation == alog ->
	proc_lib:init_ack({ok, self()}),
	LogTerm = Module:Function(Term),
	case disk_log:Operation(Log, LogTerm) of
		ok ->
			exit(normal);
		{error, Reason} ->
			exit(Reason)
	end;
codec_worker(Log, Operation, Module, Function, Term)
		when Operation == blog; Operation == balog ->
	proc_lib:init_ack({ok, self()}),
	Bytes = Module:Function(Term),
	case disk_log:Operation(Log, [Bytes | [$\r, $\n]]) of
		ok ->
			exit(normal);
		{error, Reason} ->
			exit(Reason)
	end.
		
%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

