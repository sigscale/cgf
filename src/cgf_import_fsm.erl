%%% cgf_import_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements an event action handler behaviour within the
%%% 	{@link //cgf. cgf} application.
%%%
%%% 	This module provides a CDR file importing behaviour. Specific CDR
%%% 	file type handlers are implemented as callback modules behaving to
%%% 	this behaviour module. This module behaves to
%%% 	{@link //stdlib/gen_statem. gen_statem}.
%%%
%%% 	==Usage==
%%% 	The callback module should declare a `{@module}' behaviour
%%% 	module attribute:
%%% 	```
%%% 	-behaviour({@module}).
%%% 	'''
%%% 	The call back module handles reading the CDR file, parsing the
%%% 	charging data records and logging.
%%%
%%% 	== State Transitions ==
%%% 	The following diagram depicts the states, and events which drive
%%% 	state transitions, in the finite state machine (FSM):
%%%
%%% 	<img alt="state machine" src="import-fsm.svg" />
%%%
%%% 	<h2><a name="callbacks">Callback Functions</a></h2>
%%% 	The following callback functions are used.
%%%
%%% 	<h3 class="function">
%%% 		<a name="init">init/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>init(Args) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Args = [Filename, Log]
%%% 					| [Filename, Log, Metadata]
%%% 					| [Filename, Log, Metadata, ...]</tt></li>
%%% 			<li><tt>Result = {ok, StateData} | {error, Reason}</tt></li>
%%% 			<li><tt>StateData = term()</tt></li>
%%% 			<li><tt>Reason = term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Called to initialize the import handler.
%%%
%%% 	See {@link //cgf/cgf:add_action/3. cgf:add_action/3}.
%%%
%%% 	<h3 class="function">
%%% 		<a name="open">open/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>open(Filename, StateData) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Filename :: file:filename() | binary()</tt></li>
%%% 			<li><tt>StateData = term()</tt></li>
%%% 			<li><tt>Result = {continue, Cont, StateData}
%%% 					| {error, Reason}</tt></li>
%%% 			<li><tt>Cont = any()</tt></li>
%%% 			<li><tt>Reason = term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Called once, directly after initialization. It should open
%%% 	the CDR file for reading. On success returns `Cont', an opaque
%%% 	continuation, typically either a file handle or data.
%%%
%%% 	<h3 class="function">
%%% 		<a name="read">read/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>read(Cont, StateData) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Cont = any()</tt></li>
%%% 			<li><tt>StateData = term()</tt></li>
%%% 			<li><tt>Result = {continue, CDR, Cont, StateData}
%%% 					| {error, Reason, Cont, StateData}
%%% 					| {close, Reason Cont, StateData}
%%% 					| {stop, Reason, StateData}</tt></li>
%%% 			<li><tt>Reason = normal | shutdown | term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Called to read the next `CDR' from the file.
%%%
%%% 	<h3 class="function">
%%% 		<a name="parse">parse/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>parse(CDR, Log, StateData) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>CDR = term()</tt></li>
%%% 			<li><tt>Log = disk_log:log()</tt></li>
%%% 			<li><tt>StateData = term()</tt></li>
%%% 			<li><tt>Result = {continue, StateData}
%%% 					| {error, Reason, StateData}
%%% 					| {close, Reason, StateData}
%%% 					| {stop, Reason, StateData}</tt></li>
%%% 			<li><tt>Reason = normal | shutdown | term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Called to parse and log a CDR.
%%%
%%% 	<h3 class="function">
%%% 		<a name="close">close/2</a>
%%% 	</h3>
%%% 	<div class="spec">
%%% 		<p>
%%% 			<tt>close(Cont, StateData) -&gt; Result</tt>
%%% 		</p>
%%% 		<ul class="definitions">
%%% 			<li><tt>Cont = any()</tt></li>
%%% 			<li><tt>StateData = term()</tt></li>
%%% 			<li><tt>Result = {stop, StateData}
%%% 					| {stop, Report, StateData}
%%% 					| {error, Reason, StateData}</tt></li>
%%% 			<li><tt>Report = map()</tt></li>
%%% 			<li><tt>Reason = normal | shutdown | term()</tt></li>
%%% 		</ul>
%%% 	</div>
%%% 	Called to close a CDR File.
%%%
-module(cgf_import_fsm).
-copyright('Copyright (c) 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_statem).

%% export the public api
-export([start/3, start_link/3]).
%% export the callbacks needed for gen_statem behaviour
-export([init/1, handle_event/4, callback_mode/0,
			terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([open/3, read/3, parse/3, close/3]).

-include_lib("kernel/include/logger.hrl").

-callback init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateData} | {error, Reason},
		StateData :: term(),
		Reason :: term().
-callback open(Filename, StateData) -> Result
	when
		Filename :: file:filename() | binary(),
		StateData :: term(),
		Result :: {continue, Cont, StateData}
				| {error, Reason},
		Cont :: any(),
		Reason :: term().
-callback read(Cont, StateData) -> Result
	when
		Cont :: any(),
		StateData :: term(),
		Result :: {continue, CDR, Cont, StateData}
				| {error, Reason, Cont, StateData}
				| {close, Reason, Cont, StateData}
				| {stop, Reason, StateData},
		CDR :: term(),
		Reason :: normal | shutdown | term().
-callback parse(CDR, Log, StateData) -> Result
	when
		CDR :: term(),
		Log :: disk_log:log(),
		StateData :: term(),
		Result :: {continue, StateData}
				| {error, Reason, StateData}
				| {close, Reason, StateData}
				| {stop, Reason, StateData},
		Reason :: normal | shutdown | term().
-callback close(Cont, StateData) -> Result
	when
		Cont :: any(),
		StateData :: term(),
		Result :: {stop, StateData}
				| {stop, Report, StateData}
				| {error, Reason, StateData},
		Report :: map(),
		Reason :: normal | shutdown | term().

-type state() :: open | read | parse | close.

-type statedata() ::
		#{module := erlang:module(),
		filename := file:filename() | binary(),
		log := disk_log:log(),
		metadata :=  map(),
		import_log := disk_log:log(),
		start := pos_integer(),
		args => [term()],
		cb_statedata => term(),
		cont => file:io_device() | binary() | list() | map()}.

%%----------------------------------------------------------------------
%%  The cgf_import_fsm public API
%%----------------------------------------------------------------------

-spec start(Module, Args, Opts) -> Result
	when
		Module :: erlang:module(),
		Args :: [term()],
		Opts :: [gen_statem:start_opt()],
		Result :: gen_statem:start_ret().
%% @doc Start a {@module} behaviour process.
%%
%% 	The callback `Module' is initialized with `Args':
%% 	```
%% 	[Filename, Log]
%% 			| [Filename, Log, Metadata]
%% 			| [Filename, Log, Metadata | ExtraArgs]
%% 	'''
start(Module, Args, Opts)
		when is_atom(Module), is_list(Args),
		length(Args) >= 2, is_list(Opts) ->
	gen_statem:start(?MODULE, [Module | Args], Opts).

-spec start_link(Module, Args, Opts) -> Result
	when
		Module :: erlang:module(),
		Args :: [term()],
		Opts :: [gen_statem:start_opt()],
		Result :: gen_statem:start_ret().
%% @doc Start a supervised {@module} behaviour process.
%%
%% 	The callback `Module' is initialized with `Args':
%% 	```
%% 	[Filename, Log]
%% 			| [Filename, Log, Metadata]
%% 			| [Filename, Log, Metadata | ExtraArgs]
%% 	'''
start_link(Module, Args, Opts)
		when is_atom(Module), is_list(Args),
		length(Args) >= 2, is_list(Opts) ->
	gen_statem:start_link(?MODULE, [Module | Args], Opts).

%%----------------------------------------------------------------------
%%  The cgf_import_fsm gen_statem callbacks
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions, state_enter].

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State, Data} | {ok, State, Data, Actions}
				| ignore | {stop, Reason},
		State :: state(),
		Data :: statedata(),
		Actions :: Action | [Action],
		Action :: gen_statem:action(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%%
%% 	Initialize an event action handler instance.
%%
%% @see //stdlib/gen_statem:init/1
%% @private
init([Module, Filename, Log])
		when is_atom(Module),
		(is_list(Filename) orelse is_binary(Filename)) ->
	{ok, ImportLog} = application:get_env(import_log),
	Data = #{module => Module,
			filename => Filename,
			log => Log,
			metadata => #{},
			import_log => ImportLog,
			start => erlang:system_time(millisecond)},
	case Module:init([Filename, Log]) of
		{ok, CbStateData} ->
			NewData = Data#{cb_statedata => CbStateData},
			{ok, open, NewData};
		{error, Reason} ->
			?LOG_ERROR([{Module, Reason},
					{filename, Filename},
					{log, Log}]),
			{stop, Reason}
	end;
init([Module, Filename, Log, Metadata])
		when is_atom(Module),
		(is_list(Filename) orelse is_binary(Filename)),
		is_map(Metadata) ->
	{ok, ImportLog} = application:get_env(import_log),
	Data = #{module => Module,
			filename => Filename,
			log => Log,
			metadata => Metadata,
			import_log => ImportLog,
			start => erlang:system_time(millisecond)},
	case Module:init([Filename, Log, Metadata]) of
		{ok, CbStateData} ->
			NewData = Data#{cb_statedata => CbStateData},
			{ok, open, NewData};
		{error, Reason} ->
			?LOG_ERROR([{Module, Reason},
					{filename, Filename},
					{log, Log}]),
			{stop, Reason}
	end;
init([Module, Filename, Log, Metadata | ExtraArgs])
		when is_atom(Module),
		(is_list(Filename) orelse is_binary(Filename)),
		is_map(Metadata), is_list(ExtraArgs) ->
	{ok, ImportLog} = application:get_env(import_log),
	Data = #{module => Module,
			filename => Filename,
			log => Log,
			metadata => Metadata,
			import_log => ImportLog,
			start => erlang:system_time(millisecond),
			extra_args => ExtraArgs},
	case Module:init([Filename, Log, Metadata | ExtraArgs]) of
		{ok, CbStateData} ->
			NewData = Data#{cb_statedata => CbStateData},
			{ok, open, NewData};
		{error, Reason} ->
			?LOG_ERROR([{Module, Reason},
					{filename, Filename},
					{log, Log}]),
			{stop, Reason}
	end.

-spec open(EventType, EventContent, Data) -> Result
	when
		EventType :: enter | gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>open</em> state.
%% @private
open(enter = _EventType, open = _EventContent, _Data) ->
	Action = {state_timeout, 0, internal},
	{keep_state_and_data, Action};
open(state_timeout = _EventType, internal = _EventContent,
		#{module := Module,
				filename := Filename, log := Log,
				cb_statedata := CbStateData} = Data) ->
	case Module:open(Filename, CbStateData) of
		{continue, Cont, CbStateData1} ->
			log_open(<<"success">>, Data),
			NewData = Data#{cont => Cont,
					cb_statedata => CbStateData1},
			{next_state, read, NewData};
		{error, Reason} ->
			log_open(<<"failure">>, Data),
			?LOG_ERROR([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			{stop, Reason}
	end.

-spec read(EventType, EventContent, Data) -> Result
	when
		EventType :: enter | gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>read</em> state.
%% @private
read(enter = _EventType, _EventContent, _Data) ->
	Action = {state_timeout, 0, internal},
	{keep_state_and_data, Action};
read(state_timeout = _EventType, internal = _EventContent,
		#{module := Module, cont := Cont,
				filename := Filename, log := Log,
				cb_statedata := CbStateData} = Data) ->
	case Module:read(Cont, CbStateData) of
		{continue, CDR, Cont1, CbStateData1} ->
			NewData = Data#{cont => Cont1,
					cb_statedata => CbStateData1},
			Action = {next_event, internal, CDR},
			{next_state, parse, NewData, Action};
		{error, Reason, Cont1, CbStateData1} ->
			?LOG_WARNING([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			NewData = Data#{cont => Cont1,
					cb_statedata => CbStateData1},
			{next_state, read, NewData};
		{close, Reason, Cont1, CbStateData1} ->
			NewData = Data#{cont => Cont1,
					cb_statedata => CbStateData1},
			Action = {next_event, internal, Reason},
			{next_state, close, NewData, Action};
		{stop, Reason, CbStateData1} ->
			?LOG_ERROR([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			NewData = Data#{cb_statedata => CbStateData1},
			{stop, Reason, NewData}
	end.

-spec parse(EventType, EventContent, Data) -> Result
	when
		EventType :: enter | gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>parse</em> state.
%% @private
parse(enter = _EventType, _EventContent, _Data) ->
	keep_state_and_data;
parse(internal = _EventType, CDR = _EventContent,
		#{module := Module,
				filename := Filename, log := Log,
				cb_statedata := CbStateData} = Data) ->
	case Module:parse(CDR, Log, CbStateData) of
		{continue, CbStateData1} ->
			NewData = Data#{cb_statedata => CbStateData1},
			{next_state, read, NewData};
		{error, Reason, CbStateData1} ->
			?LOG_WARNING([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			NewData = Data#{cb_statedata => CbStateData1},
			{next_state, read, NewData};
		{close, Reason, CbStateData1} ->
			NewData = Data#{cb_statedata => CbStateData1},
			Action = {next_event, internal, Reason},
			{next_state, close, NewData, Action};
		{stop, Reason, CbStateData1} ->
			?LOG_ERROR([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			NewData = Data#{cb_statedata => CbStateData1},
			{stop, Reason, NewData}
	end.

-spec close(EventType, EventContent, Data) -> Result
	when
		EventType :: enter | gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>close</em> state.
%% @private
close(enter = _EventType, _EventContent, _Data) ->
	keep_state_and_data;
close(internal = _EventType, Reason = _EventContent,
		#{module := Module, cont := Cont,
				filename := Filename, log := Log,
				cb_statedata := CbStateData} = Data) ->
	case Module:close(Cont, CbStateData) of
		{stop, CbStateData1} ->
			Data1 = maps:remove(cont, Data),
			NewData = Data1#{cb_statedata => CbStateData1},
			log_close(Reason, #{}, NewData),
			{stop, Reason, NewData};
		{stop, Report, CbStateData1} ->
			Data1 = maps:remove(cont, Data),
			NewData = Data1#{cb_statedata => CbStateData1},
			log_close(Reason, Report, NewData),
			{stop, Reason, NewData};
		{error, Reason1, CbStateData1} ->
			?LOG_WARNING([{Module, Reason},
					{state, ?FUNCTION_NAME},
					{filename, Filename},
					{log, Log}]),
			Data1 = maps:remove(cont, Data),
			NewData = Data1#{cb_statedata => CbStateData1},
			log_close(Reason1, #{}, NewData),
			{stop, Reason1, NewData}
	end.

-spec handle_event(EventType, EventContent, State, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(State).
%% @doc Handles events received in any state.
%% @private
%%
handle_event(EventType, EventContent, _State, Data) ->
	{stop, {EventType, EventContent}, Data}.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec log_open(Outcome, Data) -> ok
	when
		Outcome :: binary(),
		Data :: statedata().
%% @doc Log after file open.
%% @hidden
log_open(Outcome,
		#{module := Module, log := BxLog, import_log := ImportLog,
				start := Start, metadata := Metadata} = _Data) ->
	MetadataLog = maps:get("log", Metadata, #{}),
	Metadata1 = maps:with(["file", "user"], MetadataLog),
	Event = #{"outcome" => Outcome},
	Process = #{"entity_id" => pid_to_list(self()),
			"start" => cgf_log:iso8601(Start)},
	Log = #{"logger" => atom_to_list(BxLog)},
	ImportCDR = #{"moduleName" => atom_to_list(Module)},
	OpenEvent = Metadata1#{"event" => Event,
			"process" => Process,
			"log" => Log,
			"Import_CDR" => ImportCDR},
	cgf_log:blog(ImportLog, OpenEvent).

-spec log_close(Reason, Report, Data) -> ok
	when
		Reason :: normal | shutdown | term(),
		Report :: map(),
		Data :: statedata().
%% @doc Log after file close.
%% @hidden
log_close(Reason, Report, Data)
		when Reason == normal; Reason == shutdown ->
	log_close1(<<"success">>, Report, Data);
log_close(_Reason, Report, Data) ->
	log_close1(<<"failure">>, Report, Data).
%% @hidden
log_close1(Outcome, Report,
		#{module := Module, log := BxLog, import_log := ImportLog,
				start := Start, metadata := Metadata} = _Data)
		when is_map(Report) ->
	MetadataLog = maps:get("log", Metadata, #{}),
	Metadata1 = maps:with(["file", "user"], MetadataLog),
	Event = #{"outcome" => Outcome},
	Process = #{"entity_id" => pid_to_list(self()),
			"start" => cgf_log:iso8601(Start),
			"stop" => cgf_log:now()},
	Log = #{"logger" => atom_to_list(BxLog)},
	ImportCDR = Report#{"moduleName" => atom_to_list(Module)},
	CloseEvent = Metadata1#{"event" => Event,
			"process" => Process,
			"log" => Log,
			"Import_CDR" => ImportCDR},
	cgf_log:blog(ImportLog, CloseEvent).

