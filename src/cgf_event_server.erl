%%% cgf_event_server.erl
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
%%% @doc This {@link //stdlib/gen_server. gen_server} behaviour callback
%%% 	module provides an event manager in the
%%% 	{@link //cgf. cgf} application.
%%%
-module(cgf_event_server).
-copyright('Copyright (c) 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(gen_server).

% export the gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, handle_continue/2, code_change/3]).
% export the private api
-export([handle_copy/4, handle_move/4, handle_delete/4, handle_unzip/4,
		handle_gunzip/4, handle_untar/4]).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, 10000).

-record(state,
		{max_fsm :: non_neg_integer(),
		max_mem :: non_neg_integer(),
		load_avg :: {avg1 | avg5 | avg15, pos_integer()},
		eventq :: queue:queue()}).
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
init([] = _Args) ->
	{ok, MaxFsm} = application:get_env(max_action_import),
	{ok, MaxMem} = application:get_env(max_action_memory),
	LoadAvg = case application:get_env(max_action_load) of
		{ok, {1, N}} when is_number(N) ->
			{avg1, floor(N * 256)};
		{ok, {5, N}} when is_number(N) ->
			{avg5, floor(N * 256)};
		{ok, {15, N}} when is_number(N) ->
			{avg15, floor(N * 256)}
	end,
	State = #state{max_fsm = MaxFsm, max_mem = MaxMem,
			load_avg = LoadAvg, eventq = queue:new()},
	{ok, State, {continue, init}}.

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
handle_call({file_close = Event, Content} = _Request, _From, State) ->
	case cgf:match_event(Event, Content) of
		{ok, Actions} ->
			{Result, NewState} = start_action(Event, Content, Actions, State),
			{reply, Result, NewState, timeout(NewState)};
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content}]),
			{reply, {error, Reason}, State, timeout(State)}
	end;
handle_call({import_end = _Event, _Content} = _Request, _From, State) ->
	NewState = start_import(State),
	{reply, ok, NewState, timeout(NewState)}.

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
handle_cast(_Request, State) ->
	{noreply, State, timeout(State)}.

-spec handle_continue(Continue, State) -> Result
	when
		Continue :: term(),
		State :: state(),
		Result :: {noreply, NewState :: state()}
				| {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}}
				| {stop, Reason :: term(), NewState :: state()}.
%% @doc Handle continued execution.
handle_continue(init = _Continue, State) ->
	case cgf_event:add_sup_handler(cgf_event, []) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State};
		{'EXIT', Reason} ->
			{stop, Reason, State}
	end.

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
%% @hidden
handle_info(timeout = _Info, State) ->
	NewState = start_import(State),
	{noreply, NewState, timeout(NewState)};
handle_info({gen_event_EXIT, cgf_event = _Handler, Reason} = _Info, State) ->
	{stop, Reason, State, timeout(State)}.

-spec terminate(Reason, State) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
      State :: state().
%% @see //stdlib/gen_server:terminate/3
%% @private
terminate(_Reason, _State) ->
	ok.

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

%%----------------------------------------------------------------------
%%  The cgf_event_server private API
%%----------------------------------------------------------------------

%% @hidden
start_action(Event, #{root := Root,
		path := <<$/, Path/binary>>} = Content, Actions, State)
		when byte_size(Root) > 0 ->
	start_action(Event, Content#{path := Path}, Actions, State);
start_action(Event, #{root := Root, path := Path} = Content,
		[{_Match, {import, {Module, Log}} = _Action} | T],
		#state{eventq = EventQ} = State) ->
	Filename = filename:join(Root, Path),
	Metadata = metadata(Content, #{}),
	StartArgs = [Module, [Filename, Log, Metadata], []],
	EventQ1 = queue:in(StartArgs, EventQ),
	State1 = State#state{eventq = EventQ1},
	NewState = start_import(State1),
	start_action(Event, Content, T, NewState);
start_action(Event, #{root := Root, path := Path} = Content,
		[{_Match, {import, {Module, Log, Metadata}} = _Action} | T],
		#state{eventq = EventQ} = State) ->
	Filename = filename:join(Root, Path),
	Metadata1 = metadata(Content, Metadata),
	StartArgs = [Module, [Filename, Log, Metadata1], []],
	EventQ1 = queue:in(StartArgs, EventQ),
	State1 = State#state{eventq = EventQ1},
	NewState = start_import(State1),
	start_action(Event, Content, T, NewState);
start_action(Event, #{root := Root, path := Path} = Content,
		[{_Match, {import, {Module, Log, Metadata, ExtraArgs}} = _Action} | T],
		#state{eventq = EventQ} = State) ->
	Filename = filename:join(Root, Path),
	Metadata1 = metadata(Content, Metadata),
	StartArgs = [Module, [Filename, Log, Metadata1] ++ ExtraArgs, []],
	EventQ1 = queue:in(StartArgs, EventQ),
	State1 = State#state{eventq = EventQ1},
	NewState = start_import(State1),
	start_action(Event, Content, T, NewState);
start_action(Event, #{root := Root, path := Path} = Content,
		[{_Match, {import, {Module, Log, Metadata, ExtraArgs, Opts}} = _Action} | T],
		#state{eventq = EventQ} = State) ->
	Filename = filename:join(Root, Path),
	Metadata1 = metadata(Content, Metadata),
	StartArgs = [Module, [Filename, Log, Metadata1] ++ ExtraArgs, Opts],
	EventQ1 = queue:in(StartArgs, EventQ),
	State1 = State#state{eventq = EventQ1},
	NewState = start_import(State1),
	start_action(Event, Content, T, NewState);
start_action(Event, Content, [{Match, {copy, _} = Action} | T], State) ->
	spawn(?MODULE, handle_copy, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(Event, Content, [{Match, {move, _} = Action} | T], State) ->
	spawn(?MODULE, handle_move, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(Event, Content, [{Match, {delete, _} = Action} | T], State) ->
	spawn(?MODULE, handle_delete, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(Event, Content, [{Match, {unzip, _} = Action} | T], State) ->
	spawn(?MODULE, handle_unzip, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(Event, Content, [{Match, {gunzip, _} = Action} | T], State) ->
	spawn(?MODULE, handle_gunzip, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(Event, Content, [{Match, {untar, _} = Action} | T], State) ->
	spawn(?MODULE, handle_untar, [Event, Content, Match, Action]),
	start_action(Event, Content, T, State);
start_action(_Event, _Content, [], State) ->
	{ok, State}.

%% @hidden
start_import(#state{max_mem = MaxMem} = State) ->
	case erlang:memory(total) of
		TotalMemory when TotalMemory < MaxMem ->
			start_import1(State);
		_TotalMemory ->
			State
	end.
%% @hidden
start_import1(#state{load_avg = {F, MaxLoad}} = State) ->
	try cpu_sup:F() of
		LoadAverage when LoadAverage < MaxLoad ->
			start_import2(State);
		_LoadAverage ->
			State
	catch
		_:_ ->
			start_import2(State)
	end.
%% @hidden
start_import2(#state{max_fsm = MaxFsm, eventq = EventQ} = State) ->
	Counts = supervisor:count_children(cgf_import_sup),
	case proplists:get_value(active, Counts) of
		Active when Active < MaxFsm ->
			{{value, StartArgs}, EventQ1} = queue:out(EventQ),
			NewState = State#state{eventq = EventQ1},
			start_import3(StartArgs, NewState);
		_Active ->
			State
	end.
%% @hidden
start_import3(StartArgs, State) ->
	case supervisor:start_child(cgf_import_sup, StartArgs) of
		{ok, _Child} ->
			State;
		{ok, _Child, _Info} ->
			State;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{supervisor, cgf_import_sup},
					{start_args, StartArgs}]),
			State
	end.

%% @private
handle_copy(Event,
		#{root := Root, path := Path} = Content,
		Match, {copy, {RE, Replacement}} = Action)
		when is_binary(RE), is_binary(Replacement) ->
	Subject = filename:basename(Path),
	try re:replace(Subject, RE, Replacement, [{return, binary}]) of
		Subject ->
			ok;
		NewPath ->
			case filelib:safe_relative_path(NewPath, Root) of
				unsafe ->
					throw(unsafe_path);
				SafePath ->
					handle_copy(Event, Content, Match, Action, SafePath)
			end
	catch
		_:Reason ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_copy(Event,
		#{root := Root, user := Username,
				path := Path, stack := Stack} = Content,
		Match, Action, NewPath) ->
	Filename = filename:join(Root, Path),
	UserPath = filename:join(<<"/">>, NewPath),
	FilePath = <<Root/binary, UserPath/binary>>,
	case file:copy(Filename, FilePath) of
		{ok, _} ->
			Stack1 = [{Event, {Match, Action}} | Stack],
			EventPayload = #{module => ?MODULE,
					user => Username,
					root => Root,
					path => UserPath,
					stack => Stack1},
			cgf_event:notify(file_close, EventPayload);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.

%% @private
handle_move(Event,
		#{root := Root, path := Path} = Content,
		Match, {move, {RE, Replacement}} = Action)
		when is_binary(RE), is_binary(Replacement) ->
	Subject = filename:basename(Path),
	try re:replace(Subject, RE, Replacement, [{return, binary}]) of
		Subject ->
			ok;
		NewPath ->
			case filelib:safe_relative_path(NewPath, Root) of
				unsafe ->
					throw(unsafe_path);
				SafePath ->
					handle_move(Event, Content, Match, Action, SafePath)
			end
	catch
		_:Reason ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_move(Event,
		#{root := Root, user := Username,
				path := Path, stack := Stack} = Content,
		Match, Action, NewPath) ->
	Filename = filename:join(Root, Path),
	UserPath = filename:join(<<"/">>, NewPath),
	FilePath = <<Root/binary, UserPath/binary>>,
	case file:rename(Filename, FilePath) of
		ok ->
			Stack1 = [{Event, {Match, Action}} | Stack],
			EventPayload = #{module => ?MODULE,
					user => Username,
					root => Root,
					path => UserPath,
					stack => Stack1},
			cgf_event:notify(file_close, EventPayload);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.

%% @private
handle_delete(Event,
		#{root := Root, path := Path} = Content,
		Match, {delete, RE} = Action) ->
	Filename = filename:join(Root, Path),
	Subject = filename:basename(Path),
	try re:run(Subject, RE) of
		{match, _} ->
			case file:delete(Filename) of
				ok ->
					ok;
				{error, Reason1} ->
					?LOG_ERROR([{?MODULE, Reason1},
							{event, Event},
							{content, Content},
							{match, Match},
							{action, Action}])
			end;
		nomatch ->
			ok
	catch
		_:Reason2 ->
			?LOG_ERROR([{?MODULE, Reason2},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.

%% @private
handle_unzip(Event,
		#{root := Root, path := Path} = Content,
		Match, {unzip, {RE, Replacement}} = Action)
		when is_binary(RE), is_binary(Replacement) ->
	Subject = filename:basename(Path),
	try re:replace(Subject, RE, Replacement, [{return, binary}]) of
		Subject ->
			ok;
		NewPath ->
			case filelib:safe_relative_path(NewPath, Root) of
				unsafe ->
					throw(unsafe_path);
				SafePath ->
					handle_unzip(Event, Content, Match, Action, SafePath)
			end
	catch
		_:Reason ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_unzip(Event,
		#{root := Root, user := Username,
				path := Path, stack := Stack} = Content,
		Match, Action, NewPath) ->
	Filename = binary_to_list(filename:join(Root, Path)),
	UserPath = filename:join(<<"/">>, NewPath),
	DirPath = <<Root/binary, UserPath/binary>>,
	Options = [{cwd, binary_to_list(DirPath)}],
	case zip:unzip(Filename, Options) of
		{ok, Files} ->
			F = fun(File) ->
					case string:prefix(File, binary_to_list(Root)) of
						UnzipPath when is_list(UnzipPath) ->
							Stack1 = [{Event, {Match, Action}} | Stack],
							EventPayload = #{module => ?MODULE,
									user => Username,
									root => Root,
									path => filename:join(<<"/">>, UnzipPath),
									stack => Stack1},
							cgf_event:notify(file_close, EventPayload)
					end
			end,
			lists:foreach(F, Files);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.

%% @private
handle_gunzip(Event,
		#{root := Root, path := Path} = Content,
		Match, {gunzip, {RE, Replacement}} = Action)
		when is_binary(RE), is_binary(Replacement) ->
	Subject = filename:basename(Path),
	try re:replace(Subject, RE, Replacement, [{return, binary}]) of
		Subject ->
			ok;
		NewPath ->
			case filelib:safe_relative_path(NewPath, Root) of
				unsafe ->
					throw(unsafe_path);
				SafePath ->
					handle_gunzip(Event, Content, Match, Action, SafePath)
			end
	catch
		_:Reason ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_gunzip(Event,
		#{root := Root, path := Path} = Content,
		Match, Action, NewPath) ->
	Filename = binary_to_list(filename:join(Root, Path)),
	Options = [read, binary, compressed],
	handle_gunzip(Event, Content, Match, Action, NewPath,
			file:open(Filename, Options)).
%% @hidden
handle_gunzip(Event,
		#{root := Root} = Content,
		Match, Action, NewPath, {ok, IoDevice1}) ->
	UserPath = filename:join(<<"/">>, NewPath),
	DirPath = <<Root/binary, UserPath/binary>>,
	Filename = binary_to_list(DirPath),
	Options = [write, binary],
	handle_gunzip(Event, Content, Match, Action, UserPath,
			IoDevice1, file:open(Filename, Options));
handle_gunzip(Event, Content, Match, Action, _NewPath, {error, Reason}) ->
	?LOG_ERROR([{?MODULE, Reason},
			{event, Event},
			{content, Content},
			{match, Match},
			{action, Action}]).
%% @hidden
handle_gunzip(Event,
		#{root := Root, user := Username, stack := Stack} = Content,
		Match, Action, UserPath, IoDevice1, {ok, IoDevice2}) ->
	case file:copy(IoDevice1, IoDevice2) of
		{ok, _BytesCopied} ->
			Stack1 = [{Event, {Match, Action}} | Stack],
			EventPayload = #{module => ?MODULE,
					user => Username,
					root => Root,
					path => UserPath,
					stack => Stack1},
			cgf_event:notify(file_close, EventPayload);
		{error, Reason} ->
			file:close(IoDevice1),
			file:close(IoDevice2),
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end;
handle_gunzip(Event, Content, Match, Action, _UserPath,
		IoDevice1, {error, Reason}) ->
	file:close(IoDevice1),
	?LOG_ERROR([{?MODULE, Reason},
			{event, Event},
			{content, Content},
			{match, Match},
			{action, Action}]).

%% @private
handle_untar(Event,
		#{root := Root, path := Path} = Content,
		Match, {untar, {RE, Replacement}} = Action)
		when is_binary(RE), is_binary(Replacement) ->
	Subject = filename:basename(Path),
	try re:replace(Subject, RE, Replacement, [{return, binary}]) of
		Subject ->
			ok;
		NewPath ->
			case filelib:safe_relative_path(NewPath, Root) of
				unsafe ->
					throw(unsafe_path);
				SafePath ->
					UserPath = filename:join(<<"/">>, SafePath),
					handle_untar(Event, Content, Match, Action, UserPath)
			end
	catch
		_:Reason ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_untar(Event,
		#{root := Root, path := Path} = Content,
		Match, Action, UserPath) ->
	Filename = binary_to_list(filename:join(Root, Path)),
	Options = [read, binary, compressed],
	case file:open(Filename, Options) of
		{ok, File} ->
			handle_untar(Event, Content, Match, Action,
					UserPath, Filename, Options, File);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_untar(Event, Content, Match, Action,
		UserPath, Filename, FileOptions, File) ->
	Options = [verbose],
	case erl_tar:table({file, File}, Options) of
		{ok, TarEntries} ->
			ok = file:close(File),
			F = fun({Name, regular, _, _, _, _, _}) ->
						{true, Name};
					(_) ->
						false
			end,
			Files = lists:filtermap(F, TarEntries),
			handle_untar1(Event, Content, Match, Action,
					UserPath, Filename, FileOptions, Files);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_untar1(Event, Content, Match, Action,
		UserPath, Filename, Options, Files) ->
	case file:open(Filename, Options) of
		{ok, File} ->
			handle_untar2(Event, Content, Match, Action,
					UserPath, File, Files);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.
%% @hidden
handle_untar2(Event,
		#{root := Root, user := Username, stack := Stack} = Content,
		Match, Action, UserPath, File, Files) ->
	DirPath = <<Root/binary, UserPath/binary>>,
	Options = [{files, Files}, {cwd, binary_to_list(DirPath)}],
	case erl_tar:extract({file, File}, Options) of
		ok ->
			ok = file:close(File),
			F = fun(Filename) ->
					FilePath = filename:join(UserPath, Filename),
					Stack1 = [{Event, {Match, Action}} | Stack],
					EventPayload = #{module => ?MODULE,
							user => Username,
							root => Root,
							path => FilePath,
							stack => Stack1},
					cgf_event:notify(file_close, EventPayload)
			end,
			lists:foreach(F, Files);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, Reason},
					{event, Event},
					{content, Content},
					{match, Match},
					{action, Action}])
	end.

%% @hidden
metadata(#{root := Root, path := Path,
		user := User} = _Content, Metadata) ->
	Filename = filename:join(Root, Path),
	FileMap = #{"name" => filename:basename(Path),
			"directory" => filename:dirname(Path),
			"path" => Filename},
	UserMap = #{"name" => User},
	Log = #{"file" => FileMap, "user" => UserMap},
	F = fun("file", MetadataValue, ContentValue) ->
				maps:merge(MetadataValue, ContentValue);
			("user", MetadataValue, ContentValue) ->
				maps:merge(MetadataValue, ContentValue);
			(_Key, MetadataValue, _ContentValue) ->
				MetadataValue
	end,
	maps:merge_with(F, Metadata, #{"log" => Log}).

%% @hidden
timeout(#state{eventq = EventQ} = _State) ->
	case queue:is_empty(EventQ) of
		true ->
			infinity;
		false ->
			?TIMEOUT
	end.

