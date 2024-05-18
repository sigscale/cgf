%%% cgf_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%%   module starts and stops the {@link //cgf. cgf} application.
%%%
-module(cgf_app).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the cse private API for installation
-export([install/0, install/1]).

-type state() :: #{}.

%%----------------------------------------------------------------------
%%  The cgf_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {ok, pid()} | {ok, pid(), State} | {error, Reason},
		State :: state(),
		Reason :: term().
%% @doc Starts the application processes.
start(normal = _StartType, _Args) ->
	case supervisor:start_link({local, cgf_sup}, cgf_sup, []) of
		{ok, TopSup} ->
			{ok, Logs} = application:get_env(logs),
			start1(TopSup, Logs);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start1(TopSup, [{LogName, Options} | T]) ->
	case cgf_log:open(LogName, Options) of
		ok ->
			start1(TopSup, T);
		{error, Reason} ->
			{error, Reason}
	end;
start1(TopSup, []) ->
	SystemDir = case application:get_env(ssh, system_dir) of
		{ok, Path1} ->
			Path1;
		undefined ->
			"ssh/system"
	end,
	PathComponents = filename:split(SystemDir),
	BaseDir = lists:droplast(PathComponents),
	undefined = application:get_env(ssh, user_dir),
	UserDir = filename:join([BaseDir, "user"]),
	UserDirFun = case application:get_env(ssh, user_dir_fun) of
		{ok, Fun} ->
			Fun;
		undefined ->
			fun(Username) -> filename:join([UserDir, Username]) end
	end,
	{ok, SubSystems} = application:get_env(sftpd),
	start2(TopSup, SystemDir, UserDirFun, SubSystems).
%% @hidden
start2(TopSup, SystemDir, UserDirFun,
		[{Address, Port, DaemonOptions, SftpdOptions} | T]) ->
	Options1 = case proplists:lookup(system_dir, DaemonOptions) of
		{system_dir, _SystemDir1} ->
			DaemonOptions;
		none ->
			[{system_dir, SystemDir} | DaemonOptions]
	end,
	none = proplists:lookup(user_dir, Options1),
	Options2 = case proplists:lookup(user_dir_fun, Options1) of
		{user_dir_fun, _UserDirFun1} ->
			Options1;
		none ->
			[{user_dir_fun, UserDirFun} | Options1]
	end,
	Spec = cgf_sftpd:subsystem_spec(SftpdOptions),
	Options3 = [{subsystems, [Spec]} | Options2], 
	case ssh:daemon(Address, Port, Options3) of
		{ok, _DaemonRef} ->
			start2(TopSup, SystemDir, UserDirFun, T);
		{error, Reason} ->
			{error, Reason}
	end;
start2(TopSup, _SystemDir, _UserDirFun, []) ->
	{ok, TopSup}.

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%%   applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> state()
	when
		State :: state().
%% @doc Called when the application is about to be shut down,
%%   before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State) -> any()
	when
		State :: state().
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code replacement, if there are any
%%   changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

-spec install() -> Result
	when
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @doc Initialize CGF tables.
%% 	`Nodes' is a list of the nodes where
%% 	{@link //cgf. cgf} tables will be replicated.
%%
%% 	If {@link //mnesia. mnesia} is not running an attempt
%% 	will be made to create a schema on all available nodes.
%% 	If a schema already exists on any node
%% 	{@link //mnesia. mnesia} will be started on all nodes
%% 	using the existing schema.
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	{ok, []}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

