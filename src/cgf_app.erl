%%% cgf_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 - 2025 SigScale Global Inc.
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
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the cgf private API for installation
-export([install/0, install/1, join/1]).

-include_lib("kernel/include/logger.hrl").

-type state() :: #{}.

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 60000).

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
	Tables = [cgf_action],
	{ok, Wait} = application:get_env(wait_tables),
	case mnesia:wait_for_tables(Tables, Wait) of
		ok ->
			start1();
		{timeout, BadTables} ->
			case force(BadTables) of
				ok ->
					?LOG_WARNING([{?MODULE, "Force loaded mnesia tables"},
							{tables, BadTables}]),
					start1();
				{error, Reason} ->
					?LOG_ERROR([{?MODULE, "Failed to force load mnesia tables"},
							{tables, BadTables}, {reason, Reason}]),
					{error, Reason}
			end;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, "Failed to load mnesia tables"},
					{tables, Tables}, {reason, Reason}]),
			{error, Reason}
	end.
%% @hidden
start1() ->
	case supervisor:start_link({local, cgf_sup}, cgf_sup, []) of
		{ok, TopSup} ->
			{ok, Logs} = application:get_env(logs),
			start2(TopSup, Logs);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start2(TopSup, [{LogName, Options} | T]) ->
	case cgf_log:open(LogName, Options) of
		ok ->
			start2(TopSup, T);
		{error, Reason} ->
			{error, Reason}
	end;
start2(TopSup, []) ->
	SystemDir = case application:get_env(ssh, system_dir) of
		{ok, Path1} ->
			Path1;
		undefined ->
			"ssh/system"
	end,
	PathComponents = filename:split(SystemDir),
	BaseDir = lists:droplast(PathComponents),
	undefined = application:get_env(ssh, user_dir),
	UserDir = filename:join(BaseDir ++ ["user"]),
	UserDirFun = case application:get_env(ssh, user_dir_fun) of
		{ok, Fun} ->
			Fun;
		undefined ->
			fun(Username) -> filename:join([UserDir, Username]) end
	end,
	{ok, SubSystems} = application:get_env(sftpd),
	start3(TopSup, SystemDir, UserDirFun, SubSystems).
%% @hidden
start3(TopSup, SystemDir, UserDirFun,
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
			start3(TopSup, SystemDir, UserDirFun, T);
		{error, Reason} ->
			{error, Reason}
	end;
start3(TopSup, _SystemDir, _UserDirFun, []) ->
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
	case mnesia:system_info(is_running) of
		no ->
			case mnesia:create_schema(Nodes) of
				ok ->
					?LOG_INFO([{?MODULE, "Created mnesia schema"},
							{nodes, Nodes}]),
					install1(Nodes);
				{error, {_, {already_exists, _}}} ->
					?LOG_INFO([{?MODULE, "Existing mnesia schema"},
						{nodes, Nodes}]),
					install1(Nodes);
				{error, Reason} ->
					?LOG_ERROR([{?MODULE, "Failed to create mnesia schema"},
							{description, mnesia:error_description(Reason)},
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes)
	end.
%% @hidden
install1([Node] = Nodes) when Node == node() ->
	case mnesia:start() of
		ok ->
			?LOG_INFO([{?MODULE, "Started mnesia"}]),
			install2(Nodes);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, "Failed to start mnesia"},
					{description, mnesia:error_description(Reason)},
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes) ->
	case rpc:multicall(Nodes, mnesia, start, [], 60000) of
		{Results, []} ->
			F = fun(ok) ->
						false;
					(_) ->
						true
			end,
			case lists:filter(F, Results) of
				[] ->
					?LOG_INFO([{?MODULE, "Started mnesia on all nodes"},
							{nodes, Nodes}]),
					install2(Nodes);
				NotOKs ->
					?LOG_ERROR([{?MODULE, "Failed to start mnesia on all nodes"},
							{nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} ->
			?LOG_ERROR([{?MODULE, "Failed to start mnesia on all nodes"},
					{nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, {Results, BadNodes}}
	end.
%% @hidden
install2(Nodes) ->
	Wait = case application:get_env(cgf, wait_tables) of
		{ok, Wait1} ->
			Wait1;
		undefined ->
			ok = application:load(cgf),
			{ok, Wait1} = application:get_env(cgf, wait_tables),
			Wait1
	end,
	case mnesia:wait_for_tables([schema], Wait) of
		ok ->
			install3(Nodes, []);
		{timeout, _BadTables} ->
			?LOG_ERROR([{?MODULE, "Timeout waiting for mnesia schema table"}]),
			{error, timeout};
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, "Failed waiting for mnesia schema table"},
					{description, mnesia:error_description(Reason)},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case create_table(cgf_action, Nodes) of
		ok ->
			install4(Nodes, [cgf_action | Acc]);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Tables) ->
	{ok, Wait} = application:get_env(cgf, wait_tables),
	case mnesia:wait_for_tables(Tables, Wait) of
		ok ->
			install5(Nodes, Tables);
		{timeout, Tables} ->
			?LOG_ERROR([{?MODULE, "Timeout waiting for mnesia tables"},
					{tables, Tables}]),
			{error, timeout};
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, "Failed waiting for mnesia tables"},
					{description, mnesia:error_description(Reason)},
					{tables, Tables},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install5(_Nodes, Tables) ->
	{ok, Tables}.

-spec join(Node) -> Result
	when
		Node :: atom(),
		Result :: {ok, Tables} | {error, Reason},
		Tables :: [atom()],
		Reason :: term().
%% @doc Join an existing cluster.
%%
%% 	Tables will be copied from the given `Node'.
%%
join(Node) when is_atom(Node)  ->
	case mnesia:system_info(is_running) of
		no ->
			join1(Node);
		Running ->
			error_logger:error_report(["mnesia running", {is_running, Running}]),
			{error, mnesia_running}
	end.
%% @hidden
join1(Node) ->
	case net_kernel:connect_node(Node) of
		true ->
			join2(Node);
		Connect ->
			error_logger:error_report(["Failed to connect node",
					{result, Connect}]),
			{error, Connect}
	end.
%% @hidden
join2(Node) ->
	case rpc:call(Node, mnesia, add_table_copy, [schema, node(), ram_copies]) of
		{atomic, ok} ->
			join3(Node);
		{aborted, {already_exists, schema, _}} ->
			error_logger:info_msg("Found existing schema table on ~s.~n", [Node]),
			join3(Node);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join3(Node) ->
	case application:start(mnesia) of
		ok ->
			join4(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join4(Node) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, _Nodes} ->
			join5(Node);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join5(Node) ->
	case mnesia:change_table_copy_type(schema, node(), disc_copies) of
		{atomic, ok} ->
			error_logger:info_msg("Copied schema table from ~s.~n", [Node]),
			join6(Node, mnesia:system_info(db_nodes), [schema]);
		{aborted, {already_exists, schema, _, disc_copies}} ->
			error_logger:info_msg("Found existing schema table on ~s.~n", [Node]),
			join6(Node, mnesia:system_info(db_nodes), [schema]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join6(Node, Nodes, Acc) ->
	case rpc:call(Node, mnesia, add_table_copy, [cgf_action, node(), disc_copies]) of
		{atomic, ok} ->
			error_logger:info_msg("Copied cgf_action table from ~s.~n", [Node]),
			join7(Node, Nodes, [cgf_action | Acc]);
		{aborted, {already_exists, cgf_action, _}} ->
			error_logger:info_msg("Found existing cgf_action table on ~s.~n", [Node]),
			join7(Node, Nodes, [cgf_action | Acc]);
		{aborted, {no_exists, {cgf_action, _}}} ->
			case create_table(cgf_action, Nodes) of
				ok ->
					join7(Node, Nodes, [cgf_action | Acc]);
				{error, Reason} ->
					{error, Reason}
			end;
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
join7(_Node, _Nodes, Tables) ->
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			{ok, Tables};
		{timeout, BadTables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, BadTables}]),
			{error, timeout};
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec force(Tables) -> Result
	when
		Tables :: [TableName],
		Result :: ok | {error, Reason},
		TableName :: atom(),
		Reason :: term().
%% @doc Try to force load bad tables.
%% @private
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
	end;
force([]) ->
	ok.

-spec create_table(Table, Nodes) -> Result
	when
		Table :: atom(),
		Nodes :: [node()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Create mnesia table.
%% @private
create_table(cgf_action, Nodes) when is_list(Nodes) ->
	create_table1(cgf_action, mnesia:create_table(cgf_action,
			[{type, bag}, {disc_copies, Nodes},
			{user_properties, [{cgf, true}]}])).
%% @hidden
create_table1(Table, {atomic, ok}) ->
	?LOG_INFO([{?MODULE, "Created new mnesia table"},
			{table, Table}]),
	ok;
create_table1(Table, {aborted, {already_exists, Table}}) ->
	?LOG_INFO([{?MODULE, "Found existing mnesia table"},
			{table, Table}]),
	ok;
create_table1(_Table, {aborted, {not_active, _, Node} = Reason}) ->
	?LOG_ERROR([{?MODULE, "Mnesia not started on node"},
			{node, Node}]),
	{error, Reason};
create_table1(Table, {aborted, Reason}) ->
	?LOG_ERROR([{?MODULE, "Failed to create mnesia table"},
			{table, Table},
			{description, mnesia:error_description(Reason)},
			{error, Reason}]),
	{error, Reason}.

