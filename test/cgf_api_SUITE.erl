%%% cgf_api_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021 - 2025 SigScale Global Inc.
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
%%% Test suite for the public API of
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_api_SUITE).
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([add_action/0, add_action/1,
		find_action/0, find_action/1,
		get_action/0, get_action/1,
		get_actions/0, get_actions/1,
		delete_action/0, delete_action/1,
		match_event/0, match_event/1]).

-include_lib("common_test/include/ct.hrl").
-include("cgf_3gpp_file.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> [Info]
	when
		Info :: ct_suite:ct_info().
%% @doc  Require variables and set default values for the suite.
%%
suite() ->
	Description = "Test suite for the public API of the cgf application.",
	[{userdata, [{doc, Description}]},
			{timetrap, {minutes, 1}}].

-spec init_per_suite(Config) -> NewConfig
	when
		Config :: ct_suite:ct_config(),
		NewConfig :: ct_suite:ct_config()
				| {skip, Reason}
				| {skip_and_save, Reason, Config},
		Reason :: term().
%% @doc Initialization before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	ok = cgf_test_lib:unload(mnesia),
	ok = cgf_test_lib:load(mnesia),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = cgf_test_lib:init_tables(),
	ok = cgf_test_lib:start(),
	Config.

-spec end_per_suite(Config) -> Result
	when
		Config :: ct_suite:ct_config(),
		Result :: term() | {save_config, Config}.
%% @doc Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	cgf_test_lib:stop().

-spec init_per_testcase(TestCase, Config) -> NewConfig
	when
		TestCase :: ct_suite:ct_testname(),
		Config :: ct_suite:ct_config(),
		NewConfig :: ct_suite:ct_config()
				| {fail, Reason}
				| {skip, Reason},
		Reason :: term().
%% Initialization before each test case.
%%
init_per_testcase(_TestCase, Config) ->
   Config.

-spec end_per_testcase(TestCase, Config) -> Result
	when
		TestCase :: ct_suite:ct_testname(),
		Config :: ct_suite:ct_config(),
		Result :: term()
				| {fail, Reason}
				| {save_config, Config},
		Reason :: term().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec all() -> Result
	when
		Result :: [TestDef] | {skip, Reason},
		TestDef :: ct_suite:ct_test_def(),
		Reason :: term().
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[add_action, find_action, get_action, get_actions,
			delete_action, match_event].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

add_action() ->
	Description = "Add an event action to the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

add_action(_Config) ->
	Event = file_close,
	Match = rand_match(),
	Action = rand_action(),
	ok = cgf:add_action(Event, Match, Action).

find_action() ->
	Description = "Find an event action in the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

find_action(_Config) ->
	Event = file_close,
	Match = rand_match(),
	Action = rand_action(),
	ok = cgf:add_action(Event, Match, Action),
	{ok, Action} = cgf:find_action(Event, Match).

get_action() ->
	Description = "Get an event action from the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

get_action(_Config) ->
	Event = file_close,
	Match = rand_match(),
	Action = rand_action(),
	ok = cgf:add_action(Event, Match, Action),
	Action = cgf:get_action(Event, Match).

get_actions() ->
	Description = "Get all event actions in the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

get_actions(_Config) ->
	Event = file_close,
	Count = rand:uniform(1000),
	Fadd = fun F(0) ->
				ok;
			F(N) ->
				Match = rand_match(),
				Action = rand_action(),
				ok = cgf:add_action(Event, Match, Action),
				F(N - 1)
	end,
	Fadd(Count),
	Actions = cgf:get_actions(Event),
	true = length(Actions) >= Count.

delete_action() ->
	Description = "Delete an event action from the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

delete_action(_Config) ->
	Event = file_close,
	Match = rand_match(),
	Action = rand_action(),
	ok = cgf:add_action(Event, Match, Action),
	ok = cgf:delete_action(Event, Match).

match_event() ->
	Description = "Match an event to action(s) in the table.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

match_event(_Config) ->
	Event = file_close,
	Count = rand:uniform(1000),
	Fadd = fun F(0) ->
				ok;
			F(N) ->
				Match = rand_match(),
				Action = rand_action(),
				cgf:add_action(Event, Match, Action),
				F(N - 1)
	end,
	Fadd(Count),
	Actions = cgf:get_actions(Event),
	{Match, _Action} = lists:nth(rand:uniform(Count), Actions),
	{UserPrefix, DirPrefix, FilePrefix, SuffPrefix} = Match,
	UserSuffix = list_to_binary(rand_chars()),
	User = <<UserPrefix/binary, UserSuffix/binary>>,
	DirSuffix = list_to_binary(rand_chars()),
	Directory = <<DirPrefix/binary, DirSuffix/binary>>,
	FileSuffix = list_to_binary(rand_chars()),
	Filename = <<FilePrefix/binary, FileSuffix/binary>>,
	Suffix = case byte_size(SuffPrefix) of
		0 ->
			<<>>;
		_N ->
			<<$., SuffPrefix/binary>>
	end,
	Content = #{module => cgf_sftpd,
			user => User,
			root => <<"/tmp">>,
			path => <<Directory/binary, $/, Filename/binary, Suffix/binary>>},
	{ok, _} = cgf:match_event(Event, Content).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

%% @doc Returns random `Match'.
rand_match() ->
	User = rand_chars(),
	Directory = rand_chars(),
	Filename = rand_chars(),
	Suffix = rand_suffix(),
	{User, Directory, Filename, Suffix}.

%% @doc Returns random `Action'.
rand_action() ->
	Actions = [import, copy, move, delete],
	Action = lists:nth(rand:uniform(length(Actions)), Actions),
	rand_action(Action).

rand_action(import) ->
	{import, rand_import()};
rand_action(copy) ->
	RE = "^([A-Z]{5})",
	Replacement = "&.bak",
	{copy, {RE, Replacement}};
rand_action(move) ->
	RE = "^[0-9]{8}-MSC-",
	Replacement = "MSC/&",
	{move, {RE, Replacement}};
rand_action(delete) ->
	RE = "^[A-Z0-9]{6,8}.tmp$",
	{delete, RE}.

%% @doc Returns random `Import'.
rand_import() ->
	Arity = rand:uniform(4) + 1,
	rand_import(Arity).

rand_import(2) ->
	Module = rand_mod(),
	Log = log_name(Module),
	{Module, Log};
rand_import(3) ->
	Module = rand_mod(),
	Log = log_name(Module),
	Metadata = #{foo => 42},
	{Module, Log, Metadata};
rand_import(4) ->
	Module = rand_mod(),
	Log = log_name(Module),
	Metadata = #{foo => 42},
	ExtraArgs = [bar],
	{Module, Log, Metadata, ExtraArgs};
rand_import(5) ->
	Module = rand_mod(),
	Log = log_name(Module),
	Metadata = #{foo => 42},
	ExtraArgs = [bar],
	Opts = [{debug, [trace]}],
	{Module, Log, Metadata, ExtraArgs, Opts}.

%% @doc Returns random characters.
rand_chars() ->
	N = rand:uniform(20) - 1,
	rand_chars(N).

%% @doc Returns N random characters.
rand_chars(N) ->
	Charset = [$a, $A, $b, $B, $c, $C, $d, $D, $e, $E, $f, $F,
			$g, $G, $h, $H, $i, $I, $j, $J, $k, $K, $l, $L, $m, $M,
			$n, $N, $o, $O, $p, $P, $q, $Q, $r, $R, $s, $S, $t, $T,
			$u, $U, $v, $V, $w, $W, $x, $X, $y, $Y, $z, $Z, $0, $1,
			$2, $3, $4, $5, $6, $7, $8, $9, $ , $#, $%, $(, $), $+,
			$,, $-, $., $:, $;, $=, $_, ${, $}, $~],
	rand_chars(Charset, N, []).
rand_chars(_Charset, 0, Acc) ->
	Acc;
rand_chars(Charset, N, Acc) ->
	Char = lists:nth(rand:uniform(length(Charset)), Charset),
	rand_chars(Charset, N - 1, [Char | Acc]).

%% @doc Return a random suffix match value.
rand_suffix() ->
	Suffixes = [[], "ber", "csv", "cdr"],
	lists:nth(rand:uniform(length(Suffixes)), Suffixes).

%% @doc Return a random import module.
rand_mod() ->
	Modules = [cgf_cs, cgf_gprs, cgf_ims],
	lists:nth(rand:uniform(length(Modules)), Modules).

%% @doc Return a log name.
log_name(cgf_cs) ->
	bx_cs;
log_name(cgf_gprs) ->
	bx_ps;
log_name(cgf_ims) ->
	bx_ims;
log_name(_) ->
	bx_cdr.

