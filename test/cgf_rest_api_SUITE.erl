%%% cgf_rest_api_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021-2025 SigScale Global Inc.
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
%%% Test suite for the REST API of the {@link //cgf. cgf} application.
%%%
-module(cgf_rest_api_SUITE).
-copyright('Copyright (c) 2021-2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([get_health/0, get_health/1,
		head_health/0, head_health/1]).

-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	[{timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	ok = cgf_test_lib:unload(mnesia),
	ok = cgf_test_lib:load(mnesia),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = cgf_test_lib:init_tables(),
	ok = cgf_test_lib:start(),
	Modules = [mod_cgf_rest_accepted_content,
			mod_cgf_rest_get, mod_cgf_rest_head, mod_get],
	Options = [{bind_address, {0,0,0,0}}, {port, 0},
			{server_name, atom_to_list(?MODULE)},
			{server_root, PrivDir},
			{document_root, "/"},
			{modules, Modules}],
	{ok, Httpd} = inets:start(httpd, Options),
	[{port, Port}] = httpd:info(Httpd, [port]),
	Url = "http://localhost:" ++ integer_to_list(Port),
	[{host, Url} | Config].

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = cgf_test_lib:stop().

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[get_health, head_health].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_health() ->
	[{userdata, [{doc,"Get health in REST interface"}]}].

get_health(Config) ->
	Host = ?config(host, Config),
	Accept = {"accept", "application/health+json"},
	Request = {Host ++ "/health", [Accept]},
	{ok, Result} = httpc:request(get, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, Body} = Result,
	{_, "application/health+json"} = lists:keyfind("content-type", 1, Headers),
	{ok, #{"serviceId" := _Node,
			"description" := "Health of SigScale CGF",
			"checks" := #{"uptime" := [Time]},
			"status" := "pass"}} = zj:decode(Body),
	#{"componentType" := "system",
			"observedUnit" := "s",
			"observedValue" := Uptime} = Time,
	Uptime > 0.

head_health() ->
	[{userdata, [{doc,"Get headers (only) of health in REST interface"}]}].

head_health(Config) ->
	Host = ?config(host, Config),
	Accept = {"accept", "application/health+json"},
	Request = {Host ++ "/health", [Accept]},
	{ok, Result} = httpc:request(head, Request, [], []),
	{{"HTTP/1.1", 200, _OK}, Headers, _Body} = Result,
	{_, "application/health+json"} = lists:keyfind("content-type", 1, Headers).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

