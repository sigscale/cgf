%%% cgf_lib_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021-2023 SigScale Global Inc.
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
%%% Test suite for the public API of the {@link //cgf. cgf} application.
%%%
-module(cgf_lib_SUITE).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([octets/0, octets/1,
		bcd/0, bcd/1]).

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
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok.

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

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[octets, bcd].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

octets() ->
	[{userdata, [{doc, "Convert an OCTET STRING to hexadecimal"}]}].

octets(_Config) ->
	Size = rand:uniform(64),
	Octets = rand:bytes(Size),
	<<Integer:(Size * 8)>> = Octets,
	HexString = cgf_lib:octet_string(Octets),
	HexString = iolist_to_binary(io_lib:fwrite("~*.16.0b", [Size * 2, Integer])).

bcd() ->
	[{userdata, [{doc, "Decode a BCD directory number (DN)"}]}].

bcd(_Config) ->
	TON = rand:uniform(4) - 1,
	NPI = rand:uniform(4) - 1,
	NA = #{0 => <<"unknown">>, 1 => <<"international">>,
			2 => <<"national">>},
	NP = #{0 => <<"unknown">>, 1 => <<"e164">>, 3 => <<"x121">>,
			8 => <<"national">>, 9 => <<"private">>},
	DN = list_to_binary(cgf_test_lib:rand_dn(rand:uniform(82))),
	F = fun F(<<D1, D2, Rest/binary>>, Acc) ->
				N1 = list_to_integer([D1]),
				N2 = list_to_integer([D2]),
				F(Rest, <<Acc/binary, N2:4, N1:4>>);
			F(<<D>>, Acc) ->
				N = list_to_integer([D]),
				<<Acc/binary, 2#1111:4, N:4>>;
			F(<<>>, Acc) ->
				Acc
	end,
	BCD = F(DN, <<>>),
	PartyAddress = cgf_lib:bcd_dn(<<1:1, TON:3, NPI:4, BCD/binary>>),
	NatureOfAddress = maps:get(<<"natureOfAddress">>, PartyAddress),
	NatureOfAddress = maps:get(TON, NA, <<"reserved">>),
	NumberingPlan = maps:get(<<"numberingPlan">>, PartyAddress),
	NumberingPlan = maps:get(NPI, NP, <<"reserved">>),
	DN = maps:get(<<"address">>, PartyAddress).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

