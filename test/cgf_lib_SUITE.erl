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
-export([suite/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([file_header/0, file_header/1,
		cdr_header/0, cdr_header/1,
		octets/0, octets/1,
		bcd/0, bcd/1]).

-include_lib("cgf/include/cgf_3gpp_file.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	Description = "Test suite for the public API of the cgf application.",
   [{userdata, [{doc, Description}]}, {timetrap, {minutes, 1}}].

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

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[file_header, cdr_header, octets, bcd].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

file_header() ->
	Description = "CDR file header CODEC (3GPP TS 32.297)",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

file_header(_Config) ->
	Low = {release(), rand:uniform(10) - 1},
	High = {release(), rand:uniform(10) - 1},
	Open = cdr_timestamp(),
	Append = cdr_timestamp(),
	Count = rand:uniform(16#fffffffe),
	Sequence = rand:uniform(16#ffffffff) - 1,
	CloseReason = close_reason(),
	Address = cgf_test_lib:rand_ipv6(),
	Lost = cdr_lost(),
	Header = #cdr_file_header{low = Low, high = High,
			open = Open, append = Append, count = Count,
			sequence = Sequence, reason = CloseReason,
			address = Address, lost = Lost},
	Binary = cgf_3gpp_file:file_header(Header),
	Header = cgf_3gpp_file:file_header(Binary).

cdr_header() ->
	Description = "CDR event record header CODEC (3GPP TS 32.297)",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

cdr_header(_Config) ->
	TS = ts_number(),
	Release = release(),
	Version = rand:uniform(10) - 1,
	Format = cdr_format(),
	Header = #cdr_header{ts = TS, release = Release,
			version = Version, format = Format},
	Binary = cgf_3gpp_file:cdr_header(Header),
	Header = cgf_3gpp_file:cdr_header(Binary).

octets() ->
	Description = "Convert an OCTET STRING to hexadecimal",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

octets(_Config) ->
	Size = rand:uniform(64),
	Octets = rand:bytes(Size),
	<<Integer:(Size * 8)>> = Octets,
	HexString = cgf_lib:octet_string(Octets),
	HexString = iolist_to_binary(io_lib:fwrite("~*.16.0b", [Size * 2, Integer])).

bcd() ->
	Description = "Decode a BCD directory number (DN)",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

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

cdr_timestamp() ->
	{rand:uniform(12), rand:uniform(28), rand:uniform(24),
			rand:uniform(60) - 1, (rand:uniform(27) - 13) * 60}.

close_reason() ->
	close_reason(rand:uniform(10)).
close_reason(1) ->
	normal;
close_reason(2) ->
	size;
close_reason(3) ->
	time;
close_reason(4) ->
	count;
close_reason(5) ->
	manual;
close_reason(6) ->
	change;
close_reason(7) ->
	undefined;
close_reason(8) ->
	error;
close_reason(9) ->
	space;
close_reason(10) ->
	integrity.

cdr_lost() ->
	cdr_lost(rand:uniform(256) - 1).
cdr_lost(0) ->
	{'==', 0};
cdr_lost(N) when N =< 127 ->
	{'>=', N};
cdr_lost(128) ->
	{'>=', 1};
cdr_lost(N) when N < 255 ->
	{'==', N - 128};
cdr_lost(255) ->
	{'>=', 127}.

cdr_format() ->
	cdr_format(rand:uniform(4)).
cdr_format(1) ->
	ber;
cdr_format(2) ->
	uper;
cdr_format(3) ->
	per;
cdr_format(4) ->
	xer.

ts_number() ->
	ts_number(rand:uniform(26) - 1).
ts_number(0) ->
	32005;
ts_number(1) ->
	32015;
ts_number(2) ->
	32205;
ts_number(3) ->
	32215;
ts_number(4) ->
	32225;
ts_number(5) ->
	32235;
ts_number(6) ->
	32250;
ts_number(7) ->
	32251;
ts_number(9) ->
	32260;
ts_number(10) ->
	32270;
ts_number(11) ->
	32271;
ts_number(12) ->
	32272;
ts_number(13) ->
	32273;
ts_number(14) ->
	32275;
ts_number(15) ->
	32274;
ts_number(16) ->
	32277;
ts_number(17) ->
	32296;
ts_number(18) ->
	32278;
ts_number(19) ->
	32253;
ts_number(20) ->
	32255;
ts_number(21) ->
	32254;
ts_number(22) ->
	32256;
ts_number(23) ->
	28201;
ts_number(24) ->
	28202;
ts_number(25) ->
	32257.

release() ->
	release(rand:uniform(18)).
release(N) when N < 4 ->
	99;
release(N) ->
	N.

