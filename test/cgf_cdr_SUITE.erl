%%% cgf_cdr_SUITE.erl
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
%%% Test suite for CDR parsing in
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_cdr_SUITE).
-copyright('Copyright (c) 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([import_cs/0, import_cs/1]).

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
	Description = "Test suite for CDR parsing in the cgf application.",
	[{userdata, [{doc, Description}]}, {timetrap, {minutes, 1}}].

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
	Config.

-spec end_per_suite(Config) -> Result
	when
		Config :: ct_suite:ct_config(),
		Result :: term() | {save_config, Config}.
%% @doc Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok.

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
	[import_cs].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

import_cs() ->
	Description = "Import a circuit switched (CS) CDR file.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

import_cs(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	Log = bx_cs,
	FileHeader = file_header(),
	FileHeader1 = FileHeader#cdr_file_header{count = 1},
	FileHeaderB = cgf_3gpp_file:file_header(FileHeader1),
	FileHeaderL = byte_size(FileHeaderB),
	CdrHeader = cdr_header(32250),
	CdrHeaderB = cgf_3gpp_file:cdr_header(CdrHeader),
	CdrHeaderL = byte_size(CdrHeaderB),
	{ok, CDR} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	CdrLength = byte_size(CDR),
	FileLength = 8 + FileHeaderL + CdrHeaderL + 2 + CdrLength,
	FileB = <<FileLength:32, FileHeaderL:32, FileHeaderB/binary,
			CdrLength:16, CdrHeaderB/binary, CDR/binary>>,
	FilePath = filename:join(PrivDir, cdr_filename(1)),
	ok = file:write_file(FilePath, FileB),
	% ok = cgf_cs:import(FilePath, Log).
	% implement the below in cgf_cs:import/2,3
	{ok, B} = file:read_file(FilePath),
	<<FileLength:32, FileHeaderL:32, FileHeaderB:FileHeaderL/binary,
			CdrLength:16, CdrHeader:3/binary, CDR:CdrLength/binary>> = B,
	'CSChargingDataTypes':decode('CSRecord', CDR).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

file_header() ->
	Release = 18,
	Version = 1,
	Low = {Release, Version},
	High = {Release, Version},
	Open = timestamp_32297(),
	Append = timestamp_32297(),
	Count = rand:uniform(16#fffffffe),
	Sequence = rand:uniform(16#ffffffff) - 1,
	CloseReason = normal,
	Address = cgf_test_lib:rand_ipv6(),
	Lost = {'==', 0},
	#cdr_file_header{low = Low, high = High,
			open = Open, append = Append, count = Count,
			sequence = Sequence, reason = CloseReason,
			address = Address, lost = Lost}.

cdr_header(TS) ->
	Release = 18,
	Version = 1,
	#cdr_header{ts = TS, release = Release, version = Version,
			format = ber}.

timestamp_32297() ->
	timestamp_32297(calendar:local_time()).
timestamp_32297(LocalTime) ->
	[UTC | _] = calendar:local_time_to_universal_time_dst(LocalTime),
	LocalSeconds = calendar:datetime_to_gregorian_seconds(LocalTime),
	UTCSeconds = calendar:datetime_to_gregorian_seconds(UTC),
	Offset = (LocalSeconds - UTCSeconds) div 60,
	{{_Year, Month, Day}, {Hour, Minute, _Second}} = LocalTime,
	{Month, Day, Hour, Minute, Offset}.


mo_call_record() ->
	MSISDN = cgf_test_lib:rand_dn(),
	IMSI = cgf_test_lib:rand_imsi(),
	IMEI = cgf_test_lib:rand_imei(),
	TON = 2,
	NPI = 1,
	CallingPartyBCD = cgf_lib:tbcd(MSISDN),
	CallingPartyAddress = <<1:1, TON:3, NPI:4, CallingPartyBCD/binary>>,
	CalledPartyDN = cgf_test_lib:rand_dn(),
	CalledPartyBCD = cgf_lib:tbcd(CalledPartyDN),
	CalledPartyAddress = <<1:1, TON:3, NPI:4, CalledPartyBCD/binary>>,
	MscAddressDN = cgf_test_lib:rand_dn(),
	MscAddressBCD = cgf_lib:tbcd(MscAddressDN),
	MscAddress = <<1:1, TON:3, NPI:4, MscAddressBCD/binary>>,
	LocalTime = calendar:local_time(),
	LocalSeconds = calendar:datetime_to_gregorian_seconds(LocalTime),
	Duration = cgf_test_lib:rand_duration(),
	StartSeconds = LocalSeconds - Duration,
	StartTime = calendar:gregorian_seconds_to_datetime(StartSeconds),
	CallReference = binary:encode_unsigned(rand:uniform(4294967295)),
	MOCallRecord = #{recordType => moCallRecord,
			servedIMSI => cgf_lib:tbcd(IMSI),
			servedIMEI => cgf_lib:tbcd(IMEI),
			servedMSISDN => cgf_lib:tbcd(MSISDN),
			callingNumber => CallingPartyAddress,
			calledNumber => CalledPartyAddress,
			recordingEntity => MscAddress,
			mscOutgoingTKGP => {tkgpNumber, rand:uniform(99)},
			basicService => {teleservice, <<17>>},
			answerTime => bcd_date_time(StartTime),
			releaseTime => bcd_date_time(LocalTime),
			callDuration => Duration,
			causeForTerm => normalRelease,
			callReference => CallReference},
	{moCallRecord, MOCallRecord}.

bcd_date_time(LocalTime) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = LocalTime,
	Offset = element(5, timestamp_32297(LocalTime)),
	Year1 = (Year - 2000) rem 10,
	Year2 = (Year - 2000) div 10,
	Month1 = Month rem 10, 
	Month2 = Month div 10, 
	Day1 = Day rem 10,
	Day2 = Day div 10,
	Hour1 = Hour rem 10,
	Hour2 = Hour div 10,
	Minute1 = Minute rem 10,
	Minute2 = Minute div 10,
	Second1 = Second rem 10,
	Second2 = Second div 10,
	{Sign, Tz} = case Offset of
		N when N < 0 ->
			{$-, N * -1};
		N ->
			{$+, N}
	end,
	TzHour = Tz div 60,
	TzMinute = Tz rem 60,
	TzHour1 = TzHour rem 10,
	TzHour2 = TzHour div 10,
	TzMinute1 = TzMinute rem 10,
	TzMinute2 = TzMinute div 10,
	<<Year2:4, Year1:4, Month2:4, Month1:4, Day2:4, Day1:4,
			Hour2:4, Hour1:4, Minute2:4, Minute1:4, Second2:4, Second1:4,
      Sign:8, TzHour2:4, TzHour1:4, TzMinute2:4, TzMinute1:4>>.

cdr_filename(RC) ->
	NodeID = lists:concat(["CGF", node()]),
	LocalTime = calendar:local_time(),
	{{Year, _, _}, _} = LocalTime,  
	{Month, Day, Hour, Minute, Offset} = timestamp_32297(LocalTime),
	{Sign, Tz} = case Offset of
		N when N < 0 ->
			{$-, N * -1};
		N ->
			{$+, N}
	end,
	Date = io_lib:fwrite("~4.10.0b~2.10.0b~2.10.0b", [Year, Month, Day]),
	Time = io_lib:fwrite("~2.10.0b~2.10.0b~c~2.10.0b~2.10.0b",
			[Hour, Minute, Sign, Tz div 60, Tz rem 60]),
	lists:concat([NodeID, "_-_", RC, ".", Date, "_-_", Time]).

