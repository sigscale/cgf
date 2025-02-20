%%% cgf_cdr_SUITE.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
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
-export([import_cs/0, import_cs/1,
		import_cs_32297/0, import_cs_32297/1,
		sftp_cs/0, sftp_cs/1,
		import_mt_call/0, import_mt_call/1,
		import_mo_sms/0, import_mo_sms/1,
		file_close_copy/0, file_close_copy/1,
		file_close_move/0, file_close_move/1,
		file_close_delete/0, file_close_delete/1]).

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
	[{userdata, [{doc, Description}]},
			{require, cgf_log},
			{default_config, cgf_log,
					[{logs,
							[{bx_tap,
									[{file, "Bx_TAP"},
									{codec, {cgf_log_codec_ecs, bx}},
									{format, external},
									{type, wrap},
									{size, {1048576, 10}}]},
							{bx_ps,
									[{file, "Bx_PS"},
									{codec, {cgf_log_codec_ecs, bx}},
									{format, external},
									{type, wrap},
									{size, {1048576, 10}}]},
							{bx_cs,
									[{file, "Bx_CS"},
									{codec, {cgf_log_codec_ecs, bx}},
									{format, external},
									{type, wrap},
									{size, {1048576, 10}}]},
							{bx_ims,
									[{file, "Bx_IMS"},
									{codec, {cgf_log_codec_ecs, bx}},
									{format, external},
									{type, wrap},
									{size, {1048576, 10}}]}]}]},
			{require, cgf_ssh},
			{default_config, cgf_ssh,
					[{system_dir, "ssh/system"},
					{server_options,
							[{shell, disabled},
							{exec, disabled},
							{subsystems, []}]}]},
			{require, cgf_sftp},
			{default_config, cgf_sftp,
					[{sftp, {127,0,0,1}},
					{user, "ct"}]},
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
	DataDir = proplists:get_value(data_dir, Config),
	PrivDir = proplists:get_value(priv_dir, Config),
	ok = cgf_test_lib:unload(mnesia),
	ok = cgf_test_lib:load(mnesia),
	ok = application:set_env(mnesia, dir, PrivDir),
	ok = cgf_test_lib:init_tables(),
	ok = cgf_test_lib:unload(cgf),
	ok = cgf_test_lib:load(cgf),
	LogDir = ct:get_config({cgf_log, log_dir}, PrivDir),
	ok = application:set_env(cgf, bx_log_dir, LogDir),
	Logs = ct:get_config({cgf_log, logs}, []),
	ok = application:set_env(cgf, logs, Logs),
	ok = cgf_test_lib:unload(ssh),
	ok = cgf_test_lib:load(ssh),
	SshSystemDir = filename:join([DataDir, ssh, system]),
	ok = application:set_env(ssh, system_dir, SshSystemDir),
	SshServerOptions = ct:get_config({cgf_ssh, server_options}, []),
	ok = application:set_env(ssh, server_options, SshServerOptions),
	SftpdRoot = filename:join(PrivDir, sftpd),
	ok = file:make_dir(SftpdRoot),
	SshUser = ct:get_config({cgf_sftp, user}, "ct"),
	SshUserDir = filename:join([DataDir, ssh, user, SshUser]),
	SftpdUserDir = filename:join(SftpdRoot, SshUser),
	ok = file:make_dir(SftpdUserDir),
	SftpdOptions = [{root, SftpdRoot}],
	SftpdAddress = ct:get_config({cfg_sftp, sftp}, {127,0,0,1}),
	SftpdPort = ct:get_config({cfg_sftp, port}, rand:uniform(16384) + 49151),
	ok = application:set_env(cgf, sftpd,
			[{SftpdAddress, SftpdPort, [], SftpdOptions}]),
	ok = cgf_test_lib:start(),
	[{sftpd_port, SftpdPort}, {sftpd_user_dir, SftpdUserDir},
			{ssh_user, SshUser}, {ssh_user_dir, SshUserDir} | Config].

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
init_per_testcase(TestCase, Config)
		when TestCase == sftp_cs ->
	Port = proplists:get_value(sftpd_port, Config),
	User = proplists:get_value(ssh_user, Config),
	UserDir = proplists:get_value(ssh_user_dir, Config),
	SshOptions = [{port, Port}, {user, User}, {user_dir, UserDir},
			{silently_accept_hosts, true}, {save_accepted_host, true},
			{auth_methods, "publickey"}],
	{ok, Handle} = ct_ssh:connect(cgf_sftp, sftp, SshOptions),
	LogOptions = [{format, external},
			{codec, {cgf_log_codec_ecs, bx}}],
	ok = cgf_log:open(TestCase, LogOptions),
	[{handle, Handle} | Config];
init_per_testcase(TestCase, Config)
		when TestCase == file_close_copy;
		TestCase == file_close_move;
		TestCase == file_close_delete ->
	Port = proplists:get_value(sftpd_port, Config),
	User = proplists:get_value(ssh_user, Config),
	UserDir = proplists:get_value(ssh_user_dir, Config),
	SshOptions = [{port, Port}, {user, User}, {user_dir, UserDir},
			{silently_accept_hosts, true}, {save_accepted_host, true},
			{auth_methods, "publickey"}],
	{ok, Handle} = ct_ssh:connect(cgf_sftp, sftp, SshOptions),
	[{handle, Handle} | Config];
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
end_per_testcase(sftp_cs = TestCase, _Config) ->
	ok = cgf_log:close(TestCase);
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
	[import_cs, import_cs_32297, sftp_cs, import_mt_call,
		import_mo_sms,
		file_close_copy, file_close_move, file_close_delete].

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
	{ok, CDR1} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	{ok, CDR2} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	{ok, CDR3} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	FilePath = filename:join(PrivDir, cdr_filename(1)),
	ok = file:write_file(FilePath, <<CDR1/binary, CDR2/binary, CDR3/binary>>),
	ok = cgf_cs:import(FilePath, Log).

import_cs_32297() ->
	Description = "Import a CS CDR file in 3GPP 32.297 format.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

import_cs_32297(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	Log = bx_cs,
	FileHeader = file_header(),
	FileHeader1 = FileHeader#cdr_file_header{count = 1},
	FileHeaderB = cgf_3gpp_file:file_header(FileHeader1),
	FileHeaderL = byte_size(FileHeaderB) + 8,
	CdrHeader = cdr_header(32250),
	CdrHeaderB = cgf_3gpp_file:cdr_header(CdrHeader),
	CdrHeaderL = byte_size(CdrHeaderB) + 2,
	{ok, CDR} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	CdrLength = byte_size(CDR),
	FileLength = FileHeaderL + CdrHeaderL + CdrLength,
	FileB = <<FileLength:32, FileHeaderL:32, FileHeaderB/binary,
			CdrLength:16, CdrHeaderB/binary, CDR/binary>>,
	FilePath = filename:join(PrivDir, cdr_filename(1)),
	ok = file:write_file(FilePath, FileB),
	ok = cgf_cs:import(FilePath, Log).

sftp_cs() ->
	Description = "End-to-end test of SFTP to Bx (CS).",
	ct:comment(Description),
	[{userdata, [{doc, Description}]},
			{require, cgf_ssh},
			{require, cgf_sftp}].

sftp_cs(Config) ->
	User = proplists:get_value(ssh_user, Config),
	Handle = proplists:get_value(handle, Config),
	{ok, CDR1} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	{ok, CDR2} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	{ok, CDR3} = 'CSChargingDataTypes':encode('CSRecord', mo_call_record()),
	Data = <<CDR1/binary, CDR2/binary, CDR3/binary>>,
	Filename = cdr_filename(1),
	Match = {User, [], Filename, []},
	Action = {import, {cgf_cs_fsm, ?FUNCTION_NAME}},
	ok = cgf:add_action(file_close, Match, Action),
	ok = ct_ssh:write_file(Handle, Filename, Data),
	ct:sleep(1000),
	3 = proplists:get_value(no_written_items, disk_log:info(?FUNCTION_NAME)).

import_mt_call() ->
	Description = "Import a mobile terminated (MT) CDR file.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

import_mt_call(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	Log = bx_cs,
	FileHeader = file_header(),
	FileHeader1 = FileHeader#cdr_file_header{count = 1},
	FileHeaderB = cgf_3gpp_file:file_header(FileHeader1),
	FileHeaderL = byte_size(FileHeaderB) + 8,
	CdrHeader = cdr_header(32250),
	CdrHeaderB = cgf_3gpp_file:cdr_header(CdrHeader),
	CdrHeaderL = byte_size(CdrHeaderB) + 2,
	{ok, CDR} = 'CSChargingDataTypes':encode('CSRecord', mt_call_record()),
	CdrLength = byte_size(CDR),
	FileLength = FileHeaderL + CdrHeaderL + CdrLength,
	FileB = <<FileLength:32, FileHeaderL:32, FileHeaderB/binary,
			CdrLength:16, CdrHeaderB/binary, CDR/binary>>,
	FilePath = filename:join(PrivDir, cdr_filename(1)),
	ok = file:write_file(FilePath, FileB),
	ok = cgf_cs:import(FilePath, Log).

import_mo_sms() ->
	Description = "Import a circuit switched (CS) MO SMS record CDR file.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

import_mo_sms(Config) ->
	PrivDir = proplists:get_value(priv_dir, Config),
	Log = bx_cs,
	FileHeader = file_header(),
	FileHeader1 = FileHeader#cdr_file_header{count = 1},
	FileHeaderB = cgf_3gpp_file:file_header(FileHeader1),
	FileHeaderL = byte_size(FileHeaderB) + 8,
	CdrHeader = cdr_header(32250),
	CdrHeaderB = cgf_3gpp_file:cdr_header(CdrHeader),
	CdrHeaderL = byte_size(CdrHeaderB) + 2,
	{ok, CDR} = 'CSChargingDataTypes':encode('CSRecord', mo_sms_record()),
	CdrLength = byte_size(CDR),
	FileLength = FileHeaderL + CdrHeaderL + CdrLength,
	FileB = <<FileLength:32, FileHeaderL:32, FileHeaderB/binary,
			CdrLength:16, CdrHeaderB/binary, CDR/binary>>,
	FilePath = filename:join(PrivDir, cdr_filename(1)),
	ok = file:write_file(FilePath, FileB),
	ok = cgf_cs:import(FilePath, Log).

file_close_copy() ->
	Description = "SFTP put a file with a copy action.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]},
			{require, cgf_ssh},
			{require, cgf_sftp}].

file_close_copy(Config) ->
	User = proplists:get_value(ssh_user, Config),
	SftpdUserDir = proplists:get_value(sftpd_user_dir, Config),
	Handle = proplists:get_value(handle, Config),
	Data = rand:bytes(rand:uniform(1048576)),
	Filename = cdr_filename(1),
	Match = {User, [], Filename, []},
	RE = <<"^CGF">>,
	Replacement = <<"copy/&">>,
	Action = {copy, {RE, Replacement}},
	ok = cgf:add_action(file_close, Match, Action),
	CopyDir = filename:join(SftpdUserDir, copy),
	ok = file:make_dir(CopyDir),
	ok = ct_ssh:write_file(Handle, Filename, Data),
	ct:sleep(1000),
	CopyPath = filename:join(CopyDir, Filename),
	{ok, _} = file:read_file_info(CopyPath).

file_close_move() ->
	Description = "SFTP put a file with a move action.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]},
			{require, cgf_ssh},
			{require, cgf_sftp}].

file_close_move(Config) ->
	User = proplists:get_value(ssh_user, Config),
	SftpdUserDir = proplists:get_value(sftpd_user_dir, Config),
	Handle = proplists:get_value(handle, Config),
	Data = rand:bytes(rand:uniform(1048576)),
	Filename = cdr_filename(1),
	Match = {User, [], Filename, []},
	RE = <<"^CGF">>,
	Replacement = <<"move/&">>,
	Action = {copy, {RE, Replacement}},
	ok = cgf:add_action(file_close, Match, Action),
	MoveDir = filename:join(SftpdUserDir, "move"),
	ok = file:make_dir(MoveDir),
	ok = ct_ssh:write_file(Handle, Filename, Data),
	ct:sleep(1000),
	MovePath = filename:join(MoveDir, Filename),
	{ok, _} = file:read_file_info(MovePath).

file_close_delete() ->
	Description = "SFTP put a file with a delete action.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]},
			{require, cgf_ssh},
			{require, cgf_sftp}].

file_close_delete(Config) ->
	User = proplists:get_value(ssh_user, Config),
	SftpdUserDir = proplists:get_value(sftpd_user_dir, Config),
	Handle = proplists:get_value(handle, Config),
	Data = rand:bytes(rand:uniform(1048576)),
	Filename = cdr_filename(1),
	Match = {User, [], Filename, []},
	RE = <<"^CGF">>,
	Action = {delete, RE},
	ok = cgf:add_action(file_close, Match, Action),
	ok = ct_ssh:write_file(Handle, Filename, Data),
	ct:sleep(1000),
	FilePath = filename:join(SftpdUserDir, Filename),
	{error, enoent} = file:read_file_info(FilePath).

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

mt_call_record() ->
	MSISDN = cgf_test_lib:rand_dn(),
	IMSI = cgf_test_lib:rand_imsi(),
	IMEI = cgf_test_lib:rand_imei(),
	TON = 1,
	NPI = 1,
	CalledPartyBCD = cgf_lib:tbcd(MSISDN),
	CalledPartyAddress = <<1:1, TON:3, NPI:4, CalledPartyBCD/binary>>,
	CallingPartyDN = cgf_test_lib:rand_dn(),
	CallingPartyBCD = cgf_lib:tbcd(CallingPartyDN),
	CallingPartyAddress = <<1:1, TON:3, NPI:4, CallingPartyBCD/binary>>,
	MscAddressDN = cgf_test_lib:rand_dn(),
	MscAddressBCD = cgf_lib:tbcd(MscAddressDN),
	MscAddress = <<1:1, TON:3, NPI:4, MscAddressBCD/binary>>,
	LocalTime = calendar:local_time(),
	LocalSeconds = calendar:datetime_to_gregorian_seconds(LocalTime),
	Duration = cgf_test_lib:rand_duration(),
	StartSeconds = LocalSeconds - Duration,
	StartTime = calendar:gregorian_seconds_to_datetime(StartSeconds),
	CallReference = binary:encode_unsigned(rand:uniform(4294967295)),
	BasicService = {teleservice, <<11>>},
	MTCallRecord = #{recordType => mtCallRecord,
		servedIMSI => cgf_lib:tbcd(IMSI),
		servedIMEI => cgf_lib:tbcd(IMEI),
		servedMSISDN => cgf_lib:tbcd(MSISDN),
		callingNumber => CallingPartyAddress,
		calledNumber => CalledPartyAddress,
		recordingEntity => MscAddress,
		mscIncomingTKGP => {tkgpNumber, rand:uniform(99)},
		basicService => BasicService,
		seizureTime => bcd_date_time(StartTime),
		releaseTime => bcd_date_time(LocalTime),
		callDuration => Duration,
		causeForTerm => normalRelease,
		callReference => CallReference,
		roaming => false,
		chargingID => binary:encode_unsigned(rand:uniform(4294967295))},
	{mtCallRecord, MTCallRecord}.

mo_sms_record() ->
	MSISDN = cgf_test_lib:rand_dn(),
	IMSI = cgf_test_lib:rand_imsi(),
	IMEI = cgf_test_lib:rand_imei(),
	TON = 2,
	NPI = 1,
	MSISDNBCD = cgf_lib:tbcd(MSISDN),
	MSISDNAddress = <<1:1, TON:3, NPI:4, MSISDNBCD/binary>>,
	CalledPartyDN = cgf_test_lib:rand_dn(),
	CalledPartyBCD = cgf_lib:tbcd(CalledPartyDN),
	CalledPartyAddress = <<1:1, TON:3, NPI:4, CalledPartyBCD/binary>>,
	ServiceCentreDN = cgf_test_lib:rand_dn(),
	ServiceCentreBCD = cgf_lib:tbcd(ServiceCentreDN),
	ServiceCentreAddress = <<1:1, TON:3, NPI:4, ServiceCentreBCD/binary>>,
	OriginationTime = calendar:local_time(),
	MessageReference = binary:encode_unsigned(rand:uniform(255)),
	SmsResult = {gsm0408Cause, rand:uniform(256)},
	MOSMSRecord = #{recordType => moSMSRecord,
		servedIMSI => cgf_lib:tbcd(IMSI),
		servedIMEI => cgf_lib:tbcd(IMEI),
		servedMSISDN => MSISDNAddress,
		serviceCentre => ServiceCentreAddress,
		destinationNumber => CalledPartyAddress,
		recordingEntity => ServiceCentreAddress,
		originationTime => bcd_date_time(OriginationTime),
		messageReference => MessageReference,
		smsResult => SmsResult},
	{moSMSRecord, MOSMSRecord}.

