%%% cgf_json_validation_SUITE.erl
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
%%% Test suite for validation against the JSON Schemas in
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_json_validation_SUITE).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% export test cases
-export([ecs_base/0, ecs_base/1,
		ecs_ecs/0, ecs_ecs/1,
		ecs_service/0, ecs_service/1,
		ecs_event/0, ecs_event/1,
		ecs_network/0, ecs_network/1,
		ecs_user/0, ecs_user/1,
		ecs_log/0, ecs_log/1,
		ecs_data_stream/0, ecs_data_stream/1]).

-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	Description = "Test suite for validation against the JSON Schemas",
   [{userdata, [{doc, Description}]}, {timetrap, {minutes, 1}}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before the whole suite.
%%
init_per_suite(Config) ->
	Applications = [crypto, asn1, public_key, ssl, inets],
	ok = cgf_test_lib:start(Applications),
	case  application:start(jesse) of 
		ok ->
			ok = cgf_test_lib:start(),
			{spec_dir, SpecDir} = ct:get_testspec_terms(spec_dir),
			SchemaDir = filename:join([SpecDir, "..", priv, schema]),
			[{schema_dir, SchemaDir} | Config];
		{error, Reason} ->
			{skipped, Reason}
	end.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok.

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(ecs_base, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_base"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_ecs, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_ecs"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_service, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_service"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_event, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_event"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_network, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_network"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_user, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_user"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_log, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_log"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
init_per_testcase(ecs_data_stream, Config) ->
	SchemaDir = proplists:get_value(schema_dir, Config),
	case add_schemas(SchemaDir, ["ECS_data_stream"]) of
		ok ->
			Config;
		{error, Reason} ->
			{skipped, Reason}
	end;
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
	[ecs_base, ecs_ecs, ecs_service, ecs_event, ecs_network, ecs_user,
			ecs_log, ecs_data_stream].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

ecs_base() ->
	Description = "Validate ECS base envelope.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_base(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_base", Data).

ecs_ecs() ->
	Description = "Validate meta-information about ECS.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_ecs(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_ecs", Data).

ecs_service() ->
	Description = "Validate ECS service information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_service(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_service", Data).

ecs_event() ->
	Description = "Validate ECS event information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_event(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_event", Data).

ecs_network() ->
	Description = "Validate ECS network information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_network(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_network", Data).

ecs_user() ->
	Description = "Validate ECS user information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_user(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	IoData = cgf_log_codec_ecs:bx([{RecordType, Parameters}]),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_user", Data).

ecs_log() ->
	Description = "Validate ECS log information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_log(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	MetaData = {"log", log()},
	CDR = [{RecordType, Parameters}, MetaData],
	IoData = cgf_log_codec_ecs:bx(CDR),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_log", Data).

ecs_data_stream() ->
	Description = "Validate ECS data stream information.",
	ct:comment(Description),
	[{userdata, [{doc, Description}]}].

ecs_data_stream(_Config) ->
	RecordType = moCall,
	IMSI = "001001" ++ cgf_test_lib:rand_dn(9),
	MSISDN = "1416" ++ cgf_test_lib:rand_dn(7),
	Parameters = mo_call(IMSI, MSISDN),
	MetaData = {"data_stream", data_stream()},
	CDR = [{RecordType, Parameters}, MetaData],
	IoData = cgf_log_codec_ecs:bx(CDR),
	{ok, Data} = zj:binary_decode(IoData),
	{ok, _} = jesse:validate("ECS_data_stream", Data).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

add_schemas(Dir, [Key | T]) ->
	Filename = filename:join([Dir, Key ++ ".json"]),
	add_schema(Dir, T, Key, file:read_file(Filename));
add_schemas(_Dir, []) ->
	ok.

add_schema(Dir, T, Key, {ok, Binary}) ->
	add_schema1(Dir, T, Key, zj:binary_decode(Binary));
add_schema(_Dir, _T, _Key, {error, Reason}) ->
	{error, Reason}.
add_schema1(Dir, T, Key, {ok, Schema}) ->
	add_schema2(Dir, T, jesse:add_schema(Key, Schema));
add_schema1(_Dir, _T, _Key, {error, Reason}) ->
	{error, Reason}.
add_schema2(Dir, T, ok) ->
	add_schemas(Dir, T);
add_schema2(_Dir, _T, {error, Reason}) ->
	{error, Reason}.

data_stream() ->
	#{"type" => "logs",
			"dataset" => "cdr.billing.cs",
			"namespace" => "acme"}.

log() ->
	File = #{"name" => "CGFNodeId_-_1234.20050401_-_2315+0200",
			"mime_type" => "application/octet-stream"},
	User = #{"name" => "sftp-user1",
			"email" => "billing@example.net",
			"domain" => "mnc001.mcc001.3gppnetwork.org"},
	#{"file" => File, "user" => User}.

call_outcome() ->
	Outcomes = [success, failure, unknown],
	Outcome = lists:nth(rand:uniform(length(Outcomes)), Outcomes),
	call_outcome(Outcome).

call_outcome(success) ->
	"normalRelease";
call_outcome(failure) ->
	Causes = ["unsuccessfulCallAttempt", "abnormalRelease",
			"unauthorizedRequestingNetwork", "unauthorizedLCSClient",
			"positionMethodFailure", "unknownOrUnreachableLCSClient"],
	lists:nth(rand:uniform(length(Causes)), Causes);
call_outcome(unknown) ->
	Causes = ["partialRecord", "partialRecordCallReestablishment",
			"cAMELInitCallRelease"],
	lists:nth(rand:uniform(length(Causes)), Causes).

mo_call(IMSI, MSISDN) ->
	CalledNumber = cgf_test_lib:rand_dn(15),
	ReleaseTime = erlang:system_time(second),
	AnswerTime = ReleaseTime - rand:uniform(3600),
	SeizureTime = AnswerTime - rand:uniform(30),
	CallDuration = ReleaseTime - AnswerTime,
	Cause = call_outcome(),
	CallReference = rand:uniform(100000),
	#{<<"servedIMSI">> => list_to_binary(IMSI),
			<<"servedMSISDN">> => list_to_binary(MSISDN),
			<<"seizureTime">> => timestamp(SeizureTime),
			<<"answerTime">> => timestamp(AnswerTime),
			<<"releaseTime">> => timestamp(ReleaseTime),
			<<"callDuration">> => CallDuration,
			<<"causeForTerm">> => list_to_binary(Cause),
			<<"calledNumber">> => #{
					<<"natureOfAddress">> => <<"international">>,
					<<"numberingPlan">> => <<"e164">>,
					<<"address">> => list_to_binary(CalledNumber)},
			<<"recordingEntity">> => #{
					<<"natureOfAddress">> => <<"international">>,
					<<"numberingPlan">> => <<"e164">>,
					<<"address">> => <<"14165550000">>},
			<<"location">> => #{
					<<"locationAreaCode">> => 10200,
					<<"cellId">> => 4353,
					<<"mCC-MNC">> => <<"001001">>},
			<<"basicService">> => #{<<"bearerService">> => 20},
			<<"msClassmark">> => [125, 87],
			<<"callReference">> => CallReference,
			<<"systemType">> => <<"gERAN">>}.

timestamp(Time) ->
	TS = calendar:system_time_to_rfc3339(Time),
	list_to_binary(TS).

