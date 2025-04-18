%%% cgf_log_codec_ecs.erl
%%% vim: ts=3
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
%%% @doc This library module implements CODEC functions for logging
%%% 	with Elastic Common Schema (ECS) data model
%%% 	in the {@link //cgf. cgf} application.
%%%
-module(cgf_log_codec_ecs).
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% export the cgf_log_codec_ecs public API
-export([bx/1, import/1]).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 27).
		-define(JSON, json).
	-else.
		-define(JSON, zj).
	-endif.
-else.
	-define(JSON, zj).
-endif.

%%----------------------------------------------------------------------
%%  The cgf_log_codec_ecs public API
%%----------------------------------------------------------------------

-spec bx(CDR) -> iodata()
	when
		CDR :: [{RecordType, Parameters}],
		RecordType :: moCall | mtCall
				| moSMS | mtSMS | scSMO | scSMT | sgsnSMO | sgsnSMT
				| sgsnPDP | ggsnPDP | sgw | ssAction
				| incGateway | outGateway | transit | roaming
				| roam_batchControlInfo | roam_accountingInfo
				| roam_moCall | roam_mtCall | roam_gprs
				| vas | rated | abmf
				| string(),
		Parameters :: #{_Name := binary(), _Value := term()}.
%% @doc Bx interface CODEC for Elastic Stack logs.
%%
%% 	Formats charging data record (CDR) events on the Bx interface
%% 	for consumption by Elastic Stack by providing a JSON
%% 	format aligned with The Elastic Common Schema (ECS).
%%
%% 	Use this CODEC function with the {@link //cgf/cgf_log. cgf_log}
%% 	logging functions with the
%% 	{@link //cgf/cgf_log:log_option(). cgf_log:log_option()}
%% 	`{codec, {{@module}, bx}}'.
%%
bx([{moCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = call_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_moCall", $", $:, ?JSON:encode(Parameters)]]);
bx([{mtCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = call_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_mtCall", $", $:, ?JSON:encode(Parameters)]]);
bx([{moSMS = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_moSMS", $", $:, ?JSON:encode(Parameters)]]);
bx([{mtSMS = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_mtSMS", $", $:, ?JSON:encode(Parameters)]]);
bx([{ssAction = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_ssAction", $", $:, ?JSON:encode(Parameters)]]);
bx([{incGateway = _RecordType, Parameters} | T] = _CDR) ->
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_incGateway", $", $:, ?JSON:encode(Parameters)]]);
bx([{outGateway = _RecordType, Parameters} | T] = _CDR) ->
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_outGateway", $", $:, ?JSON:encode(Parameters)]]);
bx([{transit = _RecordType, Parameters} | T] = _CDR) ->
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_transit", $", $:, ?JSON:encode(Parameters)]]);
bx([{roaming = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = call_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_roaming", $", $:, ?JSON:encode(Parameters)]]);
bx([{sgsnPDP = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = session_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = session_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_sgsnPDP", $", $:, ?JSON:encode(Parameters)]]);
bx([{ggsnPDP = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = session_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = session_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_ggsnPDP", $", $:, ?JSON:encode(Parameters)]]);
bx([{sgsnSMO = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_sgsnSMO", $", $:, ?JSON:encode(Parameters)]]);
bx([{sgsnSMT = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_sgsnSMT", $", $:, ?JSON:encode(Parameters)]]);
bx([{sgw = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = session_duration(Parameters),
	Timestamp = case {StartTime, StopTime} of
		{[], StopTime} ->
			StopTime;
		{StartTime, _} ->
			StartTime
	end,
	Outcome = session_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, StopTime, Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_sgw", $", $:, ?JSON:encode(Parameters)]]);
bx([{vas = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_vas", $", $:, ?JSON:encode(Parameters)]]);
bx([{abmf = _RecordType, Parameters} | T] = _CDR) ->
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"timestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, []), $,,
			ecs_event(Timestamp, [], [],
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_balance", $", $:, ?JSON:encode(Parameters)]]);
bx([{roam_moCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, [], Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_roam_moCall", $", $:, ?JSON:encode(Parameters)]]);
bx([{roam_mtCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, [], Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_roam_mtCall", $", $:, ?JSON:encode(Parameters)]]);
bx([{roam_gprs = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [[${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI), $,,
			ecs_event(StartTime, [], Duration,
					"event", ["session"], ["connection"], Outcome), $,,
			$", "Bx_roam_gprsCall", $", $:, ?JSON:encode(Parameters)]]);
bx([{RecordType, Parameters} | T] = _CDR)
		when is_list(RecordType) ->
	Now = cgf_log:iso8601(erlang:system_time(millisecond)),
	bx1(T, [[${,
			ecs_base(Now), $,,
			ecs_service("bx", "cgf"), $,,
			$", RecordType, $", $:, ?JSON:encode(Parameters)]]).
%% @hidden
bx1([{rated, Rated} | T], Acc) ->
	Acc1 = [[$,, $", "Bx_rated", $", $:, ?JSON:encode(Rated)]],
	bx1(T, [Acc1 | Acc]);
bx1([{roam_batchControlInfo, BatchControlInfo} | T], Acc) ->
	Acc1 = [[$,, $", "Bx_roam_batchControlInfo", $", $:,
			?JSON:encode(BatchControlInfo)]],
	bx1(T, [Acc1 | Acc]);
bx1([{roam_accountingInfo, AccountingInfo} | T], Acc) ->
	Acc1 = [[$,, $", "Bx_roam_accountingInfo", $", $:, ?JSON:encode(AccountingInfo)]],
	bx1(T, [Acc1 | Acc]);
bx1([{AttributeName, AttributeValue} | T], Acc)
		when is_list(AttributeName) ->
	Acc1 = [[$,, $", AttributeName, $", $:, ?JSON:encode(AttributeValue)]],
	bx1(T, [Acc1 | Acc]);
bx1([], Acc) ->
	[lists:reverse(Acc) | [$}]].

-spec import(Report) -> iodata()
	when
		Report :: map().
%% @doc Import report CODEC for Elastic Stack logs.
%%
%% 	Formats events reporting on import process execution
%% 	for consumption by Elastic Stack by providing a JSON
%% 	format aligned with The Elastic Common Schema (ECS).
%%
%% 	Use this CODEC function with the {@link //cgf/cgf_log. cgf_log}
%% 	logging functions with the
%% 	{@link //cgf/cgf_log:log_option(). cgf_log:log_option()}
%% 	`{codec, {{@module}, import}}'.
%%
import(Report) ->
	Event = maps:get("event", Report),
	Outcome = maps:get("outcome", Event),
	Process = maps:get("process", Report),
	StartTime = maps:get("start", Process),
	StopTime = maps:get("end", Process, []),
	{Timestamp, Duration, Type} = case length(StopTime) of
		0 ->
			{StartTime, [], ["access", "start"]};
		_ ->
			StartMs = cgf_log:iso8601(StartTime),
			StopMs = cgf_log:iso8601(StopTime),
			Nanos = (StopMs - StartMs) * 1000000,
			{StopTime, integer_to_list(Nanos), ["access", "end"]}
	end,
	User = maps:get("user", Report),
	File = maps:get("file", Report),
	ImportCDR  = maps:get("Import_CDR", Report),
	Kind = "event",
	Category = ["file", "process"],
	[${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(StartTime, StopTime, Duration,
					Kind, Category, Type, Outcome), $,,
			$", "user", $", $:, ?JSON:encode(User), $,,
			$", "file", $", $:, ?JSON:encode(File), $,,
			$", "process", $", $:, ?JSON:encode(Process), $,,
			$", "Import_CDR", $", $:, ?JSON:encode(ImportCDR), $}].

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec ecs_base(Timestamp) -> iodata()
	when
		Timestamp :: string().
%% @doc Elastic Common Schema (ECS): Base attributes.
%%
%% 	`Timestamp' is milliseconds since the epoch
%% 	(e.g. `erlang:system_time(millisecond)').
%% @private
ecs_base(Timestamp) ->
	TS = [$", "@timestamp", $", $:, $", Timestamp, $"],
	Labels = [$", "labels", $", $:, ${,
			$", "application", $", $:, $", "sigscale-cgf", $", $}],
	Version = [$", "ecs", $", $:, ${,
			$", "version", $", $:, $", "8.5", $", $}],
	[TS, $,, Labels, $,, Version].

-spec ecs_service(Name, Type) -> iodata()
	when
		Name :: string(),
		Type :: string().
%% @doc Elastic Common Schema (ECS): Service attributes.
%% @private
ecs_service(Name, Type) ->
	Sname = [$,, $", "name", $", $:, $", Name, $"],
	Stype = [$,, $", "type", $", $:, $", Type, $"],
	Snode = [$", "node", $", $:, ${,
			$", "name", $", $:, $", atom_to_list(node()), $", $}],
	[$", "service", $", $:, ${, Snode, Sname, Stype, $}].

-spec ecs_event(Start, End, Duration,
		Kind, Category, Type, Outcome) -> iodata()
	when
		Start :: string(),
		End:: string(),
		Duration :: string(),
		Kind :: string(),
		Category :: [string()],
		Type :: [string()],
		Outcome :: string().
%% @doc Elastic Common Schema (ECS): Event attributes.
%% @private
ecs_event(Start, End, Duration, Kind, Category, Type, Outcome) ->
	Now = cgf_log:iso8601(erlang:system_time(millisecond)),
	Ecreated = [$", "created", $", $:, $", Now, $"],
	Estart = case Start of
		[] ->
			[];
		_ ->
			[$,, $", "start", $", $:, $", Start, $"]
	end,
	Eend = case End of
		[] ->
			[];
		_ ->
			[$,, $", "end", $", $:, $", End, $"]
	end,
	Eduration = case Duration of
		[] ->
			[];
		_ ->
			[$,, $", "duration", $", $:, $", Duration, $"]
	end,
	Ekind = [$,, $", "kind", $", $:, $", Kind, $"],
	Ecategories = case Category of
		[H1] ->
			[$", H1, $"];
		[H1 | T1] ->
			[$", H1, $" | [[$,, $", E, $"] || E <- T1]]
	end,
	Ecategory = [$,, $", "category", $", $:, $[, Ecategories, $]],
	Etypes = case Type of
		[H2] ->
			[$", H2, $"];
		[H2 | T2] ->
			[$", H2, $" | [[$,, $", E, $"] || E <- T2]]
	end,
	Etype = [$,, $", "type", $", $:, $[, Etypes, $]],
	Eoutcome = [$,, $", "outcome", $", $:, $", Outcome, $"],
	[$", "event", $", $:, ${, Ecreated, Estart, Eend,
			Eduration, Ekind, Ecategory, Etype, Eoutcome, $}].

-spec ecs_user(Name, Id) -> iodata()
	when
		Name :: string(),
		Id :: string().
%% @doc Elastic Common Schema (ECS): User attributes.
%% @private
ecs_user([] = _Name, Id) ->
	ecs_user1(Id, []);
ecs_user(Name, Id) ->
	Uname = [$", "name", $", $:, $", Name, $"],
	ecs_user1(Id, [Uname]).
%% @hidden
ecs_user1([] = _Id, Acc) ->
	ecs_user2(Acc);
ecs_user1(Id, Acc) ->
	Uid = [$", "id", $", $:, $", Id, $"],
	ecs_user2([Uid | Acc]).
%% @hidden
ecs_user2([]) ->
	[];
ecs_user2(Acc) ->
	[H | T] = lists:reverse(Acc),
	Rest = [[$,, Field] || Field <- T],
	[$", "user", $", $:, ${, H, Rest, $}].

-spec imsi(Parameters) -> IMSI
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		IMSI :: string(). % "imsi-001001123456789"
%% @hidden
imsi(#{<<"servedIMSI">> := ServedIMSI} = _Parameters)
		when byte_size(ServedIMSI) > 0 ->
	"imsi-" ++ binary_to_list(ServedIMSI);
imsi(#{<<"servedIMSI">> := ServedIMSI} = _Parameters)
		when length(ServedIMSI) > 0 ->
	"imsi-" ++ ServedIMSI;
imsi(#{<<"basicCallInformation">> := #{<<"chargeableSubscriber">>
		:= #{<<"simChargeableSubscriber">> := #{<<"imsi">> := IMSI}}}})
		when byte_size(IMSI) > 0 ->
	"imsi-" ++ binary_to_list(IMSI);
imsi(#{<<"basicCallInformation">> := #{<<"chargeableSubscriber">>
		:= #{<<"simChargeableSubscriber">> := #{<<"imsi">> := IMSI}}}})
		when length(IMSI) > 0 ->
	"imsi-" ++ IMSI;
imsi(#{<<"gprsBasicCallInformation">> := #{<<"gprsChargeableSubscriber">>
		:= #{<<"chargeableSubscriber">> := #{<<"simChargeableSubscriber">>
		:= #{<<"imsi">> := IMSI}}}}})
		when byte_size(IMSI) > 0 ->
	"imsi-" ++ binary_to_list(IMSI);
imsi(#{<<"gprsBasicCallInformation">> := #{<<"gprsChargeableSubscriber">>
		:= #{<<"chargeableSubscriber">> := #{<<"simChargeableSubscriber">>
		:= #{<<"imsi">> := IMSI}}}}})
		when length(IMSI) > 0 ->
	"imsi-" ++ IMSI;
imsi(_Parameters) ->
	[].

-spec msisdn(Parameters) -> MSISDN
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		MSISDN :: string(). % "msisdn-14165551234"
%% @hidden
msisdn(#{<<"servedMSISDN">> := ServedMSISDN} = _Parameters)
		when byte_size(ServedMSISDN) > 0 ->
	"msisdn-" ++ binary_to_list(ServedMSISDN);
msisdn(#{<<"servedMSISDN">> := ServedMSISDN} = _Parameters)
		when length(ServedMSISDN) > 0 ->
	"msisdn-" ++ ServedMSISDN;
msisdn(#{<<"chargingParty">> := ChargingParty} = _Parameters)
		when byte_size(ChargingParty) > 0 ->
	"msisdn-" ++ binary_to_list(ChargingParty);
msisdn(#{<<"chargingParty">> := ChargingParty} = _Parameters)
		when length(ChargingParty) > 0 ->
	"msisdn-" ++ ChargingParty;
msisdn(#{<<"basicCallInformation">> := #{<<"chargeableSubscriber">>
		:= #{<<"simChargeableSubscriber">> := #{<<"msisdn">> := MSISDN}}}})
		when byte_size(MSISDN) > 0 ->
	"msisdn-" ++ binary_to_list(MSISDN);
msisdn(#{<<"basicCallInformation">> := #{<<"chargeableSubscriber">>
		:= #{<<"simChargeableSubscriber">> := #{<<"msisdn">> := MSISDN}}}})
		when length(MSISDN) > 0 ->
	"msisdn-" ++ MSISDN;
msisdn(#{<<"gprsBasicCallInformation">> := #{<<"gprsChargeableSubscriber">>
		:= #{<<"chargeableSubscriber">> := #{<<"simChargeableSubscriber">>
		:= #{<<"msisdn">> := MSISDN}}}}})
		when byte_size(MSISDN) > 0 ->
	"msisdn-" ++ binary_to_list(MSISDN);
msisdn(#{<<"gprsBasicCallInformation">> := #{<<"gprsChargeableSubscriber">>
		:= #{<<"chargeableSubscriber">> := #{<<"simChargeableSubscriber">>
		:= #{<<"msisdn">> := MSISDN}}}}})
		when length(MSISDN) > 0 ->
	"msisdn-" ++ MSISDN;
msisdn(_Parameters) ->
	[].

-spec call_duration(Parameters) -> Result
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		Result :: {StartTime, StopTime, Duration},
		StartTime :: iodata(),
		StopTime :: iodata(),
		Duration :: string().
%% @hidden
call_duration(Parameters) ->
	case {maps:find(<<"seizureTime">>, Parameters),
			maps:find(<<"answerTime">>, Parameters),
			maps:find(<<"releaseTime">>, Parameters),
			maps:find(<<"callDuration">>, Parameters)} of
		{_, {ok, Answer}, {ok, Release}, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Answer, Release, integer_to_list(Seconds * 1000000000)};
		{_, {ok, Answer}, {ok, Release}, error} ->
			Ms = cgf_log:iso8601(Release) - cgf_log:iso8601(Answer),
			{Answer, Release, integer_to_list(Ms * 1000000)};
		{{ok, Seizure}, error, {ok, Release}, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Seizure, Release, integer_to_list(Seconds * 1000000000)};
		{{ok, Seizure}, error, {ok, Release}, error} ->
			Ms = cgf_log:iso8601(Release) - cgf_log:iso8601(Seizure),
			{Seizure, Release, integer_to_list(Ms * 1000000)};
		{{ok, Seizure}, error, error, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Seizure, [], integer_to_list(Seconds * 1000000000)};
		{{ok, Seizure}, error, error, error} ->
			{Seizure, [], []};
		{_, {ok, Answer}, error, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Answer, [], integer_to_list(Seconds * 1000000000)};
		{_, {ok, Answer}, error, error} ->
			{Answer, [], []};
		{_, _, {ok, Release}, {ok, Seconds}}
				when is_integer(Seconds) ->
			{[], Release, integer_to_list(Seconds * 1000000000)};
		{_, _, {ok, Release}, error} ->
			{[], Release, []};
		_ ->
			{[], [], []}
	end.

-spec session_duration(Parameters) -> Result
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		Result :: {StartTime, StopTime, Duration},
		StartTime :: iodata(),
		StopTime :: iodata(),
		Duration :: string().
%% @hidden
session_duration(Parameters) ->
	case {maps:find(<<"startTime">>, Parameters),
			maps:find(<<"stopTime">>, Parameters),
			maps:find(<<"duration">>, Parameters)} of
		{{ok, Start}, {ok, Stop}, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Start, Stop, integer_to_list(Seconds * 1000000000)};
		{{ok, Start}, {ok, Stop}, error} ->
			Ms = cgf_log:iso8601(Start) - cgf_log:iso8601(Stop),
			{Start, Stop, integer_to_list(Ms * 1000000)};
		{{ok, Start}, _, {ok, Seconds}}
				when is_integer(Seconds) ->
			{Start, [], integer_to_list(Seconds * 1000000000)};
		{_, {ok, Stop}, {ok, Seconds}}
				when is_integer(Seconds) ->
			{[], Stop, integer_to_list(Seconds * 1000000000)};
		{{ok, Start}, {ok, Stop}, _} ->
			{Start, Stop, []};
		{{ok, Start}, _, _} ->
			{Start, [], []};
		{_, {ok, Stop}, _} ->
			{[], Stop, []};
		_ ->
			{[], [], []}
	end.

%% @hidden
roam_duration(#{<<"basicCallInformation">> := I}) ->
	StartTime = case maps:find(<<"callEventStartTimeStamp">>, I) of
		{ok, Start} ->
			Start;
		error ->
			[]
	end,
	Duration = case maps:find(<<"totalCallEventDuration">>, I) of
		{ok, Seconds} ->
			integer_to_list(Seconds * 1000000000);
		error ->
			[]
	end,
	{StartTime, Duration};
roam_duration(#{<<"gprsBasicCallInformation">> := I}) ->
	StartTime = case maps:find(<<"callEventStartTimeStamp">>, I) of
		{ok, Start} ->
			Start;
		error ->
			[]
	end,
	Duration = case maps:find(<<"totalCallEventDuration">>, I) of
		{ok, Seconds} ->
			integer_to_list(Seconds * 1000000000);
		error ->
			[]
	end,
	{StartTime, Duration};
roam_duration(_) ->
	{[], []}.

-spec call_outcome(Parameters) -> Outcome
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		Outcome :: string(). % "success" | "failure" | |unknown"
%% @hidden
call_outcome(Parameters) ->
	case maps:find(<<"causeForTerm">>, Parameters) of
		{ok, "normalRelease"} ->
			"success";
		{ok, "partialRecord"} ->
			"unknown";
		{ok, "partialRecordCallReestablishment"} ->
			"unknown";
		{ok, "unsuccessfulCallAttempt"} ->
			"failure";
		{ok, "abnormalRelease"} ->
			"failure";
		{ok, "cAMELInitCallRelease"} ->
			"unknown";
		{ok, "unauthorizedRequestingNetwork"} ->
			"failure";
		{ok, "unauthorizedLCSClient"} ->
			"failure";
		{ok, "positionMethodFailure"} ->
			"failure";
		{ok, "unknownOrUnreachableLCSClient"} ->
			"failure";
		_ ->
			"unknown"
	end.

-spec session_outcome(Parameters) -> Outcome
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		Outcome :: string(). % "success" | "failure" | |unknown"
%% @hidden
session_outcome(Parameters) ->
	case maps:find(<<"causeForRecClosing">>, Parameters) of
		{ok, "normalRelease"} ->
			"success";
		{ok, "partialRecord"} ->
			"unknown";
		{ok, "abnormalRelease"} ->
			"failure";
		{ok, "cAMELInitCallRelease"} ->
			"unknown";
		{ok, "volumeLimit"} ->
			"success";
		{ok, "timeLimit"} ->
			"success";
		{ok, "servingNodeChange"} ->
			"success";
		{ok, "maxChangeCond"} ->
			"unknown";
		{ok, "managementIntervention"} ->
			"unknown";
		{ok, "intraSGSNIntersystemChange"} ->
			"success";
		{ok, "rATChange"} ->
			"success";
		{ok, "mSTimeZoneChange"} ->
			"success";
		{ok, "sGSNPLMNIDChange"} ->
			"unknown";
		{ok, "sGWChange"} ->
			"success";
		{ok, "aPNAMBRChange"} ->
			"success";
		{ok, "mOExceptionDataCounterReceipt"} ->
			"failure";
		{ok, "unauthorizedRequestingNetwork"} ->
			"failure";
		{ok, "unauthorizedLCSClient"} ->
			"failure";
		{ok, "positionMethodFailure"} ->
			"failure";
		{ok, "unknownOrUnreachableLCSClient"} ->
			"failure";
		{ok, "listofDownstreamNodeChange"} ->
			"unknown";
		_ ->
			"unknown"
	end.

-spec roam_outcome(Parameters) -> Outcome
	when
		Parameters :: #{_Name := binary(), _Value := term()},
		Outcome :: string(). % "success" | "failure" | |unknown"
%% @doc GSMA TD.57 Annex C
%% @hidden
roam_outcome(#{<<"gprsBasicCallInformation">>
		:= #{<<"causeForTerm">> := Cause}} = _Parameters) ->
	roam_outcome1(Cause);
roam_outcome(#{<<"basicCallInformation">>
		:= #{<<"causeForTerm">> := Cause}} = _Parameters) ->
	roam_outcome1(Cause);
roam_outcome(_Parameters) ->
	"unknown".
%% @hidden
roam_outcome1(0 = _Cause) ->
	"success";
roam_outcome1(3 = _Cause) ->
	"failure";
roam_outcome1(4 = _Cause) ->
	"failure";
roam_outcome1(16 = _Cause) ->
	"success";
roam_outcome1(17 = _Cause) ->
	"success";
roam_outcome1(18 = _Cause) ->
	"success";
roam_outcome1(21 = _Cause) ->
	"success";
roam_outcome1(22 = _Cause) ->
	"success";
roam_outcome1(23 = _Cause) ->
	"success";
roam_outcome1(24 = _Cause) ->
	"success";
roam_outcome1(_Cause) ->
	"unknown".

