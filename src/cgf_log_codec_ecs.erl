%%% cgf_log_codec_ecs.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2021-2024 SigScale Global Inc.
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
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% export the cgf_log_codec_ecs  public API
-export([bx/1]).
-export([ecs_base/1, ecs_server/4, ecs_client/4, ecs_network/2,
		ecs_source/5, ecs_destination/1, ecs_service/2, ecs_event/7,
		ecs_url/1]).

%%----------------------------------------------------------------------
%%  The cgf_log_codec_ecs public API
%%----------------------------------------------------------------------

-spec bx(CDR) -> iodata()
	when
		CDR :: [{RecordType, Parameters}],
		RecordType :: moCall | mtCall
				| moSMS | mtSMS | scSMO | scSMT
				| sgsnPDP | sgw | ssAction
				| incGateway | outGateway | roaming
				| roam_batchControlInfo | roam_accountingInfo
				| roam_moCall | roam_mtCall | roam_gprs
				| vas | rated | abmf
				| string(),
		Parameters :: #{_Name := binary(), _Value := term()}.
%% @doc Bx interface CODEC for Elastic Stack logs.
%%
%% 	Formats charging data record (CDR) events on the Bx interface
%% 	for consumption by Elastic Stack by providing a JSON format aligned
%% 	with The Elastic Common Schema (ECS).
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_moCall", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_mtCall", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_moSMS", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_mtSMS", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_ssAction", $", $:, zj:encode(Parameters)]);
bx([{incGateway = _RecordType, Parameters} | T] = _CDR) ->
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_incGateway", $", $:, zj:encode(Parameters)]);
bx([{outGateway = _RecordType, Parameters} | T] = _CDR) ->
	Timestamp = case maps:find(<<"eventtimestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_outGateway", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_roaming", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_sgsnPDP", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_sgw", $", $:, zj:encode(Parameters)]);
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
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_vas", $", $:, zj:encode(Parameters)]);
bx([{abmf = _RecordType, Parameters} | T] = _CDR) ->
	MSISDN = msisdn(Parameters),
	Timestamp = case maps:find(<<"timestamp">>, Parameters) of
		{ok, Ts} ->
			Ts;
		error ->
			cgf_log:iso8601(erlang:system_time(millisecond))
	end,
	Outcome = call_outcome(Parameters),
	bx1(T, [${,
			ecs_base(Timestamp), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, [], []), $,,
			ecs_event(Timestamp, [], [],
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_ABMF", $", $:, zj:encode(Parameters)]);
bx([{roam_moCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, [], Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_roam_moCall", $", $:, zj:encode(Parameters)]);
bx([{roam_mtCall = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, [], Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_roam_mtCall", $", $:, zj:encode(Parameters)]);
bx([{roam_gprs = _RecordType, Parameters} | T] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, Duration} = roam_duration(Parameters),
	Outcome = roam_outcome(Parameters),
	bx1(T, [${,
			ecs_base(StartTime), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, [], Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_roam_gprsCall", $", $:, zj:encode(Parameters)]);
bx([{RecordType, Parameters} | T] = _CDR)
		when is_list(RecordType) ->
	Now = cgf_log:iso8601(erlang:system_time(millisecond)),
	bx1(T, [${,
			ecs_base(Now), $,,
			ecs_service("bx", "cgf"), $,,
			$", RecordType, $", $:, zj:encode(Parameters)]).
%% @hidden
bx1([{rated, Rated} | T], Acc) ->
	Acc1 = [$,, $", "Bx_rated", $", $:, zj:encode(Rated)],
	bx1(T, [Acc | Acc1]);
bx1([{roam_batchControlInfo, BatchControlInfo} | T], Acc) ->
	Acc1 = [$,, $", "Bx_roam_batchControlInfo", $", $:,
			zj:encode(BatchControlInfo)],
	bx1(T, [Acc | Acc1]);
bx1([{roam_accountingInfo, AccountingInfo} | T], Acc) ->
	Acc1 = [$,, $", "Bx_roam_accountingInfo", $", $:, zj:encode(AccountingInfo)],
	bx1(T, [Acc | Acc1]);
bx1([{AttributeName, AttributeValue} | T], Acc)
		when is_list(AttributeName) ->
	Acc1 = [$,, $", AttributeName, $", $:, zj:encode(AttributeValue)],
	bx1(T, [Acc | Acc1]);
bx1([], Acc) ->
	[Acc | [$}]].

-spec ecs_base(Timestamp) -> iodata()
	when
		Timestamp :: string().
%% @doc Elastic Common Schema (ECS): Base attributes.
%%
%% 	`Timestamp' is milliseconds since the epoch
%% 	(e.g. `erlang:system_time(millisecond)').
%%
ecs_base(Timestamp) ->
	TS = [$", "@timestamp", $", $:, $", Timestamp, $"],
	Labels = [$", "labels", $", $:, ${,
			$", "application", $", $:, $", "sigscale-cgf", $", $}],
	Version = [$", "ecs", $", $:, ${,
			$", "version", $", $:, $", "8.5", $", $}],
	[TS, $,, Labels, $,, Version].

-spec ecs_server(Address, Domain, IP, Port) -> iodata()
	when
		Address :: binary() | string(),
		Domain :: binary() | string(),
		IP :: inet:ip_address() | string(),
		Port :: non_neg_integer() | string().
%% @doc Elastic Common Schema (ECS): Server attributes.
ecs_server(Address, Domain, IP, Port) when is_tuple(IP) ->
	ecs_server(Address, Domain, inet:ntoa(IP), Port);
ecs_server(Address, Domain, IP, Port) when is_integer(Port) ->
	ecs_server(Address, Domain, IP, integer_to_list(Port));
ecs_server(Address, Domain, IP, Port)
		when ((length(Address) > 0) or (size(Address) > 0)) ->
	Acc = [$", "address", $", $:, $", Address, $"],
	ecs_server1(Domain, IP, Port, Acc);
ecs_server(_Address, Domain, IP, Port)
		when ((length(Domain) > 0) or (size(Domain) > 0)) ->
	Acc = [$", "address", $", $:, $", Domain, $"],
	ecs_server1(Domain, IP, Port, Acc);
ecs_server(_Address, Domain, IP, Port) when length(IP) > 0 ->
	Acc = [$", "address", $", $:, $", IP, $"],
	ecs_server1(Domain, IP, Port, Acc);
ecs_server(_Address, _Domain, _IP, Port) ->
	ecs_server3(Port, []).
%% @hidden
ecs_server1(Domain, IP, Port, Acc)
		when ((length(Domain) > 0) or (size(Domain) > 0)) ->
	NewAcc = [Acc, $,, $", "domain", $", $:, $", Domain, $"],
	ecs_server2(IP, Port, NewAcc);
ecs_server1(_Domain, IP, Port, Acc) ->
	ecs_server2(IP, Port, Acc).
%% @hidden
ecs_server2(IP, Port, Acc) when length(IP) > 0 ->
	NewAcc = [Acc, $,, $", "ip", $", $:, $", IP, $"],
	ecs_server3(Port, NewAcc);
ecs_server2(_IP, Port, Acc) ->
	ecs_server3(Port, Acc).
%% @hidden
ecs_server3([$0], Acc) ->
	ecs_server4(Acc);
ecs_server3(Port, Acc) when length(Port) > 0 ->
	NewAcc = [Acc, $,, $", "port", $", $:, Port],
	ecs_server4(NewAcc);
ecs_server3(_Port, Acc) ->
	ecs_server4(Acc).
%% @hidden
ecs_server4([]) ->
	[];
ecs_server4(Acc) ->
	[$,, $", "server", $", $:, ${, Acc, $}].

-spec ecs_client(Address, Domain, IP, Port) -> iodata()
	when
		Address :: binary() | string(),
		Domain :: binary() | string(),
		IP :: inet:ip_address() | string(),
		Port :: non_neg_integer() | string().
%% @doc Elastic Common Schema (ECS): Client attributes.
ecs_client(Address, Domain, IP, Port) when is_tuple(IP) ->
	ecs_client(Address, Domain, inet:ntoa(IP), Port);
ecs_client(Address, Domain, IP, Port) when is_integer(Port) ->
	ecs_client(Address, Domain, IP, integer_to_list(Port));
ecs_client(Address, Domain, IP, Port)
		when ((length(Address) > 0) or (size(Address) > 0)) ->
	Acc = [$", "address", $", $:, $", Address, $"],
	ecs_client1(Domain, IP, Port, Acc);
ecs_client(_Address, Domain, IP, Port)
		when ((length(Domain) > 0) or (size(Domain) > 0)) ->
	Acc = [$", "address", $", $:, $", Domain, $"],
	ecs_client1(Domain, IP, Port, Acc);
ecs_client(_Address, Domain, IP, Port) when length(IP) > 0 ->
	Acc = [$", "address", $", $:, $", IP, $"],
	ecs_client1(Domain, IP, Port, Acc);
ecs_client(_Address, _Domain, _IP, Port) ->
	ecs_client3(Port, []).
%% @hidden
ecs_client1(Domain, IP, Port, Acc)
		when ((length(Domain) > 0) or (size(Domain) > 0)) ->
	NewAcc = [Acc, $,, $", "domain", $", $:, $", Domain, $"],
	ecs_client2(IP, Port, NewAcc);
ecs_client1(_Domain, IP, Port, Acc) ->
	ecs_client2(IP, Port, Acc).
%% @hidden
ecs_client2(IP, Port, Acc) when length(IP) > 0 ->
	NewAcc = [Acc, $,, $", "ip", $", $:, $", IP, $"],
	ecs_client3(Port, NewAcc);
ecs_client2(_IP, Port, Acc) ->
	ecs_client3(Port, Acc).
%% @hidden
ecs_client3([$0], Acc) ->
	ecs_client4(Acc);
ecs_client3(Port, Acc) when length(Port) > 0 ->
	NewAcc = [Acc, $,, $", "port", $", $:, Port],
	ecs_client4(NewAcc);
ecs_client3(_Port, Acc) ->
	ecs_client4(Acc).
%% @hidden
ecs_client4([]) ->
	[];
ecs_client4(Acc) ->
	[$,, $", "client", $", $:, ${, Acc, $}].

-spec ecs_network(Application, Protocol) -> iodata()
	when
		Application :: string(),
		Protocol :: string().
%% @doc Elastic Common Schema (ECS): Network attributes.
ecs_network([] = _Application, Protocol) ->
	ecs_network1(Protocol, []);
ecs_network(Application, Protocol) ->
	Napplication = [$", "application", $", $:, $", Application, $"],
	ecs_network1(Protocol, Napplication).
%% @hidden
ecs_network1([] = _Protocol, [] = _Napplication) ->
	[];
ecs_network1([], Napplication) ->
	[$", "network", $", $:, ${, Napplication, $}];
ecs_network1(Protocol, Napplication) ->
	Nprotocol = [$", "protocol", $", $:, $", Protocol, $"],
	[$", "network", $", $:, ${, Napplication, $,, Nprotocol, $}].

-spec ecs_source(Address, Domain, SubDomain,
		UserName, UserIds) -> iodata()
	when
		Address :: binary() | string(),
		Domain :: binary() | string(),
		SubDomain :: binary() | string(),
		UserName :: binary() | string(),
		UserIds :: [string()].
%% @doc Elastic Common Schema (ECS): Source attributes.
ecs_source([] = _Address, Domain, SubDomain, UserName, UserIds) ->
	ecs_source1(Domain, SubDomain, UserName, UserIds, []);
ecs_source(Address, Domain, SubDomain, UserName, UserIds) ->
	Saddress = [$", "address", $", $:, $", Address, $"],
	ecs_source1(Domain, SubDomain, UserName, UserIds, [Saddress]).
%% @hidden
ecs_source1([], SubDomain, UserName, UserIds, Acc) ->
	ecs_source2(SubDomain, UserName, UserIds, Acc);
ecs_source1(Domain, SubDomain, UserName, UserIds, Acc) ->
	Sdomain = [$", "domain", $", $:, $", Domain, $"],
	ecs_source2(SubDomain, UserName, UserIds, [Sdomain | Acc]).
%% @hidden
ecs_source2([], UserName, UserIds, Acc) ->
	ecs_source3(UserName, UserIds, Acc);
ecs_source2(SubDomain, UserName, UserIds, Acc) ->
	Ssubdomain = [$", "subdomain", $", $:, $", SubDomain, $"],
	ecs_source3(UserName, UserIds, [Ssubdomain | Acc]).
%% @hidden
ecs_source3([], [], Acc) ->
	ecs_source5(Acc);
ecs_source3([], [H | _] = UserIds, Acc) ->
	User = [$", "user", $", $:, ${,
			$", "id", $", $:, $", H, $", $}],
	ecs_source4(UserIds, [User | Acc]);
ecs_source3(UserName, [H | _] = UserIds, Acc) ->
	User = [$", "user", $", $:, ${,
			$", "name", $", $:, $", UserName, $", $,,
			$", "id", $", $:, $", H, $", $}],
	ecs_source4(UserIds, [User | Acc]).
%% @hidden
ecs_source4([], Acc) ->
	ecs_source5(Acc);
ecs_source4([H | T] = _UserIds, Acc) ->
	Related = [$", "related", $", $:, ${,
			$", "user", $", $:, $[, $", H, $",
			[[$,, $", R, $"] || R <- T], $], $}],
	ecs_source5([Related | Acc]).
%% @hidden
ecs_source5([]) ->
	[];
ecs_source5(Acc) ->
	[H | T] = lists:reverse(Acc),
	Rest = [[$,, Field] || Field <- T],
	[$", "source", $", $:, ${, H, Rest, $}].

-spec ecs_destination(SubDomain) -> iodata()
	when
		SubDomain :: binary() | string().
%% @doc Elastic Common Schema (ECS): Destination attributes.
ecs_destination([] = _SubDomain) ->
	[];
ecs_destination(SubDomain) ->
	Dsubdomain = [$", "subdomain", $", $:, $", SubDomain, $"],
	[$", "destination", $", $:, ${, Dsubdomain, $}].

-spec ecs_service(Name, Type) -> iodata()
	when
		Name :: string(),
		Type :: string().
%% @doc Elastic Common Schema (ECS): Service attributes.
ecs_service(Name, Type) ->
	Sname = case Name of
		[] ->
			[];
		_ ->
			[$,, $", "name", $", $:, $", Name, $"]
	end,
	Stype = case Type of
		[] ->
			[];
		_ ->
			[$,, $", "type", $", $:, $", Type, $"]
	end,
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
		Category :: string(),
		Type :: [string()],
		Outcome :: string().
%% @doc Elastic Common Schema (ECS): Event attributes.
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
	Ekind = case Kind of
		[] ->
			[];
		_ ->
			[$,, $", "kind", $", $:, $", Kind, $"]
	end,
	Ecategory = case Category of
		[] ->
			[];
		_ ->
			[$,, $", "category", $", $:, $[, $", Category, $", $]]
	end,
	Etypes = case Type of
		[] ->
			[];
		[H] ->
			[$", H, $"];
		[H | T] ->
			[$", H, $" | [[$,, $", E, $"] || E <- T]]
	end,
	Etype = case Etypes of
		[] ->
			[];
		_ ->
			[$,, $", "type", $", $:, $[, Etypes, $]]
	end,
	Eoutcome = case Outcome of
		[] ->
			[];
		_ ->
			[$,, $", "outcome", $", $:, $", Outcome, $"]
	end,
	[$", "event", $", $:, ${, Ecreated, Estart, Eend,
			Eduration, Ekind, Ecategory, Etype, Eoutcome, $}].

-spec ecs_user(Name, Id, Domain) -> iodata()
	when
		Name :: string(),
		Id :: string(),
		Domain :: string().
%% @doc Elastic Common Schema (ECS): User attributes.
ecs_user([] = _Name, Id, Domain) ->
	ecs_user1(Id, Domain, []);
ecs_user(Name, Id, Domain) ->
	Uname = [$", "name", $", $:, $", Name, $"],
	ecs_user1(Id, Domain, [Uname]).
%% @hidden
ecs_user1([] = _Id, Domain, Acc) ->
	ecs_user2(Domain, Acc);
ecs_user1(Id, Domain, Acc) ->
	Uid = [$", "id", $", $:, $", Id, $"],
	ecs_user2(Domain, [Uid | Acc]).
%% @hidden
ecs_user2([] = _Domain, Acc) ->
	ecs_user3(Acc);
ecs_user2(Domain, Acc) ->
	Udomain = [$", "domain", $", $:, $", Domain, $"],
	ecs_user3([Udomain | Acc]).
%% @hidden
ecs_user3([]) ->
	[];
ecs_user3(Acc) ->
	[H | T] = lists:reverse(Acc),
	Rest = [[$,, Field] || Field <- T],
	[$", "user", $", $:, ${, H, Rest, $}].

-spec ecs_url(URL) -> iodata()
	when
		URL :: uri_string:uri_string() | uri_string:uri_map().
%% @doc Elastic Common Schema (ECS): URL attributes.
ecs_url([] = _URL) ->
	[];
ecs_url(URL) when is_list(URL) ->
	ecs_url(uri_string:parse(URL));
ecs_url(URL) when is_map(URL) ->
	Acc1 = case maps:get(host, URL, undefined)  of
		undefined ->
			[];
		Host ->
			[[$", "domain", $", $:, $", Host, $"]]
	end,
	Acc2 = case maps:get(port, URL, undefined) of
		undefined ->
			Acc1;
		Port ->
			[[$", "port", $", $:, integer_to_list(Port)] | Acc1]
	end,
	Acc3 = case maps:get(path, URL, undefined) of
		undefined ->
			Acc2;
		Path ->
			[[$", "path", $", $:, $", Path, $"] | Acc2]
	end,
	Acc4 = case maps:get(query, URL, undefined) of
		undefined ->
			Acc3;
		Query ->
			[[$", "query", $", $:, $", Query, $"] | Acc3]
	end,
	Acc5 = case maps:get(scheme, URL, undefined) of
		undefined ->
			Acc4;
		Scheme ->
			[[$", "scheme", $", $:, $", Scheme, $"] | Acc4]
	end,
	Acc6 = case maps:get(fragment, URL, undefined) of
		undefined ->
			Acc5;
		Fragment ->
			[[$", "fragment", $", $:, $", Fragment, $"] | Acc5]
	end,
	Acc7 = case maps:get(userinfo, URL, undefined) of
		undefined ->
			Acc6;
		Userinfo ->
			[[$", "username", $", $:, $", Userinfo, $"] | Acc6]
	end,
	case Acc7 of
		[H] ->
			[$", "url", $", $:, ${, H, $}];
		[H | T] ->
			[[$", "url", $", $:, ${, H | [[$,, E] || E <- T]], $}]
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

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
				when is_binary(Answer), is_binary(Release),
				is_integer(Seconds) ->
			{Answer, Release, integer_to_list(Seconds * 1000000000)};
		{_, {ok, Answer}, {ok, Release}, error}
				when is_binary(Answer), is_binary(Release) ->
			Ms = cgf_log:iso8601(Release) - cgf_log:iso8601(Answer),
			{Answer, Release, integer_to_list(Ms * 1000000)};
		{{ok, Seizure}, error, {ok, Release}, {ok, Seconds}}
				when is_binary(Seizure), is_binary(Release),
				is_integer(Seconds) ->
			{Seizure, Release, integer_to_list(Seconds * 1000000000)};
		{{ok, Seizure}, error, {ok, Release}, error}
				when is_binary(Seizure), is_binary(Release) ->
			Ms = cgf_log:iso8601(Release) - cgf_log:iso8601(Seizure),
			{Seizure, Release, integer_to_list(Ms * 1000000)};
		{{ok, Seizure}, error, error, {ok, Seconds}}
				when is_binary(Seizure), is_integer(Seconds) ->
			{Seizure, [], integer_to_list(Seconds * 1000000000)};
		{{ok, Seizure}, error, error, error} when is_binary(Seizure) ->
			{Seizure, [], []};
		{_, {ok, Answer}, error, {ok, Seconds}}
				when is_binary(Answer), is_integer(Seconds) ->
			{Answer, [], integer_to_list(Seconds * 1000000000)};
		{_, {ok, Answer}, error, error} when is_binary(Answer) ->
			{Answer, [], []};
		{_, _, {ok, Release}, {ok, Seconds}}
				when is_binary(Release), is_integer(Seconds) ->
			{[], Release, integer_to_list(Seconds * 1000000000)};
		{_, _, {ok, Release}, error} when is_binary(Release) ->
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
				when is_binary(Start), is_binary(Stop),
				is_integer(Seconds) ->
			{Start, Stop, integer_to_list(Seconds * 1000000000)};
		{{ok, Start}, {ok, Stop}, error}
				when is_binary(Start), is_binary(Stop) ->
			Ms = cgf_log:iso8601(Start) - cgf_log:iso8601(Stop),
			{Start, Stop, integer_to_list(Ms * 1000000)};
		{{ok, Start}, _, {ok, Seconds}}
				when is_binary(Start), is_integer(Seconds) ->
			{Start, [], integer_to_list(Seconds * 1000000000)};
		{_, {ok, Stop}, {ok, Seconds}}
				when is_binary(Stop), is_integer(Seconds) ->
			{[], Stop, integer_to_list(Seconds * 1000000000)};
		{{ok, Start}, {ok, Stop}, _}
				when is_binary(Start), is_binary(Stop) ->
			{Start, Stop, []};
		{{ok, Start}, _, _} when is_binary(Start) ->
			{Start, [], []};
		{_, {ok, Stop}, _} when is_binary(Stop) ->
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

