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
		RecordType :: moCall | mtCall | moSMS | mtSMS | rated,
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
bx([{moCall = _RecordType, Parameters}] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = call_duration(Parameters),
	Outcome = call_outcome(Parameters),
	[${,
			ecs_base(cgf_log:iso8601(erlang:system_time(millisecond))), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_moCall", $", $:, zj:encode(Parameters), $}];
bx([{moCall = _RecordType, Parameters}, {rated, OCS}] = _CDR) ->
	IMSI = imsi(Parameters),
	MSISDN = msisdn(Parameters),
	{StartTime, StopTime, Duration} = call_duration(Parameters),
	Outcome = call_outcome(Parameters),
	[${,
			ecs_base(cgf_log:iso8601(erlang:system_time(millisecond))), $,,
			ecs_service("bx", "cgf"), $,,
			ecs_user(MSISDN, IMSI, []), $,,
			ecs_event(StartTime, StopTime, Duration,
					"event", "session", ["connection"], Outcome), $,,
			$", "Bx_moCall", $", $:, zj:encode(Parameters), $,,
			$", "Bx_rated", $", $:, zj:encode(OCS), $}].

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
	Tags = [$", "tags", $", $:, $[, $]],
	Version = [$", "ecs", $", $:, ${,
			$", "version", $", $:, $", "8.5", $", $}],
	[TS, $,, Labels, $,, Tags, $,, Version].

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
ecs_network(Application, Protocol) ->
	Napplication = [$", "application", $", $:, $", Application, $"],
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
ecs_source(Address, Domain, SubDomain, UserName, UserIds) ->
	Saddress = [$", "address", $", $:, $", Address, $"],
	Sdomain = [$", "domain", $", $:, $", Domain, $"],
	Ssubdomain = [$", "subdomain", $", $:, $", SubDomain, $"],
	{UserId, OtherIds} = case UserIds of
		[H]  ->
			{H, []};
		[H | T] ->
			{H, T};
		[] ->
			{[], []}
	end,
	User = [$", "user", $", $:, ${,
			$", "name", $", $:, $", UserName, $", $,,
			$", "id", $", $:, $", UserId, $", $}],
	Related = [$", "related", $", $:, ${,
			$", "user", $", $:, $[, $", UserId, $",
			[[$,, $", R, $"] || R <- OtherIds], $], $}],
	[$", "source", $", $:, ${, Saddress, $,, Sdomain, $,,
			Ssubdomain, $,, User, $,, Related, $}].

-spec ecs_destination(SubDomain) -> iodata()
	when
		SubDomain :: binary() | string().
%% @doc Elastic Common Schema (ECS): Destination attributes.
ecs_destination(SubDomain) ->
	Dsubdomain = [$", "subdomain", $", $:, $", SubDomain, $"],
	[$", "destination", $", $:, ${, Dsubdomain, $}].

-spec ecs_service(Name, Type) -> iodata()
	when
		Name :: string(),
		Type :: string().
%% @doc Elastic Common Schema (ECS): Service attributes.
ecs_service(Name, Type) ->
	Sname = [$", "name", $", $:, $", Name, $"],
	Stype = [$", "type", $", $:, $", Type, $"],
	Snode = [$", "node", $", $:, ${,
			$", "name", $", $:, $", atom_to_list(node()), $", $}],
	[$", "service", $", $:, ${, Sname, $,, Stype, $,, Snode, $}].

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
	Estart = [$", "start", $", $:, $", Start, $"],
	Eend = [$", "end", $", $:, $", End, $"],
	Eduration = [$", "duration", $", $:, $", Duration, $"],
	Ekind = [$", "kind", $", $:, $", Kind, $"],
	Ecategory = [$", "category", $", $:, $[, $", Category, $", $]],
	Etypes = case Type of
		[H] ->
			[$", H, $"];
		[H | T] ->
			[$", H, $" | [[$,, $", E, $"] || E <- T]]
	end,
	Etype = [$", "type", $", $:, $[, Etypes, $]],
	Eoutcome  = [$", "outcome", $", $:, $", Outcome, $"],
	[$", "event", $", $:, ${,
			Estart, $,, Eend, $,, Eduration, $,,
			Ekind, $,, Ecategory, $,, Etype, $,,
			Eoutcome, $}].

-spec ecs_user(Name, Id, Domain) -> iodata()
	when
		Name :: string(),
		Id :: string(),
		Domain :: string().
%% @doc Elastic Common Schema (ECS): User attributes.
ecs_user(Name, Id, Domain) ->
	Uname = [$", "name", $", $:, $", Name, $"],
	Uid = [$", "id", $", $:, $", Id, $"],
	Udomain = [$", "domain", $", $:, $", Domain, $"],
	[$", "user", $", $:, ${,
			Uname, $,, Uid, $,, Udomain, $}].

-spec ecs_url(URL) -> iodata()
	when
		URL :: uri_string:uri_string() | uri_string:uri_map().
%% @doc Elastic Common Schema (ECS): URL attributes.
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

