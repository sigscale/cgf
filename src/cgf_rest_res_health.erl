%%% cgf_rest_res_health.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022 - 2025 SigScale Global Inc.
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
%%% @doc This library module implements resource handling functions
%%% 	for a REST server in the {@link //cgf. cgf} application.
%%%
%%% This module reports on the health of the system.
%%%
%%% @reference <a href="https://tools.ietf.org/id/draft-inadarei-api-health-check-06.html">
%%% 	Health Check Response Format for HTTP APIs</a>
%%%
-module(cgf_rest_res_health).
-copyright('Copyright (c) 2022 - 2025 SigScale Global Inc.').

-export([content_types_accepted/0, content_types_provided/0,
		get_health/2, head_health/2,
		get_applications/2, head_applications/2,
		get_application/2, head_application/2]).

-ifdef(OTP_RELEASE).
	-if(?OTP_RELEASE >= 28).
		-define(JSON, json).
	-else.
		-define(JSON, zj).
	-endif.
-else.
	-define(JSON, zj).
-endif.

-spec content_types_accepted() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provide list of resource representations accepted.
content_types_accepted() ->
	[].

-spec content_types_provided() -> ContentTypes
	when
		ContentTypes :: [string()].
%% @doc Provides list of resource representations available.
content_types_provided() ->
	["application/health+json", "application/problem+json"].

-spec get_health(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for
%% 	`GET /health'
%% 	requests.
get_health([] = _Query, _RequestHeaders) ->
	try
		Applications = application([cgf,
				mnesia, asn1, ssh, inets]),
		Checks1 = #{"application" => Applications},
		F = fun(#{"componentId" := "cgf"}) ->
					true;
				(_) ->
					false
		end,
		Status = case lists:search(F, Applications) of
			{value, #{"status" := S}} ->
				S;
			false ->
				undefined
		end,
		Checks2 = Checks1#{"table:size" => table_size([cgf_action])},
		Checks3 = Checks2#{"uptime" => up()},
		case scheduler() of
			{ok, HeadOptions, Scheds} ->
				{HeadOptions, Status,
						Checks3#{"scheduler:utilization" => Scheds}};
			{error, _Reason1} ->
				{[], Status, Checks3}
		end
	of
		{CacheControl, "up" = _Status, Checks} ->
			Health = #{"status" => "pass",
					"serviceId" => atom_to_list(node()),
					"description" => "Health of SigScale CGF",
					"checks" => Checks},
			ResponseBody = ?JSON:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}
					| CacheControl],
			{ok, ResponseHeaders, ResponseBody};
		{_CacheControl, _Status, Checks} ->
			Health = #{"status" => "fail",
					"serviceId" => atom_to_list(node()),
					"description" => "Health of SigScale CGF",
					"checks" => Checks},
			ResponseBody = ?JSON:encode(Health),
			ResponseHeaders = [{content_type, "application/health+json"}],
			{error, 503, ResponseHeaders, ResponseBody}
	catch
		_:_Reason2 ->
			{error, 500}
	end.

-spec head_health(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: [].
%% @doc Body producing function for
%% 	`HEAD /health'
%% 	requests.
head_health([] = _Query, _RequestHeaders) ->
	try
		Applications = application([cgf]),
		Checks = #{"application" => Applications},
		Status = case hd(Applications) of
			#{"status" := S} ->
				S;
			_ ->
				undefined
		end,
		case scheduler() of
			{ok, HeadOptions, _Scheds} ->
				{HeadOptions, Status, Checks};
			{error, _Reason1} ->
				{[], Status, Checks}
		end
	of
		{CacheControl, "up" = _Status, _Checks} ->
			ResponseHeaders = [{content_type, "application/health+json"}
					| CacheControl],
			{ok, ResponseHeaders, []};
		{_CacheControl, _Status, _Checks} ->
			ResponseHeaders = [{content_type, "application/health+json"}],
			{error, 503, ResponseHeaders, []}
	catch
		_:_Reason2 ->
			{error, 500}
	end.

-spec get_applications(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for
%% 	`GET /health/application'
%% 	requests.
get_applications([] = _Query, _RequestHeaders) ->
	try
		application([cgf, asn1, ssh, inets])
	of
		Applications ->
			F = fun(#{"status" := "up"}) ->
						false;
					(#{"status" := "down"}) ->
						true
			end,
			case lists:any(F, Applications) of
				false ->
					Application = #{"status" => "pass",
							"serviceId" => atom_to_list(node()),
							"description" => "OTP applications",
							"checks" => [#{"application" => Applications}]},
					ResponseBody = ?JSON:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{ok, ResponseHeaders, ResponseBody};
				true ->
					Application = #{"status" => "fail",
							"serviceId" => atom_to_list(node()),
							"description" => "OTP applications",
							"checks" => [#{"application" => Applications}]},
					ResponseBody = ?JSON:encode(Application),
					ResponseHeaders = [{content_type, "application/health+json"}],
					{error, 503, ResponseHeaders, ResponseBody}
			end
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec head_applications(Query, RequestHeaders) -> Result
	when
		Query :: [{Key :: string(), Value :: string()}],
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: [].
%% @doc Body producing function for
%% 	`HEAD /health/application'
%% 	requests.
head_applications([] = _Query, _RequestHeaders) ->
	try
		application([cgf, asn1, ssh, inets])
	of
		Applications ->
			F = fun(#{"status" := "up"}) ->
						false;
					(#{"status" := "down"}) ->
						true
			end,
			case lists:any(F, Applications) of
				false ->
					ResponseHeaders = [{content_type, "application/health+json"}],
					{ok, ResponseHeaders, []};
				true ->
					ResponseHeaders = [{content_type, "application/health+json"}],
					{error, 503, ResponseHeaders, []}
			end
	catch
		_:_Reason ->
			{error, 500}
	end.

-spec get_application(Id, RequestHeaders) -> Result
	when
		Id :: string(),
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for
%% 	`GET /health/application/{Id}'
%% 	requests.
get_application(Id, _RequestHeaders) ->
	try
		Running = application:which_applications(),
		case lists:keymember(list_to_existing_atom(Id), 1, Running) of
			true ->
				Application = #{"status" => "up", "serviceId" => Id},
				ResponseHeaders = [{content_type, "application/health+json"}],
				ResponseBody = ?JSON:encode(Application),
				{ok, ResponseHeaders, ResponseBody};
			false ->
				Application = #{"status" => "down", "serviceId" => Id},
				ResponseHeaders = [{content_type, "application/health+json"}],
				ResponseBody = ?JSON:encode(Application),
				{error, 503, ResponseHeaders, ResponseBody}
		end
	catch
		_:badarg ->
			{error, 404};
		_:_Reason ->
			{error, 500}
	end.

-spec head_application(Id, RequestHeaders) -> Result
	when
		Id :: string(),
		RequestHeaders :: [tuple()],
		Result :: {ok, ResponseHeaders, ResponseBody}
				| {error, 503, ResponseHeaders, ResponseBody},
		ResponseHeaders :: [tuple()],
		ResponseBody :: iolist().
%% @doc Body producing function for
%% 	`HEAD /health/application/{Id}'
%% 	requests.
head_application(Id, _RequestHeaders) ->
	try
		Running = application:which_applications(),
		case lists:keymember(list_to_existing_atom(Id), 1, Running) of
			true ->
				ResponseHeaders = [{content_type, "application/health+json"}],
				{ok, ResponseHeaders, []};
			false ->
				ResponseHeaders = [{content_type, "application/health+json"}],
				{error, 503, ResponseHeaders, []}
		end
	catch
		_:badarg ->
			{error, 404};
		_:_Reason ->
			{error, 500}
	end.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec scheduler() -> Result
	when
		Result :: {ok, HeadOptions, Components} | {error, Reason},
		HeadOptions :: [{Option, Value}],
		Option :: etag | cache_control,
		Value :: string(),
		Components :: [map()],
		Reason :: term().
%% @doc Check scheduler component.
%% @hidden
scheduler() ->
	scheduler(cgf:statistics(scheduler_utilization)).
scheduler({ok, {Etag, Interval, Report}}) ->
	[TS, _] = string:tokens(Etag, [$-]),
	Next = case (list_to_integer(TS) + Interval)
			- erlang:system_time(millisecond) of
		N when N =< 0 ->
			0;
		N when (N rem 1000) >= 500 ->
			(N div 1000) + 1;
		N ->
			N div 1000
	end,
	MaxAge = "max-age=" ++ integer_to_list(Next),
	HeadOptions = [{etag, Etag}, {cache_control, MaxAge}],
	F = fun({SchedulerId, Utilization}) ->
				#{"componentId" => integer_to_list(SchedulerId),
						"observedValue" => Utilization,
						"observedUnit" => "percent",
						"componentType" => "system"}
	end,
	Components = lists:map(F, Report),
	{ok, HeadOptions, Components};
scheduler({error, Reason}) ->
	{error, Reason}.

-spec application(Names) -> Check
	when
		Names :: [atom()],
		Check :: [map()].
%% @doc Check application component.
%% @hidden
application(Names) ->
	application(Names, application:which_applications(), []).
%% @hidden
application([Name | T], Running, Acc) ->
	Status = case lists:keymember(Name, 1, Running) of
		true ->
			"up";
		false ->
			"down"
	end,
	Application = #{"componentId" => atom_to_list(Name),
			"componentType" => "component",
			"status" => Status},
	application(T, Running, [Application | Acc]);
application([], _Running, Acc) ->
	lists:reverse(Acc).

-spec table_size(Names) -> Check
	when
		Names :: [atom()],
		Check :: [map()].
%% @doc Check table component size.
%% @hidden
table_size(Names) ->
	table_size(Names, []).
%% @hidden
table_size([Name | T], Acc) ->
	TableSize = #{"componentId" => atom_to_list(Name),
			"componentType" => "component",
			"observedUnit" => "rows",
			"observedValue" => mnesia:table_info(Name, size)},
	table_size(T, [TableSize | Acc]);
table_size([], Acc) ->
	lists:reverse(Acc).

-spec up() -> Time
	when
		Time :: [map()].
%% @doc Check uptime in seconds.
%% @hidden
up() ->
	CurrentTime = erlang:system_time(second),
	StartTime = erlang:convert_time_unit(erlang:system_info(start_time)
			+  erlang:time_offset(), native, second),
	Uptime = CurrentTime - StartTime,
	[#{"componentType" =>"system",
			"observedUnit" => "s",
			"observedValue" => Uptime}].

