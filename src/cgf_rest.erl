%%% cgf_rest.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2022-2025 SigScale Global Inc.
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
%%% @doc This library module implements utility functions
%%% 	for REST servers in the {@link //cgf. cgf} application.
%%%
-module(cgf_rest).
-copyright('Copyright (c) 2022-2025 SigScale Global Inc.').

-export([parse_query/1, range/1, fields/2]).
-export([format_problem/2]).
-export([id/0, etag/1, date/1, iso8601/1]).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The cgf_rest public API
%%----------------------------------------------------------------------

-spec parse_query(Query) -> Result
	when
		Query :: string(),
		Result :: [{Key, Value}],
		Key :: string(),
		Value :: string().
%% @doc Parse the query portion of a URI.
%% @throws {error, 400}
parse_query("?" ++ Query) ->
	parse_query(Query);
parse_query(Query) when is_list(Query) ->
	parse_query(string:tokens(Query, "&"), []).
%% @hidden
parse_query([H | T], Acc) ->
	parse_query(T, parse_query1(H, string:chr(H, $=), Acc));
parse_query([], Acc) ->
	lists:reverse(Acc).
%% @hidden
parse_query1(_Field, 0, _Acc) ->
	throw({error, 400});
parse_query1(Field, N, Acc) ->
	Key = lists:sublist(Field, N - 1),
	Value = lists:sublist(Field, N + 1, length(Field)),
	[{Key, Value} | Acc].

-type uri() :: string().
-type problem() :: #{type := uri(), title := string(),
		code := string(), cause => string(), detail => string(),
		invalidParams => [#{param := string(), reason => string()}],
		status => 200..599}.
-spec format_problem(Problem, Headers) -> Result
	when
		Problem :: problem(),
		Headers :: [tuple()],
		Result :: {ContentType, Body},
		ContentType :: string(),
		Body :: string().
%% @doc Format a problem report in an accepted content type.
%%
%% 	`Problem' MUST contain `type', `title', and `code'.
%% 	RFC7807 specifies `type' as a URI reference to
%% 	human-readable documentation for the problem type.
%% 	Use `title' for a short summary of the problem type.
%% 	TMF630 mandates `code' to provide an application
%% 	related code which may be included in an API
%% 	specification. 3GPP SBI adds `cause' and `invalidParams'.
%%
%% 	The result shall be formatted in one of the following
%% 	media types, in priority order:
%%
%%		ContentType :: "application/problem+json"
%%				| "application/json" | "text/html"
%% @private
format_problem(Problem, Headers) ->
	case lists:keyfind("accept", 1, Headers) of
		{_, Accept} ->
			format_problem1(Problem, string:tokens(Accept, ", "));
		false ->
			format_problem1(Problem, [])
	end.
%% @hidden
format_problem1(Problem, Accepted) ->
	F = fun(AcceptedType) ->
			lists:prefix("application/problem+json", AcceptedType)
	end,
	case lists:any(F, Accepted) of
		true ->
			Type = ["\t\"type\": \"", maps:get(type, Problem), "\",\n"],
			Title = ["\t\"title\": \"", maps:get(title, Problem), "\""],
			Detail = case maps:find(detail, Problem) of
				{ok, Value1} ->
					[",\n\t\"detail\": \"", Value1, "\""];
				error ->
					[]
			end,
			Cause = case maps:find(cause, Problem) of
				{ok, Value2} ->
					[",\n\t\"cause\": \"", Value2, "\""];
				error ->
					[]
			end,
			InvalidParams = case maps:find(invalidParams, Problem) of
				{ok, Value3} ->
					Fold = fun(#{param := P, reason := R}, Acc) ->
								Comma = case length(Acc) of
									0 ->
										[];
									_ ->
										",\n"
								end,
								Param = ["\t\t{\n\t\t\t\"param\": \"", P, "\",\n"],
								Reason = ["\t\t\t\"reason\": \"", R, "\"\n\t\t}"],
								Acc ++ [Comma, Param, Reason];
							(#{param := P}, Acc) ->
								Comma = case length(Acc) of
									0 ->
										[];
									_ ->
										",\n"
								end,
								Param = ["\t\t{\n\t\t\t\"param\": \"", P, "\"\n\t\t}"],
								Acc ++ [Comma, Param]
					end,
					[",\n\t\"invalidParams\": [\n",
							lists:foldl(Fold, [], Value3), "\n\t]"];
				error ->
					[]
			end,
			Status = case maps:find(status, Problem) of
				{ok, Value4} ->
					[",\n\t\"status\": ", integer_to_list(Value4)];
				error ->
					[]
			end,
			Code = case maps:get(code, Problem) of
				C1 when length(C1) > 0 ->
					[",\n\t\"code\": \"", C1, "\"\n"];
				_C1 ->
					[]
			end,
			{"application/problem+json",
					[${, $\n, Type, Title, Detail, Cause,
					InvalidParams, Status, Code, $\n, $}]};
		false ->
			format_problem2(Problem, Accepted)
	end.
%% @hidden
format_problem2(Problem, Accepted) ->
	F = fun(AcceptedType) ->
			lists:prefix("application/json", AcceptedType)
	end,
	case lists:any(F, Accepted) of
		true ->
			Class = "\t\"@type\": \"Error\",\n",
			Type = ["\t\"referenceError\": \"", maps:get(type, Problem), "\",\n"],
			Code = ["\t\"code\": \"", maps:get(code, Problem), "\",\n"],
			Reason = ["\t\"reason\": \"", maps:get(title, Problem), "\""],
			Message = case maps:find(detail, Problem) of
				{ok, Value1} ->
					[",\n\t\"message\": \"", Value1, "\""];
				error ->
					[]
			end,
			Status = case maps:find(status, Problem) of
				{ok, Value2} ->
					[",\n\t\"status\": \"", integer_to_list(Value2), "\""];
				error ->
					[]
			end,
			Body = [${, $\n, Class, Type, Code, Reason, Message, Status, $\n, $}],
			{"application/json", Body};
		false ->
			format_problem3(Problem)
	end.
%% @hidden
format_problem3(Problem) ->
	H1 = "\n\t\t\t<h1>SigScale CGF REST API</h1>\n",
	Paragraph  = "\t\t\t<p>Oops! Something went wrong.</p>\n",
	Header = ["\t\t<header>\n", H1, Paragraph, "\t\t</header>\n"],
	ProblemType = maps:get(type, Problem),
	Link = ["<a href=\"", ProblemType, "\">", ProblemType, "</a>"],
	Type = ["\t\t\t\<dt>Problem Type</dt>\n\t\t\t<dd>",
			Link, "</dd>\n"],
	Title = ["\t\t\t<dt>Title</dt>\n\t\t\t<dd>",
			maps:get(title, Problem), "</dd>\n"],
	Detail = case maps:find(detail, Problem) of
		{ok, Value1} ->
			["\t\t\t<dt>Detail</dt>\n\t\t\t<dd>",
					Value1, "</dd>\n"];
		error ->
			[]
	end,
	Cause = case maps:find(cause, Problem) of
		{ok, Value2} ->
			["\t\t\t<dt>Cause</dt>\n\t\t\t<dd>",
					Value2, "</dd>\n"];
		error ->
			[]
	end,
	InvalidParams = case maps:find(invalidParams, Problem) of
		{ok, Value3} ->
			F = fun(#{param := P, reason := R}, Acc) ->
						Acc ++ ["\t\t\t\t\t<tr>\n",
						"\t\t\t\t\t\t<td>", P, "</td>\n",
						"\t\t\t\t\t\t<td>", R, "</td>\n",
						"\t\t\t\t\t</tr>\n"];
					(#{param := P}, Acc) ->
						Acc ++ ["\t\t\t\t\t<tr>\n",
						"\t\t\t\t\t\t<td>", P, "</td>\n",
						"\t\t\t\t\t</tr>\n"]
			end,
			["\t\t\t<dt>Invalid Parameters</dt>\n\t\t\t<dd>\n",
					"\t\t\t\t<table>\n",
					"\t\t\t\t\t<tr>\n",
					"\t\t\t\t\t\t<th>Parameter</th>\n",
					"\t\t\t\t\t\t<th>Reason</th>\n",
					"\t\t\t\t\t</tr>\n",
					lists:foldl(F, [], Value3),
					"\t\t\t</dd>\n"];
		error ->
			[]
	end,
	Status = case maps:find(status, Problem) of
		{ok, Value4} ->
			["\t\t\t<dt>Status</dt>\n\t\t\t<dd>",
					integer_to_list(Value4), "</dd>\n"];
		error ->
			[]
	end,
	Code = case maps:get(code, Problem) of
		C1 when length(C1) > 0 ->
			["\t\t\t<dt>Code<dt>\n\t\t\t<dd>", C1, "</dd>\n"];
		_C1 ->
			[]
	end,
	Definitions = ["\t\t<dl>\n", Type, Title, Detail, Cause,
			InvalidParams, Status, Code, "\t\t</dl>\n"],
	Body = ["\t<body>\n", Header, Definitions, "\t</body>\n"],
	Head = "\t<head>\n\t\t<title>Error</title>\n\t</head>\n",
	HTML = ["<!DOCTYPE html>\n<html lang=\"en\">\n", Head, Body, "</html>"],
	{"text/html", HTML}.

-spec etag(Etag) -> Etag
	when
		Etag :: string() | {TS, N},
		TS :: pos_integer(),
		N :: pos_integer().
%% @doc Map unique timestamp and HTTP ETag.
etag({TS, N} = _Etag) when is_integer(TS), is_integer(N)->
	integer_to_list(TS) ++ "-" ++ integer_to_list(N);
etag(Etag) when is_list(Etag) ->
	[TS, N] = string:tokens(Etag, "-"),
	{list_to_integer(TS), list_to_integer(N)}.

-spec id() -> ID
	when
		ID :: string().
%% @doc Create uniform unique resource identifier.
id() ->
	TS = erlang:system_time(millisecond),
	N = erlang:unique_integer([positive]),
	integer_to_list(TS) ++ "-" ++ integer_to_list(N).

-spec date(DateTimeFormat) -> Result
	when
		DateTimeFormat	:: pos_integer() | tuple(),
		Result			:: calendar:datetime() | non_neg_integer().
%% @doc Convert iso8610 to date and time or
%%		date and time to timeStamp.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds);
date(DateTime) when is_tuple(DateTime) ->
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH,
	Seconds * 1000.

-spec iso8601(MilliSeconds) -> Result
	when
		MilliSeconds	:: pos_integer() | string() | binary(),
		Result			:: string() | pos_integer().
%% @doc Convert iso8610 to ISO 8601 format date and time.
iso8601(MilliSeconds) when is_integer(MilliSeconds) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(MilliSeconds),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0b",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, MilliSeconds rem 1000]),
	lists:flatten(Chars);
iso8601(ISODateTime) when is_binary(ISODateTime) ->
	iso8601(binary_to_list(ISODateTime));
iso8601(ISODateTime) when is_list(ISODateTime) ->
	case string:rchr(ISODateTime, $T) of
		0 ->
			iso8601(ISODateTime, []);
		N ->
			iso8601(lists:sublist(ISODateTime, N - 1),
				lists:sublist(ISODateTime,  N + 1, length(ISODateTime)))
	end.
%% @hidden
iso8601(Date, Time) when is_list(Date), is_list(Time) ->
	D = iso8601_date(string:tokens(Date, ",-"), []),
	{H, Mi, S, Ms} = iso8601_time(string:tokens(Time, ":."), []),
	date({D, {H, Mi, S}}) + Ms.
%% @hidden
iso8601_date([[Y1, Y2, Y3, Y4] | T], _Acc) ->
	Y = list_to_integer([Y1, Y2, Y3, Y4]),
	iso8601_date(T, Y);
iso8601_date([[M1, M2] | T], Y) when is_integer(Y) ->
	M = list_to_integer([M1, M2]),
	iso8601_date(T, {Y, M});
iso8601_date([[D1, D2] | T], {Y, M}) ->
	D = list_to_integer([D1, D2]),
	iso8601_date(T, {Y, M, D});
iso8601_date([], {Y, M}) ->
	{Y, M, 1};
iso8601_date([], {Y, M, D}) ->
	{Y, M, D}.
%% @hidden
iso8601_time([H1 | T], []) ->
	H = list_to_integer(H1),
	iso8601_time(T, H);
iso8601_time([M1 | T], H) when is_integer(H) ->
	Mi = list_to_integer(M1),
	iso8601_time(T, {H, Mi});
iso8601_time([S1 | T], {H, Mi}) ->
	S = list_to_integer(S1),
	iso8601_time(T, {H, Mi, S});
iso8601_time([], {H, Mi}) ->
	{H, Mi, 0, 0};
iso8601_time([Ms1 | T], {H, Mi, S}) ->
	Ms = list_to_integer(Ms1),
	iso8601_time(T, {H, Mi, S, Ms});
iso8601_time([], {H, Mi, S}) ->
	{H, Mi, S, 0};
iso8601_time([], {H, Mi, S, Ms}) ->
	{H, Mi, S, Ms};
iso8601_time([], []) ->
	{0,0,0,0}.

-spec range(Range) -> Result
	when
		Range :: RHS | {Start, End},
		RHS :: string(),
		Result :: {ok, {Start, End}} | {ok, RHS} | {error, 400},
		Start :: pos_integer(),
		End :: pos_integer().
%% @doc Parse or create a `Range' request header.
%% 	`RHS' should be the right hand side of an
%% 	RFC7233 `Range:' header conforming to TMF630
%% 	(e.g. "items=1-100").
%% @private
range(Range) when is_list(Range) ->
	try
		["items", S, E] = string:tokens(Range, "= -"),
		{ok, {list_to_integer(S), list_to_integer(E)}}
	catch
		_:_ ->
			{error, 400}
	end;
range({Start, End}) when is_integer(Start), is_integer(End) ->
	{ok, "items=" ++ integer_to_list(Start) ++ "-" ++ integer_to_list(End)}.

-spec fields(Filters, JsonObject) -> Result
	when
		Filters :: string(),
		JsonObject :: tuple(),
		Result :: tuple().
%% @doc Filter a JSON object.
%%
%% 	Parses the right hand side of a `fields=' portion of a query
%% 	string and applies those filters on a `JSON' object.
%%
%% 	Each filter in `Filters' is the name of a member in the JSON
%% 	encoded `JsonObject'. A filter may refer to a complex type by
%% 	use of the "dot" path separator character (e.g. `"a.b.c"').
%% 	Where an intermediate node on a complex path is an array all
%% 	matching array members will be included. To filter out objects
%% 	an `=value', suffix may be added which will include only
%% 	objects with a member matching the name and value. Multiple
%% 	values may be provided with `=(value1,value2)'.
%%
%% 	Returns a new JSON object with only the matching items.
%%
%% 	Example:
%% 	```
%% 	1> In = {struct,[{"a",{array,[{struct,[{"name","bob"},{"value",6}]},
%% 	1> {stuct,[{"b",7}]},{struct,[{"name","sue"},{"value",5},{"other", 8}]}]}},{"b",1}]},
%% 	1> cgf_rest:fields("b,a.name=sue,a.value", In).
%% 	{struct, [{"a",{array,[{struct,[{"name","sue"},{"value",5}]}]}},{"b",1}]}
%% 	'''
%%
%% @throws {error, 400}
%%
fields(Filters, JsonObject) when is_list(Filters) ->
	Filters1 = case lists:member($(, Filters) of
		true ->
			expand(Filters, []);
		false ->
			Filters
	end,
	Filters2 = string:tokens(Filters1, ","),
	Filters3 = [string:tokens(F, ".") || F <- Filters2],
	Filters4 = lists:usort(Filters3),
	fields1(Filters4, JsonObject, []).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
fields1(Filters, {array, L}, Acc) ->
	{array, fields2(Filters, L, Acc)};
fields1(Filters, {struct, L}, Acc) ->
	{struct, fields3(Filters, L, false, true, Acc)}.

-spec fields2(Filters, Elements, Acc) -> Result
	when
		Filters :: [list(string())],
		Elements :: [Value],
		Value :: integer() | string() | {struct, Members} | {array, Elements},
		Members :: [{Key, Value}],
		Key :: string(),
		Acc :: [Value],
		Result :: [Value].
%% @doc Process each array element.
%% @hidden
fields2(Filters, [{Type, Value} | T], Acc)
		when Type == struct; Type == array ->
	case fields1(Filters, {Type, Value}, []) of
		{struct, []} ->
			fields2(Filters, T, Acc);
		Object ->
			fields2(Filters, T, [Object | Acc])
	end;
fields2(Filters, [_ | T], Acc) ->
	fields2(Filters, T, Acc);
fields2(_, [], Acc) ->
	lists:reverse(Acc).

-spec fields3(Filters, Members, IsValueMatch, ValueMatched, Acc) -> Result
	when
		Filters :: [list(string())],
		Members :: [{Key, Value}],
		Key :: string(),
		Value :: integer() | string() | {struct, Members} | {array, Elements},
		Elements :: [Value],
		IsValueMatch :: boolean(),
		ValueMatched :: boolean(),
		Acc :: [{Key, Value}],
		Result :: [{Key, Value}].
%% @doc Process each object member.
%% @hidden
fields3(Filters, [{Key1, Value1} | T], IsValueMatch, ValueMatched, Acc) ->
	case fields4(Filters, {Key1, Value1}, false) of
		{false, false} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{true, false} when IsValueMatch == false ->
			fields3(Filters, T, true, false, Acc);
		{true, false} ->
			fields3(Filters, T, true, ValueMatched, Acc);
		{false, {_, {_, []}}} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
		{false, {Key2, Value2}} ->
			fields3(Filters, T, IsValueMatch, ValueMatched, [{Key2, Value2} | Acc]);
		{true, {Key2, Value2}} ->
			fields3(Filters, T, true, true, [{Key2, Value2} | Acc])
	end;
fields3(Filters, [_ | T], IsValueMatch, ValueMatched, Acc) ->
	fields3(Filters, T, IsValueMatch, ValueMatched, Acc);
fields3(_, [], _, false, _) ->
	[];
fields3(_, [], _, true, Acc) ->
	lists:reverse(Acc).

-spec fields4(Filters, Member, IsValueMatch) -> Result
	when
		Filters :: [list(string())],
		Member :: {Key, Value},
		IsValueMatch :: boolean(),
		Key :: string(),
		Value :: integer() | string() | {struct, [Member]} | {array, [Value]},
		Result :: {ValueMatch, MatchResult},
		ValueMatch :: boolean(),
		MatchResult :: false | Member.
%% @doc Apply filters to an object member.
%% @hidden
fields4([[Key] | _], {Key, Value}, IsValueMatch) ->
	{IsValueMatch, {Key, Value}};
fields4([[S] | T], {Key, Value}, IsValueMatch) ->
	case split(S) of
		{Key, Value} ->
			{true, {Key, Value}};
		{Key, _} ->
			fields4(T, {Key, Value}, true);
		_ ->
			fields4(T, {Key, Value}, IsValueMatch)
	end;
fields4([[Key | _ ] | _] = Filters1, {Key, {Type, L}}, IsValueMatch)
		when Type == struct; Type == array ->
	F1 = fun([K | _]) when K =:= Key ->
				true;
			(_) ->
				false
	end,
	Filters2 = lists:takewhile(F1, Filters1),
	F2 = fun([_ | T]) -> T end,
	Filters3 = lists:map(F2, Filters2),
	{IsValueMatch, {Key, fields1(Filters3, {Type, L}, [])}};
fields4([_ | T], {Key, Value}, IsValueMatch) ->
	fields4(T, {Key, Value}, IsValueMatch);
fields4([], _, IsValueMatch) ->
	{IsValueMatch, false}.

%% @hidden
expand("=(" ++ T, Acc) ->
	{Key, NewAcc} = expand1(Acc, []),
	expand2(T, Key, [], NewAcc);
expand([H | T], Acc) ->
	expand(T, [H | Acc]);
expand([], Acc) ->
	lists:reverse(Acc).
%% @hidden
expand1([$, | _] = T, Acc) ->
	{lists:reverse(Acc), T};
expand1([H | T], Acc) ->
	expand1(T, [H | Acc]);
expand1([], Acc) ->
	{lists:reverse(Acc), []}.
%% @hidden
expand2([$) | T], Key, Acc1, Acc2) ->
	Expanded = Acc1 ++ "=" ++ Key,
	expand(T, Expanded ++ Acc2);
expand2([$, | T], Key, Acc1, Acc2) ->
	Expanded = "," ++ Acc1 ++ "=" ++ Key,
	expand2(T, Key, [], Expanded ++ Acc2);
expand2([H | T], Key, Acc1, Acc2) ->
	expand2(T, Key, [H | Acc1], Acc2);
expand2([], _, _, _) ->
	throw({error, 400}).

%% @hidden
split(S) ->
	split(S, []).
%% @hidden
split([$= | T], Acc) ->
	{lists:reverse(Acc), T};
split([H | T], Acc) ->
	split(T, [H | Acc]);
split([], Acc) ->
	lists:reverse(Acc).

