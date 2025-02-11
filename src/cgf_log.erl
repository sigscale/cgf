%%% cgf_log.erl
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
%%% @doc This library module implements logging functions for the
%%%   {@link //cgf. cgf} application.
%%%
-module(cgf_log).
-copyright('Copyright (c) 2021 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

%% export the cgf_log public API
-export([open/2, close/1, log/2, blog/2, alog/2, balog/2]).
-export([now/0, date/1, iso8601/1]).

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(EPOCH, 62167219200).

-type log_option() :: disk_log:dlog_option()
		| {codec, {Module :: module(), Function :: atom()}}
		| {process, boolean()}.

%%----------------------------------------------------------------------
%%  The cgf_log public API
%%----------------------------------------------------------------------

-spec open(LogName, Options) -> Result
   when
      LogName :: LocalName | {local, LogName} | {global, GlobalName},
		LocalName :: atom(),
		GlobalName :: term(),
      Options :: [log_option()],
      Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Start a log manager for `LogName'.
%%
%%   An `cgf_log_server' process is created to manage the log.
%%
%%   Optionally `{codec, {Module, Function}}' provides a CODEC
%%   function `Module:Function(Item)' to transform `Item' before
%%   sending to the log.
%%
%%   Option `{process, true}' indicates a process is started to run the CODEC.
%%
open(LogName, Options) when is_atom(LogName) ->
	open({local, LogName}, Options);
open({RegType, Name} = LogName, Options) when is_list(Options),
		(((RegType == local) and is_atom(Name)) or (RegType == global)) ->
	case lists:keymember(name, 1, Options) of
		true ->
			open1(LogName, Options);
		false ->
			open1(LogName, [{name, Name} | Options])
	end.
%% @hidden
open1(LogName, Options) ->
	case supervisor:start_child(cgf_log_sup, [[LogName, Options]]) of
		{ok, _Child} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

-spec close(LogName) -> Result
   when
      LogName :: LocalName | {NodeName, LocalName}
				| {global, GlobalName},
		LocalName :: atom(),
		NodeName :: atom(),
		GlobalName :: term(),
      Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Stop a log manager.
close(LogName)
		when is_atom(LogName); is_tuple(LogName) ->
	case catch gen_server:call(LogName, supervisor) of
		{ok, Sup} ->
			supervisor:terminate_child(cgf_log_sup, Sup);
		{'EXIT', Reason} ->
			{error, Reason}
	end.

-spec log(LogName, Term) -> Result
   when
      LogName :: LocalName | {NodeName, LocalName}
				| {global, GlobalName},
		LocalName :: atom(),
		NodeName :: atom(),
		GlobalName :: term(),
      Term :: term(),
		Result :: ok | {error, Reason},
		Reason :: disk_log:log_error_rsn().
%% @doc Synchronously write `Term' to an open log.
%%
%% 	The log should have been created with `{format, internal}'.
%%
log(LogName, Term)
		when is_atom(LogName); is_tuple(LogName) ->
	gen_server:call(LogName, {log, Term}).

-spec blog(LogName, Term) -> Result
   when
      LogName :: LocalName | {NodeName, LocalName}
				| {global, GlobalName},
		LocalName :: atom(),
		NodeName :: atom(),
		GlobalName :: term(),
      Term :: term() | iodata(),
		Result :: ok | {error, Reason},
		Reason :: disk_log:log_error_rsn().
%% @doc Synchronously wite `Bytes' to an open log.
%%
%% 	The log should have been created with `{format, external}'.
%%
blog(LogName, Term)
		when is_atom(LogName); is_tuple(LogName) ->
	gen_server:call(LogName, {blog, Term}).

-spec alog(LogName, Term) -> ok
   when
      LogName :: LocalName | {NodeName, LocalName}
				| {global, GlobalName},
		LocalName :: atom(),
		NodeName :: atom(),
		GlobalName :: term(),
      Term :: term().
%% @doc Asynchronously write `Term' to an open log.
%%
%% 	The log should have been created with `{format, internal}'.
%%
alog(LogName, Term)
		when is_atom(LogName); is_tuple(LogName) ->
	gen_server:cast(LogName, {alog, Term}).

-spec balog(LogName, Term) -> ok
   when
      LogName :: LocalName | {NodeName, LocalName}
				| {global, GlobalName},
		LocalName :: atom(),
		NodeName :: atom(),
		GlobalName :: term(),
      Term :: term() | iodata().
%% @doc Asynchronously wite `Bytes' to an open log.
%%
%% 	The log should have been created with `{format, external}'.
%%
balog(LogName, Term)
		when is_atom(LogName); is_tuple(LogName) ->
	gen_server:cast(LogName, {balog, Term}).

-spec now() -> ISO8601
	when
		ISO8601 :: string().
%% @doc Returns the current time in ISO 8601 format.
now() ->
	cgf_log:iso8601(erlang:system_time(millisecond)).

-spec date(DateTime) -> DateTime
	when
		DateTime :: MilliSeconds | calendar:datetime(),
		MilliSeconds :: pos_integer().
%% @doc Convert between unix epoch milliseconds and date and time.
date({{_, _, _}, {_, _, _}} = DateTime) ->
	(calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH) * 1000;
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

-spec iso8601(DateTime) -> DateTime
	when
		DateTime :: pos_integer() | string() | binary().
%% @doc Convert between ISO 8601 and Unix epoch milliseconds.
%% 	Parsing is not strict to allow prefix matching.
iso8601(DateTime) when is_binary(DateTime) ->
	iso8601(binary_to_list(DateTime));
iso8601(DateTime) when is_integer(DateTime) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(DateTime),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, DateTime rem 1000]),
	lists:flatten(Chars);
iso8601([Y1, Y2, Y3, Y4 | T])
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
		Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	iso8601month(list_to_integer([Y1, Y2, Y3, Y4]), T).
%% @hidden
iso8601month(Year, []) ->
	DateTime = {{Year, 1, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601month(Year, [$-]) ->
	iso8601month(Year, []);
iso8601month(Year, [$-, $0]) ->
	iso8601month(Year, [$-, $0, $1]);
iso8601month(Year, [$-, $1]) ->
	iso8601month(Year, [$-, $1, $0]);
iso8601month(Year, [$-, M1, M2 | T])
		when M1 >= $0, M1 =< $1, M2 >= $0, M2 =< $9 ->
	iso8601day(Year, list_to_integer([M1, M2]), T).
%% @hidden
iso8601day(Year, Month, []) ->
	DateTime = {{Year, Month, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601day(Year, Month, [$-]) ->
	iso8601day(Year, Month, []);
iso8601day(Year, Month, [$-, $0]) ->
	iso8601day(Year, Month, [$-, $1, $0]);
iso8601day(Year, Month, [$-, D1])
		when D1 >= $1, D1 =< $3 ->
	iso8601day(Year, Month, [$-, D1, $0]);
iso8601day(Year, Month, [$-, D1, D2 | T])
		when D1 >= $0, D1 =< $3, D2 >= $0, D2 =< $9 ->
	Day = list_to_integer([D1, D2]),
	iso8601hour({Year, Month, Day}, T).
%% @hidden
iso8601hour(Date, []) ->
	DateTime = {Date, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601hour(Date, [$T]) ->
	iso8601hour(Date, []);
iso8601hour(Date, [$T, H1])
		when H1 >= $0, H1 =< $2 ->
	iso8601hour(Date, [$T, H1, $0]);
iso8601hour(Date, [$T, H1, H2 | T])
		when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9 ->
	Hour = list_to_integer([H1, H2]),
	iso8601minute(Date, Hour, T).
%% @hidden
iso8601minute(Date, Hour, []) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601minute(Date, Hour, [$:]) ->
	iso8601minute(Date, Hour, []);
iso8601minute(Date, Hour, [$:, M1])
		when M1 >= $0, M1 =< $5 ->
	iso8601minute(Date, Hour, [$:, M1, $0]);
iso8601minute(Date, Hour, [$:, M1, M2 | T])
		when M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9 ->
	Minute = list_to_integer([M1, M2]),
	iso8601second(Date, Hour, Minute, T);
iso8601minute(Date, Hour, _) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601second(Date, Hour, Minute, []) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601second(Date, Hour, Minute, [$:]) ->
	iso8601second(Date, Hour, Minute, []);
iso8601second(Date, Hour, Minute, [$:, S1])
		when S1 >= $0, S1 =< $5 ->
	iso8601second(Date, Hour, Minute, [$:, S1, $0]);
iso8601second(Date, Hour, Minute, [$:, S1, S2 | T])
		when S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
	Second = list_to_integer([S1, S2]),
	DateTime = {Date, {Hour, Minute, Second}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	EpocMilliseconds = (GS - ?EPOCH) * 1000,
	iso8601millisecond(EpocMilliseconds, T);
iso8601second(Date, Hour, Minute, _) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601millisecond(EpocMilliseconds, []) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$.]) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$., N1, N2, N3 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9,
		N3 >= $0, N3 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2, N3]);
iso8601millisecond(EpocMilliseconds, [$., N1, N2 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2]) * 10;
iso8601millisecond(EpocMilliseconds, [$., N | _])
		when N >= $0, N =< $9 ->
	EpocMilliseconds + list_to_integer([N]) * 100;
iso8601millisecond(EpocMilliseconds, _) ->
	EpocMilliseconds.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

