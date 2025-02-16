%%% user_default.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2025 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the"License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an"AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This module implements short form commands for the Erlang shell.
%%%
%%% 	The functions in this module are available when called without
%%% 	a module name in an interactive shell. These are meant to be used
%%% 	by operations support staff to quickly get status information
%%% 	about a running system.
%%%
%%% @see //stdlib/shell_default.
%%%
-module(user_default).
-copyright('Copyright (c) 2016 - 2025 SigScale Global Inc.').

%% export the user_default public API
-export([help/0, ts/0, td/0, su/0]).

-define(MAX_HEAP_SIZE, 1000000).

%%----------------------------------------------------------------------
%%  The user_default public API
%%----------------------------------------------------------------------

-spec help() -> true.
%% @doc Get help on shell local commands.
help() ->
	shell_default:help(),
	io:fwrite("**cgf commands ** \n"),
	io:fwrite("ts()               -- table sizes\n"),
	io:fwrite("td()               -- table distribution\n"),
	io:fwrite("su()               -- scheduler utilization\n"),
	true.

-spec ts() -> ok.
%% @doc Display the total number of records in cgf tables.
ts() ->
	case mnesia:system_info(is_running)  of
		yes ->
			ts00(tables(), 0, []);
		no ->
			io:fwrite("mnesia is not running\n");
		starting ->
			io:fwrite("mnesia is starting\n");
		stopping ->
			io:fwrite("mnesia is stopping\n")
	end.
%% @hidden
ts00([H | T], Max, Acc) ->
	NewMax = case length(atom_to_list(H)) of
		N when N > Max ->
			N;
		_ ->
			Max
	end,
	ts00(T, NewMax, [H | Acc]);
ts00([], Max, Acc) ->
	ts01(lists:reverse(Acc), Max, 0, []).
%% @hidden
ts01([H | T], NameLen, Max, Acc) ->
	Size = mnesia:table_info(H, size),
	NewMax = case length(integer_to_list(Size)) of
		N when N > Max ->
			N;
		_ ->
			Max
	end,
	ts01(T, NameLen, NewMax, [{H, Size} | Acc]);
ts01([], NameLen, Max, Acc) ->
	io:fwrite("Table sizes:\n"),
	ts02(lists:reverse(Acc), NameLen, Max).
%% @hidden
ts02([{Name, Size} | T], NameLen, SizeLen) ->
	io:fwrite("~*s: ~*b\n", [NameLen + 4, Name, SizeLen, Size]),
	ts02(T, NameLen, SizeLen);
ts02([], _, _) ->
	ok.

-spec td() -> ok.
%% @doc Display the current cgf table distribution.
td() ->
	Nodes = mnesia:system_info(db_nodes),
	Running = mnesia:system_info(running_db_nodes),
	io:fwrite("Table distribution:\n"),
	io:fwrite("    Nodes:\n"),
	io:fwrite("        running:  ~s\n", [snodes(Running)]),
	io:fwrite("        stopped:  ~s\n", [snodes(Nodes -- Running)]),
	io:fwrite("    Tables:\n"),
	td0(tables()).
%% @hidden
td0([H | T]) ->
	io:fwrite("        ~s:\n", [H]),
	case mnesia:table_info(H, disc_copies) of
		[] ->
			ok;
		Nodes1 ->
			io:fwrite("            disc_copies:  ~s\n", [snodes(Nodes1)])
	end,
	case mnesia:table_info(H, ram_copies) of
		[] ->
			ok;
		Nodes2 ->
			io:fwrite("             ram_copies:  ~s\n", [snodes(Nodes2)])
	end,
	td0(T);
td0([]) ->
	ok.

-spec su() -> ok.
%% @doc Display scheduler utilization.
su() ->
	case cgf:statistics(scheduler_utilization) of
		{ok, {Etag, Interval, Report}} ->
			io:fwrite("Scheduler utilization:\n"),
			su0(Report, Etag, Interval);
		{error, Reason} ->
			exit(Reason)
	end.
%% @hidden
su0([{Scheduler, Utilization} | T], Etag, Interval) ->
	io:fwrite("~*b: ~3b%\n", [5, Scheduler, Utilization]),
	su0(T, Etag, Interval);
su0([], Etag, Interval) ->
	{TS, _} = string:take(Etag, lists:seq($0, $9)),
	Next = (list_to_integer(TS) + Interval),
	Remaining = Next - erlang:system_time(millisecond),
	Seconds = case {Remaining div 1000, Remaining rem 1000} of
		{0, _} ->
			1;
		{N, R} when R < 500 ->
			N;
		{N, _} ->
			N + 1
	end,
	io:fwrite("Next report available in ~b seconds.\n", [Seconds]).

%%----------------------------------------------------------------------
%%  the user_default private api
%%----------------------------------------------------------------------

%% @hidden
tables() ->
	tables(mnesia:system_info(tables), []).
%% @hidden
tables([H | T], Acc) ->
	case proplists:is_defined(cgf, mnesia:table_info(H, user_properties)) of
		true ->
			tables(T, [H | Acc]);
		false ->
			tables(T, Acc)
	end;
tables([], Acc) ->
	lists:reverse(Acc).

%% @hidden
snodes(Nodes) ->
	snodes(Nodes, []).
%% @hidden
snodes([H | T], []) ->
	snodes(T, [atom_to_list(H)]);
snodes([H | T], Acc) ->
	snodes(T, [atom_to_list(H), ", " | Acc]);
snodes([], Acc) ->
	lists:reverse(Acc).
