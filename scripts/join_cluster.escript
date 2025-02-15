#!/usr/bin/env escript
%% vim: syntax=erlang

main([Node]) ->
	Nodes = mnesia:system_info(db_nodes),
	case length(Nodes) of
		N when N > 0 ->
			case mnesia:set_master_nodes(Nodes) of
				ok ->
					ok;
				{error, Reason1} ->
					stopped = mnesia:stop(),
					io:fwrite("error: ~w~n", [Reason1]),
					erlang:halt(1)
			end;
		0 ->
			ok
	end,
	case cgf_app:join(list_to_atom(Node)) of
		{ok, Tables} ->
			io:fwrite("{ok, ~p}~n", [Tables]);
		{error, Reason2} ->
			io:fwrite("error: ~w~n", [Reason2]),
			erlang:halt(1)
	end.

