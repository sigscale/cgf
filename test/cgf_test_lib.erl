%%% cgf_test_lib.erl
%%% vim: ts=3
%%%
-module(cgf_test_lib).

-export([rand_dn/0, rand_dn/1]).

%% @doc Returns ten random digits.
rand_dn() ->
	rand_dn(10).

%% @doc Returns N random digits.
rand_dn(N) ->
	rand_dn(N, []).
rand_dn(0, Acc) ->
	Acc;
rand_dn(N, Acc) ->
	rand_dn(N - 1, [47 + rand:uniform(10) | Acc]).

