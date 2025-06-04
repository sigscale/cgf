%%% cgf_test_lib.erl
%%% vim: ts=3
%%%
-module(cgf_test_lib).

-export([init_tables/0]).
-export([start/0, start/1, stop/0, stop/1]).
-export([load/1, unload/1]).
-export([rand_dn/0, rand_dn/1]).
-export([rand_imsi/0, rand_imsi/1]).
-export([rand_imei/0, rand_imei/1]).
-export([rand_ip/0, rand_ipv4/0, rand_ipv6/0]).
-export([rand_duration/0, rand_duration/1]).

applications() ->
	[crypto, mnesia, asn1, public_key, sasl, os_mon, inets, ssh, cgf].

-spec init_tables() -> Result
	when
		Result :: ok | {error, Reason :: term()}.
%% @doc Initialize mnesia tabless.
init_tables() ->
	case cgf_app:install() of
		{ok, _Installed} ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

start() ->
	start(applications()).

start([H | T]) ->
	case application:start(H) of
		ok  ->
			start(T);
		{error, {already_started, H}} ->
			start(T);
		{error, Reason} ->
			{error, Reason}
	end;
start([]) ->
	ok.

stop() ->
	stop(lists:reverse(applications())).

stop([H | T]) ->
	case application:stop(H) of
		ok  ->
			stop(T);
		{error, {not_started, H}} ->
			stop(T);
		{error, Reason} ->
			{error, Reason}
	end;
stop([]) ->
	ok.

load(Application) ->
	case application:load(Application) of
		ok ->
			ok;
		{error, {already_loaded, Application}} ->
			ok = unload(Application),
			load(Application);
		{error, {running, Application}} ->
			ok = application:stop(Application),
			ok = unload(Application),
			load(Application)
	end.

unload(Application) ->
	case application:unload(Application) of
		ok ->
			ok;
		{error, {running, Application}} ->
			ok = application:stop(Application),
			unload(Application);
		{error, {not_loaded, Application}} ->
			ok
	end.

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

%% @doc Returns a random IMSI.
rand_imsi() ->
	MCCMNC = "001001",
	rand_imsi(MCCMNC).

%% @doc Returns a random IMSI in MCCMNC.
rand_imsi(MCCMNC) ->
	MCCMNC ++ rand_dn(9).

%% @doc Returns a random IMEI.
rand_imei() ->
	TAC = "01130000",
	rand_imei(TAC).

%% @doc Returns a random IMEI in TAC.
rand_imei(TAC) ->
	luhn_check(TAC ++ rand_dn(6)).

%% @doc Returns a random `inet:ip_address()' value.
rand_ip() ->
	case rand:uniform(2) of
		1 ->
			rand_ipv4();
		2 ->
			rand_ipv6()
	end.

%% @doc Returns a random `inet:ip4_address()' value.
rand_ipv4() ->
	case rand:uniform(3) of
		1 ->
			{10, rand:uniform(255), rand:uniform(255), rand:uniform(255)};
		2 ->
			{172, 15 + rand:uniform(16), rand:uniform(255), rand:uniform(255)};
		3 ->
			{192, 168, rand:uniform(255), rand:uniform(255)}
	end.

%% @doc Returns a random `inet:ip6_address()' value.
rand_ipv6() ->
	inet:ipv4_mapped_ipv6_address(rand_ipv4()).

%% @doc Calculate and append check digit.
luhn_check(Digits) ->
	F = fun(C, {0, Sum}) ->
				{1, Sum + (C - $0)};
			(C, {1, Sum}) when (C - $0) < 5 ->
				{0, Sum + ((C - $0) * 2)};
			(C, {1, Sum}) ->
				{0, Sum + ((C - $0) * 2) - 9}
	end,
	{_, S} = lists:foldr(F, {1, 0}, Digits),
	Digits ++ [((10 - (S rem 10)) rem 10) + $0].

%% @doc Returns a random call duration value.
rand_duration() ->
	rand_duration(100).
rand_duration(N) when N < 50 ->
	rand:uniform(360);
rand_duration(N) when N < 95 ->
	rand:uniform(3600);
rand_duration(_) ->
	rand:uniform(7200).

