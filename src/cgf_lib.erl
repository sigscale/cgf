%% cgf_lib.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
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
%%% @doc This library module implements common functions in the
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_lib).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([octet_string/1, bcd_dn/1, octet_ip_address/1]).

%%----------------------------------------------------------------------
%%  The cgf_lib public API
%%----------------------------------------------------------------------

-spec octet_string(Octets) -> String
	when
		Octets :: binary(),
		String :: binary().
%% @doc Convert an `OCTET STRING' to hexadecimal characters.
octet_string(Octets) ->
	octet_string(Octets, <<>>).
%% @hidden
octet_string(<<UpperNibble:4, LowerNibble:4, Rest/binary>>, Acc) ->
	Hex1 = hex(UpperNibble),
	Hex2 = hex(LowerNibble),
	Acc1 = <<Acc/binary, Hex1, Hex2>>,
	octet_string(Rest, Acc1);
octet_string(<<>>, Acc) ->
	Acc.

-spec bcd_dn(BCD) -> PartyAddress
	when
		BCD :: binary(),
		PartyAddress :: #{Attribute => Value},
		Attribute :: binary(),
		Value :: binary().
%% @doc Decode binary coded decimal (BCD) directory number (DN).
%%
%% 	Decode the ASN.1 type `BCDDirectoryNumber' in 3GPP TS 32.298.
%% 	The encoding is as for the information elements Connected Number
%% 	and Called Party BCD Number in 3GPP TS 24.008.
%%
bcd_dn(<<1:1, TON:3, NPI:4, Rest/binary>> = _BCD) ->
	PA = #{<<"natureOfAddress">> => type_of_number(TON),
			<<"numberingPlan">> => numbering_plan(NPI),
			<<"address">> => <<>>},
	bcd_dn(Rest, PA).
%% @hidden
bcd_dn(<<2#1111:4, N:4>>,
		#{<<"address">> := Address} = Acc) ->
	D = digit(N),
	Acc#{<<"address">> => <<Address/binary, D>>};
bcd_dn(<<N2:4, N1:4, Rest/binary>>,
		#{<<"address">> := Address} = Acc) ->
	D1 = digit(N1),
	D2 = digit(N2),
	Acc1 = Acc#{<<"address">> => <<Address/binary, D1, D2>>},
	bcd_dn(Rest, Acc1);
bcd_dn(<<>>, Acc) ->
	Acc.

-spec octet_ip_address(Octets) -> String
	when
		Octets :: binary(),
		String :: list().
%% @doc Converts a `OCTET STRING' to an `IPV4' or an `IPv6' address in string format.
octet_ip_address(<<A:8, B:8, C:8, D:8>>) ->
	inet:ntoa({A, B, C, D});
octet_ip_address(<<H1:16, H2:16, H3:16, H4:16, H5:16, H6:16, H7:16, H8:16>> ->)
	inet:ntoa({H1, H2, H3, H4, H5, H6, H7, H8}).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
type_of_number(0) ->
	<<"unknown">>;
type_of_number(1) ->
	<<"international">>;
type_of_number(2) ->
	<<"national">>;
type_of_number(_) ->
	<<"reserved">>.

%% @hidden
numbering_plan(0) ->
	<<"unknown">>;
numbering_plan(1) ->
	<<"e164">>;
numbering_plan(3) ->
	<<"x121">>;
numbering_plan(8) ->
	<<"national">>;
numbering_plan(9) ->
	<<"private">>;
numbering_plan(_) ->
	<<"reserved">>.

%% @hidden
digit(0) ->
	$0;
digit(1) ->
	$1;
digit(2) ->
	$2;
digit(3) ->
	$3;
digit(4) ->
	$4;
digit(5) ->
	$5;
digit(6) ->
	$6;
digit(7) ->
	$7;
digit(8) ->
	$8;
digit(9) ->
	$9;
digit(10) ->
	$*;
digit(11) ->
	$#;
digit(12) ->
	$A;
digit(13) ->
	$B;
digit(14) ->
	$C.

%% @hidden
hex(0) ->
	$0;
hex(1) ->
	$1;
hex(2) ->
	$2;
hex(3) ->
	$3;
hex(4) ->
	$4;
hex(5) ->
	$5;
hex(6) ->
	$6;
hex(7) ->
	$7;
hex(8) ->
	$8;
hex(9) ->
	$9;
hex(10) ->
	$a;
hex(11) ->
	$b;
hex(12) ->
	$c;
hex(13) ->
	$d;
hex(14) ->
	$e;
hex(15) ->
	$f.

