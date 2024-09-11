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

-export([octet_string/1, tbcd/1, bcd_dn/1,
		octet_ip_address/1, ip_address/1, bcd_date_time/1]).

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

-spec tbcd(Digits) -> Digits
	when
		Digits :: [Digit] | binary(),
		Digit :: $0..$9 | $* | $# | $a | $b | $c.
%% @doc CODEC for TBCD-String.
%%
%% 	The Telephony Binary Coded Decimal String (TBCD)
%% 	type is used to store dialing digits in a compact
%% 	form. Digits are "0123456789*#abc"..
tbcd(Digits) when is_binary(Digits) ->
	tbcd(Digits, []);
tbcd(Digits) when is_list(Digits) ->
	tbcd(Digits, <<>>).
%% @hidden
tbcd(<<15:4, A:4>>, Acc) ->
	lists:reverse([digit(A) | Acc]);
tbcd(<<A2:4, A1:4, Rest/binary>>, Acc)
		when A1 >= 0, A1 < 15, A2 >= 0, A2 < 15 ->
	tbcd(Rest, [digit(A2), digit(A1) | Acc]);
tbcd(<<>>, Acc) ->
	lists:reverse(Acc);
tbcd([A], Acc) when ((A >= $0) and (A =< $9)) or (A == $*)
		or (A == $#) or ((A >= $a) and (A =<$c)) ->
	B = digit(A),
	<<Acc/binary, 15:4, B:4>>;
tbcd([A2, A1 | T], Acc)
		when (((A1 >= $0) or (A1 =< $9)) or (A1 == $*)
		or (A1 == $#) or ((A1 >= $a) and (A1 =<$c))),
		(((A2 >= $0) and (A2 =< $9)) or (A2 == $*)
		or (A2 == $#) or ((A2 >= $a) and (A2 =<$c))) ->
	B1 = digit(A1),
	B2 = digit(A2),
	tbcd(T, <<Acc/binary, B1:4, B2:4>>);
tbcd([], Acc) ->
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

-spec octet_ip_address(Octets) -> Address
	when
		Octets :: binary(),
		Address :: string().
%% @doc Converts a `OCTET STRING' to an `IPV4' or an `IPv6' address in string format.
octet_ip_address(<<A:8, B:8, C:8, D:8>>) ->
	inet:ntoa({A, B, C, D});
octet_ip_address(<<H1:16, H2:16, H3:16, H4:16, H5:16, H6:16, H7:16, H8:16>>) ->
	inet:ntoa({H1, H2, H3, H4, H5, H6, H7, H8}).

-spec ip_address(IPAddress) -> Address
	when
		IPAddress :: {iPBinaryAddress, IPBinaryAddress}
				| {iPTextRepresentedAddress, IPTextRepresentedAddress},
		IPBinaryAddress :: {iPBinV4Address, IPBinV4Address}
				| {iPBinV6Address, IPBinV6AddressWithOrWithoutPrefixLength},
		IPBinV4Address :: binary(),
		IPBinV6Address :: binary(),
		IPBinV6AddressWithOrWithoutPrefixLength :: {iPBinV6Address, IPBinV6Address}
				| {iPBinV6AddressWithPrefix, IPBinV6AddressWithPrefixLength},
		IPBinV6AddressWithPrefixLength :: #{iPBinV6Address := IPBinV6Address,
				pDPAddressPrefixLength => PDPAddressPrefixLength},
		PDPAddressPrefixLength :: 0..128,
		IPTextRepresentedAddress :: {iPTextV4Address, string()}
				| {iPTextV6Address, string()},
		Address :: string().
%% @doc Convert an ASN.1 `IPAddress' to an IPv4/IPv6 address in string format.
ip_address({iPBinaryAddress, IPBinaryAddress}) ->
	ip_address1(IPBinaryAddress);
ip_address({iPTextRepresentedAddress, IPTextRepresentedAddress}) ->
	ip_address4(IPTextRepresentedAddress).
%% @hidden
ip_address1({iPBinV4Address, IPBinV4Address}) ->
	cgf_lib:octet_ip_address(IPBinV4Address);
ip_address1({iPBinV6Address, IPBinV6AddressWithOrWithoutPrefixLength}) ->
	ip_address2(IPBinV6AddressWithOrWithoutPrefixLength).
%% @hidden
ip_address2({iPBinV6Address, IPBinV6Address}) ->
	cgf_lib:octet_ip_address(IPBinV6Address);
ip_address2({iPBinV6AddressWithPrefix, IPBinV6AddressWithPrefixLength}) ->
	ip_address3(IPBinV6AddressWithPrefixLength).
%% @hidden
ip_address3(#{iPBinV6Address := IPBinV6Address,
		pDPAddressPrefixLength := PDPAddressPrefixLength}) ->
	cgf_lib:octet_ip_address(IPBinV6Address)
			++ "/" ++ integer_to_list(PDPAddressPrefixLength);
ip_address3(#{iPBinV6Address := IPBinV6Address}) ->
	cgf_lib:octet_ip_address(IPBinV6Address).
%% @hidden
ip_address4({iPTextV4Address, Address}) ->
	Address;
ip_address4({iPTextV6Address, Address}) ->
	Address.

-spec bcd_date_time(Binary) -> ISO6801
	when
		Binary :: binary(),
		ISO6801 :: string().
%% @doc Convert `OCTET STRING' to `date-time'.
bcd_date_time(<<Year2:4, Year1:4, Month2:4, Month1:4, Day2:4, Day1:4,
		Hour2:4, Hour1:4, Minute2:4, Minute1:4, Second2:4, Second1:4,
		Sign:8, TzHour2:4, TzHour1:4, TzMinute2:4, TzMinute1:4>>)
		when Sign == $-; Sign == $+ ->
	io_lib:fwrite("20~b~b-~b~b-~b~bT~b~b:~b~b:~b~b~c~b~b:~b~b",
			[Year2, Year1, Month2, Month1, Day2, Day1,
      Hour2, Hour1, Minute2, Minute1, Second2, Second1,
      Sign, TzHour2, TzHour1, TzMinute2, TzMinute1]).

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

