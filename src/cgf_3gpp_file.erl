%%% cgf_3gpp_file.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 - 2025 SigScale Global Inc.
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
%%% @doc This library module implements utilities for parsing
%%% 	3GPP Charging Data Record (CDR) files in the
%%%   {@link //cgf. cgf} application.
%%% @reference <a href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&amp;GSMSpecPart2=297">
%%% 	3GPP TS 32.297 Charging Data Record (CDR) file format and transfer</a>
%%%
-module(cgf_3gpp_file).
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([file_header/1, cdr_header/1]).

%% export the public types
-export_type([file_header/0, cdr_header/0,
		timestamp/0, close_reason/0, lost_cdr/0,
		rel_ver/0, ts_number/0, cdr_format/0]).

-include("cgf_3gpp_file.hrl").

-type file_header() :: #cdr_file_header{}.
-type cdr_header() :: #cdr_header{}.
-type timestamp() :: {Month :: 1..12, Day :: 1..31,
		Hour :: 0..23, Minute :: 0..59, Offset :: -720..840}.
-type close_reason() :: normal | size | time | count | manual
		| change | undefined | error | space | integrity.
-type lost_cdr() :: {'==', 0..126} | {'>=', 1..127}.
-type rel_ver() :: {Release :: 4..99, Version :: 0..99}.
-type ts_number() :: 32005 | 32015 | 32205 | 32215 | 32225 | 32235 | 32250
				| 32251 | 32260 | 32270 | 32271 | 32272 | 32273 | 32275
				| 32274 | 32277 | 32296 | 32278 | 32253 | 32255 | 32254
				| 32256 | 28201 | 28202 | 32257.
-type cdr_format() :: ber | per | uper | xer.

%%----------------------------------------------------------------------
%%  The cgf_3gpp_file public API
%%----------------------------------------------------------------------

-spec file_header(Header) -> Header
	when
		Header :: binary() | #cdr_file_header{}.
%% @doc CODEC for the CDR file header.
file_header(#cdr_file_header{low = {Lrel, Lver}, high = {Hrel, Hver},
			open = TsOpen, append = TsAppend, count = Count,
			sequence = Sequence, reason = Reason, address = Address,
			lost = {Op, N} = Lost} = Header)
		when is_integer(Lrel), Lrel >= 0, Lrel =< 99,
		is_integer(Lver), Lver >= 0, Lver =< 99,
		is_integer(Hrel), Hrel >= 0, Hrel =< 99,
		is_integer(Hver), Hver >= 0, Hver =< 99,
		tuple_size(TsOpen) == 5, tuple_size(TsAppend) == 5,
		is_integer(Count),Count > 0, Count < 16#FFFFFFFF,
		is_integer(Sequence), Sequence >= 0, Sequence < 16#FFFFFFFF,
		is_atom(Reason), tuple_size(Address) == 8,
		(((Op == '==') andalso ((N >= 0) andalso (N < 127)))
		orelse ((Op == '>=') andalso ((N > 0) andalso (N =< 127)))) ->
	Hrel1 = from_release(Hrel),
	Lrel1 = from_release(Lrel),
	TsOpen1 = timestamp(TsOpen),
	TsAppend1 = timestamp(TsAppend),
	Reason1 = close_reason(Reason),
	Address1 = address(Address),
	Lost1 = lost_cdr(Lost),
	file_header1(Header, <<Hrel1:3, Hver:5, Lrel1:3, Lver:5,
			TsOpen1:4/binary, TsAppend1:4/binary, Count:32,
			Sequence:32, Reason1, Address1:20/binary, Lost1>>);
file_header(<<Hrel:3, Hver:5, Lrel:3, Lver:5, TsOpen:4/binary,
		TsAppend:4/binary, Count:32, Sequence:32, Reason,
		Address:20/binary, Lost, Rest/binary>>) ->
	Header = #cdr_file_header{low = {to_release(Lrel), Lver},
			high = {to_release(Hrel), Hver},
			open = timestamp(TsOpen),
			append = timestamp(TsAppend),
			count = Count, sequence = Sequence,
			reason = close_reason(Reason),
			address = address(Address),
			lost = lost_cdr(Lost)},
	file_header1(Rest, Header).
%% @hidden
file_header1(#cdr_file_header{filter = Filter} = Header, Acc) ->
	FilterLen = byte_size(Filter),
	file_header2(Header, <<Acc/binary,
			FilterLen:16, Filter:FilterLen/binary>>);
file_header1(<<FilterLen:16, Filter:FilterLen/binary,
		Rest/binary>>, Acc) ->
	Acc1 = Acc#cdr_file_header{filter = Filter},
	file_header2(Rest, Acc1);
file_header1(<<>>, Acc) ->
	Acc.
%% @hidden
file_header2(#cdr_file_header{private = Private} = Header, Acc) ->
	PrivateLen = byte_size(Private),
	file_header3(Header, <<Acc/binary,
			PrivateLen:16, Private:PrivateLen/binary>>);
file_header2(<<PrivateLen:16, Private:PrivateLen/binary,
		Rest/binary>>, Acc) ->
	Acc1 = Acc#cdr_file_header{private = Private},
	file_header3(Rest, Acc1);
file_header2(<<>>, Acc) ->
	Acc.
%% @hidden
file_header3(#cdr_file_header{high = {Release, _}} = Header, Acc)
		when Release >= 10, Release /= 99 ->
	HrelExt = Release - 10,
	file_header4(Header, <<Acc/binary, HrelExt>>);
file_header3(#cdr_file_header{} = Header, Acc) ->
	file_header4(Header, Acc);
file_header3(<<HrelExt, Rest/binary>>,
		#cdr_file_header{high = {10, Version}} = Acc) ->
	Acc1 = Acc#cdr_file_header{high = {10 + HrelExt, Version}},
	file_header4(Rest, Acc1);
file_header3(Header, Acc) ->
	file_header4(Header, Acc).
%% @hidden
file_header4(#cdr_file_header{low = {Release, _}}, Acc)
		when Release >= 10, Release /= 99 ->
	LrelExt = Release - 10,
	<<Acc/binary, LrelExt>>;
file_header4(#cdr_file_header{}, Acc) ->
	Acc;
file_header4(<<LrelExt>>,
		#cdr_file_header{low = {10, Version}} = Acc) ->
	Acc#cdr_file_header{low = {10 + LrelExt, Version}};
file_header4(<<>>, Acc) ->
	Acc.

-spec cdr_header(Header) -> Header
	when
		Header :: binary() | #cdr_header{}.
%% @doc CODEC for a CDR header.
cdr_header(#cdr_header{ts = TS,
			release = Release, version = Version,
			format = Format} = _Header)
		when is_integer(Release), Release >= 10, Release /= 99,
		is_integer(Version), is_atom(Format)  ->
	TS1 = ts_number(TS),
	Release1 = from_release(Release),
	ReleaseExt = Release - 10,
	Format1 = cdr_format(Format),
	<<Release1:3, Version:5, Format1:3, TS1:5, ReleaseExt>>;
cdr_header(#cdr_header{ts = TS,
			release = Release, version = Version,
			format = Format})
		when is_integer(Release), Release >= 4, Release =< 99,
		is_integer(Version), is_integer(TS), is_atom(Format)  ->
	TS1 = ts_number(TS),
	Release1 = from_release(Release),
	Format1 = cdr_format(Format),
	<<Release1:3, Version:5, Format1:3, TS1:5>>;
cdr_header(<<Release:3, Version:5, Format:3, TS:5>>)
		when Release < 7 ->
	Release1 = to_release(Release),
	TS1 = ts_number(TS),
	Format1 = cdr_format(Format),
	#cdr_header{ts = TS1, release = Release1,
			version = Version, format = Format1};
cdr_header(<<Release:3, Version:5, Format:3, TS:5, ReleaseExt>>)
		when Release == 7 ->
	Release1 = ReleaseExt + 10,
	TS1 = ts_number(TS),
	Format1 = cdr_format(Format),
	#cdr_header{ts = TS1, release = Release1,
			version = Version, format = Format1}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec timestamp(Timestamp) -> Timestamp
	when
		Timestamp :: binary() | timestamp().
%% @doc CODEC for the timestamp in the file header.
%% @private
timestamp(<<Month:4, Day:5, Hour:5, Minute:6,
		0:1, OffsetHours:5, OffsetMinutes:6>>) ->
	{Month, Day, Hour, Minute, -((OffsetHours * 60) + OffsetMinutes)};
timestamp(<<Month:4, Day:5, Hour:5, Minute:6,
		1:1, OffsetHours:5, OffsetMinutes:6>>) ->
	{Month, Day, Hour, Minute, (OffsetHours * 60) + OffsetMinutes};
timestamp({Month, Day, Hour, Minute, Offset}) when Offset < 0 ->
	OffsetHours = -Offset div 60,
	OffsetMinutes = -Offset rem 60,
	<<Month:4, Day:5, Hour:5, Minute:6, 0:1, OffsetHours:5, OffsetMinutes:6>>;
timestamp({Month, Day, Hour, Minute, Offset}) ->
	OffsetHours = Offset div 60,
	OffsetMinutes = Offset rem 60,
	<<Month:4, Day:5, Hour:5, Minute:6, 1:1, OffsetHours:5, OffsetMinutes:6>>.

-spec close_reason(Reason) -> Reason
	when
		Reason :: byte() | close_reason().
%% @doc CODEC for the file closure reason in the file header.
%% @private
close_reason(0) ->
	normal;
close_reason(1) ->
	size;
close_reason(2) ->
	time;
close_reason(3) ->
	count;
close_reason(4) ->
	manual;
close_reason(5) ->
	change;
close_reason(128) ->
	undefined;
close_reason(129) ->
	error;
close_reason(130) ->
	space;
close_reason(131) ->
	integrity;
close_reason(normal) ->
	0;
close_reason(size) ->
	1;
close_reason(time) ->
	2;
close_reason(count) ->
	3;
close_reason(manual) ->
	4;
close_reason(change) ->
	5;
close_reason(undefined) ->
	128;
close_reason(error) ->
	129;
close_reason(space) ->
	130;
close_reason(integrity) ->
	131.

-spec address(Address) -> Address
	when
		Address :: binary() | inet:ip6_address().
%% @doc CODEC for the address in the file header.
address({N1, N2, N3, N4, N5, N6, N7, N8} = _Address) ->
	<<16#ffffffff:32, N1:16, N2:16, N3:16, N4:16,
			N5:16, N6:16, N7:16, N8:16>>;
address(<<_:32, N1:16, N2:16, N3:16, N4:16,
		N5:16, N6:16, N7:16, N8:16>> = _Address) ->
	{N1, N2, N3, N4, N5, N6, N7, N8}.

-spec lost_cdr(Lost) -> Lost
	when
		Lost :: byte() | lost_cdr().
%% @doc CODEC for the lost CDR indicator in the file header.
%% @private
lost_cdr(0) ->
	{'==', 0};
lost_cdr(N) when N =< 127 ->
	{'>=', N};
lost_cdr(128) ->
	{'>=', 1};
lost_cdr(N) when N < 255 ->
	{'==', N - 128};
lost_cdr(255) ->
	{'>=', 127};
lost_cdr({'==', 0}) ->
	0;
lost_cdr({'>=', 1}) ->
	128;
lost_cdr({'>=', N}) when N =< 127 ->
	N;
lost_cdr({'==', N}) when N < 127 ->
	N + 128;
lost_cdr({'>=', 127}) ->
	255.

-spec to_release(N) -> 4..99
	when
		N :: byte().
%% @doc Parse 3GPP release number from file header.
to_release(0) ->
	99;
to_release(1) ->
	4;
to_release(2) ->
	5;
to_release(3) ->
	6;
to_release(4) ->
	7;
to_release(5) ->
	8;
to_release(6) ->
	9;
to_release(7) ->
	10.

-spec from_release(N) -> 4..99
	when
		N :: byte().
%% @doc Encode 3GPP release number for file header.
from_release(99) ->
	0;
from_release(4) ->
	1;
from_release(5) ->
	2;
from_release(6) ->
	3;
from_release(7) ->
	4;
from_release(8) ->
	5;
from_release(9) ->
	6;
from_release(10) ->
	7;
from_release(_) ->
	7.

-spec ts_number(N) -> ts_number()
	when
		N :: 0..25.
%% @doc CODEC for the 3GPP "middle tier" technical specification in the CDR header.
ts_number(0) ->
	32005;
ts_number(1) ->
	32015;
ts_number(2) ->
	32205;
ts_number(3) -> 
	32215;
ts_number(4) ->
	32225;
ts_number(5) ->
	32235;
ts_number(6) ->
	32250;
ts_number(7) ->
	32251;
ts_number(9) ->
	32260;
ts_number(10) ->
	32270;
ts_number(11) ->
	32271;
ts_number(12) ->
	32272;
ts_number(13) ->
	32273;
ts_number(14) ->
	32275;
ts_number(15) ->
	32274;
ts_number(16) ->
	32277;
ts_number(17) ->
	32296;
ts_number(18) ->
	32278;
ts_number(19) ->
	32253;
ts_number(20) ->
	32255;
ts_number(21) ->
	32254;
ts_number(22) ->
	32256;
ts_number(23) ->
	28201;
ts_number(24) ->
	28202;
ts_number(25) ->
	32257;
ts_number(32005) ->
	0;
ts_number(32015) ->
	1;
ts_number(32205) ->
	2;
ts_number(32215) ->
	3;
ts_number(32225) ->
	4;
ts_number(32235) ->
	5;
ts_number(32250) ->
	6;
ts_number(32251) ->
	7;
ts_number(32260) ->
	9;
ts_number(32270) ->
	10;
ts_number(32271) ->
	11;
ts_number(32272) ->
	12;
ts_number(32273) ->
	13;
ts_number(32275) ->
	14;
ts_number(32274) ->
	15;
ts_number(32277) ->
	16;
ts_number(32296) ->
	17;
ts_number(32278) ->
	18;
ts_number(32253) ->
	19;
ts_number(32255) ->
	20;
ts_number(32254) ->
	21;
ts_number(32256) ->
	22;
ts_number(28201) ->
	23;
ts_number(28202) ->
	24;
ts_number(32257) ->
	25.

-spec cdr_format(Format) -> Format
	when
		Format :: 1..4 | ber | uper | per | xer.
%% @doc CODEC for data record format in the CDR header.
cdr_format(1 = _Format) ->
	ber;
cdr_format(2) ->
	uper;
cdr_format(3) ->
	per;
cdr_format(4) ->
	xer;
cdr_format(ber) ->
	1;
cdr_format(uper) ->
	2;
cdr_format(per) ->
	3;
cdr_format(xer) ->
	4.

