%%% cgf_tap.erl
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
%%% @doc This library module implements GSMA TAP file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
-module(cgf_tap).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([import/2, import/3]).

-include_lib("kernel/include/logger.hrl").
-include("TAP-0312.hrl").

%%----------------------------------------------------------------------
%%  The cgf_tap public API
%%----------------------------------------------------------------------

-spec import(Filename, Log) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv import(Filename, Log, [])
import(Filename, Log) ->
	import(Filename, Log, []).

-spec import(Filename, Log, Metadata) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import CSV file and write to Bx interface log.
import(Filename, Log, Metadata)
		when is_list(Filename), is_list(Metadata) ->
	case file:read_file(Filename) of
		{ok, Bin} ->
			import1(Filename, Log, Metadata,
					'TAP-0312':decode('DataInterChange', Bin));
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
import1(Filename, Log, Metadata, {ok, {transferBatch, TransferBatch}}) ->
	#'TransferBatch'{callEventDetails = CDRs} =  TransferBatch,
	parse(Filename, Log, Metadata, CDRs);
import1(_Filename, _Log, _Metadata, {ok, {notification, _Notification}}) ->
	{error, not_implemented};
import1(Filename, _Log, _Metadata, {error, Reason}) ->
	?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
			{filename, Filename},
			{error, Reason}]).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Filename, Log, Metadata, CDRs) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDRs :: [tuple()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse CDRs from the import file.
%% @private
parse(Filename, Log, Metadata,
		[{mobileOriginatedCall, MobileOriginatedCall} | T]) ->
	case parse_mo_call(Log, Metadata, MobileOriginatedCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{mobileTerminatedCall, MobileTerminatedCall} | T]) ->
	case parse_mt_call(Log, Metadata, MobileTerminatedCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{supplServiceEvent, SupplServiceEvent} | T]) ->
	case parse_mmtel(Log, Metadata, SupplServiceEvent) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{serviceCentreUsage, ServiceCentreUsage} | T]) ->
	case parse_sc_sm(Log, Metadata, ServiceCentreUsage) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{gprsCall, GprsCall} | T]) ->
	case parse_gprs(Log, Metadata, GprsCall) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{contentTransaction, ContentTransaction} | T]) ->
	case parse_content(Log, Metadata, ContentTransaction) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{locationService, LocationService} | T]) ->
	case parse_location(Log, Metadata, LocationService) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{messagingEvent, MessagingEvent} | T]) ->
	case parse_message(Log, Metadata, MessagingEvent) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(Filename, Log, Metadata,
		[{mobileSession, MobileSession} | T]) ->
	case parse_session(Log, Metadata, MobileSession) of
		ok ->
			parse(Filename, Log, Metadata, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse(_Filename, _Log, _Metadata, []) ->
	ok.

-spec parse_mo_call(Log, Metadata, MOC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOC :: #'MobileOriginatedCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile originated call.
parse_mo_call(Log, Metadata, MOC) ->
	Call1 = basic_call_info(MOC#'MobileOriginatedCall'.basicCallInformation, #{}),
	CDR = [{roam_moCall, Call1} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_call(Log, Metadata, MTC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTC :: #'MobileTerminatedCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile terminated call.
parse_mt_call(Log, Metadata, MTC) ->
	Call1 = basic_call_info(MTC#'MobileTerminatedCall'.basicCallInformation, #{}),
	CDR = [{roam_mtCall, Call1} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mmtel(Log, Metadata, SupplServiceEvent) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SupplServiceEvent :: #'SupplServiceEvent'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a supplementary service.
parse_mmtel(_Log, _Metadata, _SupplServiceEvent) ->
	{error, not_implemented}.

-spec parse_sc_sm(Log, Metadata, ServiceCentreUsage) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ServiceCentreUsage :: #'ServiceCentreUsage'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a service center usage.
parse_sc_sm(_Log, _Metadata, _ServiceCentreUsage) ->
	{error, not_implemented}.

-spec parse_gprs(Log, Metadata, GPRS) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		GPRS :: #'GprsCall'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a GPRS session.
parse_gprs(Log, Metadata, GPRS) ->
	Call1 = basic_call_info(GPRS#'GprsCall'.gprsBasicCallInformation, #{}),
	CDR = [{roam_gprs, Call1} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_content(Log, Metadata, ContentTransaction) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ContentTransaction :: #'ContentTransaction'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a content transaction.
parse_content(_Log, _Metadata, _ContentTransaction) ->
	{error, not_implemented}.

-spec parse_location(Log, Metadata, LocationService) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		LocationService :: #'LocationService'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a location service.
parse_location(_Log, _Metadata, _LocationService) ->
	{error, not_implemented}.

-spec parse_message(Log, Metadata, MessagingEvent) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MessagingEvent :: #'MessagingEvent'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a messaging event.
parse_message(_Log, _Metadata, _MessagingEvent) ->
	{error, not_implemented}.

-spec parse_session(Log, Metadata, MobileSession) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MobileSession :: #'MobileSession'{},
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile session.
parse_session(_Log, _Metadata, _MobileSession) ->
	{error, not_implemented}.

%% @hidden
basic_call_info(#'MoBasicCallInformation'{
		chargeableSubscriber = Subscriber,
		destination = Destination,
		callEventStartTimeStamp = TimeStamp,
      totalCallEventDuration = Duration,
      causeForTerm = Cause},
		Call) ->
	I1 = subscriber(Subscriber, #{}),
	I2 = destination(Destination, I1),
	I3 = call_event_start(TimeStamp, I2),
	I4 = call_event_duration(Duration, I3),
	I5 = cause_for_term(Cause, I4),
	Call#{<<"basicCallInformation">> => I5};
basic_call_info(#'MtBasicCallInformation'{
		chargeableSubscriber = Subscriber,
		callOriginator = Origination,
		callEventStartTimeStamp = TimeStamp,
      totalCallEventDuration = Duration,
      causeForTerm = Cause},
		Call) ->
	I1 = subscriber(Subscriber, #{}),
	I2 = origination(Origination, I1),
	I3 = call_event_start(TimeStamp, I2),
	I4 = call_event_duration(Duration, I3),
	I5 = cause_for_term(Cause, I4),
	Call#{<<"basicCallInformation">> => I5};
basic_call_info(#'GprsBasicCallInformation'{
		gprsChargeableSubscriber = GprsSubscriber,
		callEventStartTimeStamp = TimeStamp,
      totalCallEventDuration = Duration,
      causeForTerm = Cause},
		Call) ->
	I1 = gprs_subscriber(GprsSubscriber, #{}),
	I2 = call_event_start(TimeStamp, I1),
	I3 = call_event_duration(Duration, I2),
	I4 = cause_for_term(Cause, I3),
	Call#{<<"gprsBasicCallInformation">> => I4};
basic_call_info(asn1_NOVALUE, Call) ->
	Call.

%% @hidden
subscriber({simChargeableSubscriber,
		#'SimChargeableSubscriber'{imsi = IMSI, msisdn = MSISDN}},
		Acc) ->
	S1 = case IMSI of
		asn1_NOVALUE ->
			#{};
		_ ->
			#{<<"imsi">> => bcd(IMSI)}
	end,
	S2 = case MSISDN of
		asn1_NOVALUE ->
			S1;
		_ ->
			S1#{<<"msisdn">> => bcd(MSISDN)}
	end,
	Acc#{<<"chargeableSubscriber">> =>
			#{<<"simChargeableSubscriber">> => S2}};
subscriber({minChargeableSubscriber,
		#'MinChargeableSubscriber'{min = MIN, mdn = MDN}},
		Acc) ->
	S1 = case MIN of
		asn1_NOVALUE ->
			#{};
		_ ->
			#{<<"min">> => MIN}
	end,
	S2 = case MDN of
		asn1_NOVALUE ->
			S1;
		_ ->
			S1#{<<"mdn">> => MDN}
	end,
	Acc#{<<"chargeableSubscriber">> =>
			#{<<"minChargeableSubscriber">> => S2}};
subscriber(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
gprs_subscriber(#'GprsChargeableSubscriber'{
		chargeableSubscriber = Subscriber},
		Acc) when is_tuple(Subscriber) ->
	Acc#{<<"gprsChargeableSubscriber">> => subscriber(Subscriber, #{})};
gprs_subscriber(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
destination(#'Destination'{calledNumber = CalledNumber,
		dialledDigits = DialledDigits,
		sMSDestinationNumber = SMSDestination}, Acc) ->
	D1 = case CalledNumber of
		asn1_NOVALUE ->
			#{};
		_ ->
			#{<<"calledNumber">> => bcd(CalledNumber)}
	end,
	D2 = case DialledDigits of
		asn1_NOVALUE ->
			D1;
		_ ->
			D1#{<<"dialledDigits">> => DialledDigits}
	end,
	D3 = case SMSDestination of
		asn1_NOVALUE ->
			D2;
		_ ->
			D2#{<<"sMSDestinationNumber">> => SMSDestination}
	end,
	Acc#{<<"destination">> => D3};
destination(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
origination(#'CallOriginator'{callingNumber = CallingNumber,
		sMSOriginator = SMSOrigination}, Acc) ->
	D1 = case CallingNumber of
		asn1_NOVALUE ->
			#{};
		_ ->
			#{<<"callingNumber">> => bcd(CallingNumber)}
	end,
	D2 = case SMSOrigination of
		asn1_NOVALUE ->
			D1;
		_ ->
			D1#{<<"sMSOriginator">> => SMSOrigination}
	end,
	Acc#{<<"callOriginator">> => D2};
origination(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
call_event_start(#'DateTime'{} = DateTime, Acc) ->
	Acc#{<<"callEventStartTimeStamp">> => timestamp(DateTime)};
call_event_start(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
call_event_duration(Duration, Acc) when is_integer(Duration) ->
	Acc#{<<"totalCallEventDuration">> => Duration};
call_event_duration(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
cause_for_term(Cause, Acc) when is_integer(Cause) ->
	Acc#{<<"causeForTerm">> => Cause};
cause_for_term(asn1_NOVALUE, Acc) ->
	Acc.

%% @hidden
bcd(Binary) ->
	bcd(Binary, []).
%% @hidden
bcd(<<D:4, 15:4>>, Acc) ->
	bcd1([D | Acc], []);
bcd(<<D1:4, D2:4>>, Acc) ->
	bcd1([D2, D1 | Acc], []);
bcd(<<D1:4, D2:4, Rest/binary>>, Acc) ->
	bcd(Rest, [D2, D1 | Acc]).
%% @hidden
bcd1([0 | T], Acc) ->
	bcd1(T, [$0 | Acc]);
bcd1([1 | T], Acc) ->
	bcd1(T, [$1 | Acc]);
bcd1([2 | T], Acc) ->
	bcd1(T, [$2 | Acc]);
bcd1([3 | T], Acc) ->
	bcd1(T, [$3 | Acc]);
bcd1([4 | T], Acc) ->
	bcd1(T, [$4 | Acc]);
bcd1([5 | T], Acc) ->
	bcd1(T, [$5 | Acc]);
bcd1([6 | T], Acc) ->
	bcd1(T, [$6 | Acc]);
bcd1([7 | T], Acc) ->
	bcd1(T, [$7 | Acc]);
bcd1([8 | T], Acc) ->
	bcd1(T, [$8 | Acc]);
bcd1([9 | T], Acc) ->
	bcd1(T, [$9 | Acc]);
bcd1([10 | T], Acc) ->
	bcd1(T, [$a | Acc]);
bcd1([11 | T], Acc) ->
	bcd1(T, [$b | Acc]);
bcd1([12 | T], Acc) ->
	bcd1(T, [$c | Acc]);
bcd1([13 | T], Acc) ->
	bcd1(T, [$d | Acc]);
bcd1([14 | T], Acc) ->
	bcd1(T, [$e | Acc]);
bcd1([], Acc) ->
	Acc.

%% @hidden
timestamp(#'DateTime'{localTimeStamp = <<Year:4/binary, Month:2/binary,
		Day:2/binary, Hour:2/binary, Minute:2/binary, Second:2/binary>>,
		utcTimeOffsetCode = Z}) ->
	<<Year/binary, $-, Month/binary, $-, Day/binary, $T, Hour/binary,
			$:, Minute/binary, $:, Second/binary>>.

