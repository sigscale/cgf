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
%%% @doc This library module implements GSMA TAP3 file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% @reference The GSM Association (<a href="https://www.gsma.com/">GSMA</a>)
%%% 	specifies Transferred Accounts Procedure version 3
%%% 	(<a href="https://www.gsma.com/get-involved/working-groups/interoperability-data-specifications-and-settlement-group/standardised-b2b-interfaces-specified-by-ids/open-standards-specifications/tap3-open-standard-download-form">TAP3</a>):
%%% 	<ul>
%%% 		<li>GSMA TD.57 - TAP 3.12 Format Specification</li>
%%% 		<li>GSMA TD.58 - TAP 3.12 Implementation Handbook</li>
%%% 	</ul>.
%%%
-module(cgf_tap).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([import/2, import/3]).

-include_lib("kernel/include/logger.hrl").

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
%% @doc Import TAP3 file and write to Bx interface log.
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
import1(Filename, Log, Metadata, {ok, {transferBatch,
		#{callEventDetails := CDRs, accountingInfo := AccountingInfo}}}) ->
	parse(Filename, Log, Metadata, AccountingInfo, CDRs);
import1(Filename, Log, Metadata, {ok, {transferBatch,
		#{callEventDetails := CDRs, accountingInfo := AccountingInfo}}, _}) ->
	parse(Filename, Log, Metadata, AccountingInfo, CDRs);
import1(_Filename, _Log, _Metadata, {ok, {notification, _Notification}}) ->
	{error, not_implemented};
import1(Filename, _Log, _Metadata, {error, Reason}) ->
	?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
			{filename, Filename},
			{error, Reason}]).

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Filename, Log, Metadata, AccountingInfo, CDRs) -> Result
	when
		Filename :: file:filename(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDRs :: [tuple()],
		AccountingInfo :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse CDRs from the import file.
%% @private
parse(Filename, Log, Metadata, AccountingInfo, CDRs) ->
	case parse_accounting(AccountingInfo) of
		#{} = AccountingMap ->
			parse1(Filename, Log, Metadata, AccountingMap, CDRs);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
parse1(Filename, Log, Metadata, AccountingMap,
		[{mobileOriginatedCall, MobileOriginatedCall} | T]) ->
	case parse_mo_call(Log, [{roam_accountingInfo, AccountingMap} | Metadata], MobileOriginatedCall) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap,T );
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{mobileTerminatedCall, MobileTerminatedCall} | T]) ->
	case parse_mt_call(Log, [{roam_accountingInfo, AccountingMap} | Metadata], MobileTerminatedCall) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{supplServiceEvent, SupplServiceEvent} | T]) ->
	case parse_mmtel(Log, [{roam_accountingInfo, AccountingMap} | Metadata], SupplServiceEvent) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{serviceCentreUsage, ServiceCentreUsage} | T]) ->
	case parse_sc_sm(Log, [{roam_accountingInfo, AccountingMap} | Metadata], ServiceCentreUsage) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{gprsCall, GprsCall} | T]) ->
	case parse_gprs(Log, [{roam_accountingInfo, AccountingMap} | Metadata], GprsCall) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{contentTransaction, ContentTransaction} | T]) ->
	case parse_content(Log, [{roam_accountingInfo, AccountingMap} | Metadata], ContentTransaction) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{locationService, LocationService} | T]) ->
	case parse_location(Log, [{roam_accountingInfo, AccountingMap} | Metadata], LocationService) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{messagingEvent, MessagingEvent} | T]) ->
	case parse_message(Log, [{roam_accountingInfo, AccountingMap} | Metadata], MessagingEvent) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(Filename, Log, Metadata, AccountingMap,
		[{mobileSession, MobileSession} | T]) ->
	case parse_session(Log, [{roam_accountingInfo, AccountingMap} | Metadata], MobileSession) of
		ok ->
			parse1(Filename, Log, Metadata, AccountingMap, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{filename, Filename},
					{error, Reason}]),
			{error, Reason}
	end;
parse1(_Filename, _Log, _Metadata, _AccountingMap, []) ->
	ok.

-spec parse_mo_call(Log, Metadata, MOC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOC :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile originated call.
parse_mo_call(Log, Metadata, MOC) ->
	Call = mobile_originated_call(MOC),
	CDR = [{roam_moCall, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_call(Log, Metadata, MTC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTC :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile terminated call.
parse_mt_call(Log, Metadata, MTC) ->
	Call = mobile_terminated_call(MTC),
	CDR = [{roam_mtCall, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mmtel(Log, Metadata, SupplServiceEvent) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SupplServiceEvent :: map(),
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
		ServiceCentreUsage :: map(),
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
		GPRS :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a GPRS session.
parse_gprs(Log, Metadata, GPRS) ->
	Call = gprs_call(GPRS),
	CDR = [{roam_gprs, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_content(Log, Metadata, ContentTransaction) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ContentTransaction :: map(),
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
		LocationService :: map(),
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
		MessagingEvent :: map(),
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
		MobileSession :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile session.
parse_session(_Log, _Metadata, _MobileSession) ->
	{error, not_implemented}.

-spec parse_accounting(AccountingInfo) -> Result
	when
		AccountingInfo :: map(),
		Result :: #{currencyConversionInfo => [map()],
			discounting => map(),
			localCurrency => list(),
			tapCurrency => list(),
			tapDecimalPlaces => integer(),
			taxation => [map()]}| {error, Reason},
		Reason :: term().
%% @doc Parse Accounting Info from the import file.
%% @private
parse_accounting(AccountingInfo) ->
	parse_accounting(AccountingInfo, #{}).
%% @hidden
parse_accounting(#{currencyConversionInfo := CurrencyConversionInfo} = AI, Acc) ->
	case parse_cci(CurrencyConversionInfo) of
		ConvertedCurrencyInfo when is_list(ConvertedCurrencyInfo) ->
			parse_accounting1(AI,
				Acc#{currencyConversionInfo => ConvertedCurrencyInfo});
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{error, Reason}]),
			{error, Reason}
	end;
parse_accounting(AI, Acc) ->
	parse_accounting1(AI, Acc).
%% @hidden
parse_accounting1(#{auditControlInfo := AuditControlInfo} = AI, Acc) ->
	case parse_discounting(AuditControlInfo) of
		#{} = Discounting ->
			parse_accounting2(AI,
				Acc#{discounting => Discounting});
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{error, Reason}]),
			{error, Reason}
	end;
parse_accounting1(AI, Acc) ->
	parse_accounting2(AI, Acc).
%% @hidden
parse_accounting2(#{localCurrency := LocalCurrency} = AI, Acc)
		when is_binary(LocalCurrency) ->
	parse_accounting3(AI,
		Acc#{localCurrency => binary_to_list(LocalCurrency)});
parse_accounting2(AI, Acc) ->
	parse_accounting3(AI, Acc).
%% @hidden
parse_accounting3(#{tapCurrency := TapCurrency} = AI, Acc)
		when is_binary(TapCurrency) ->
	parse_accounting4(AI,
		Acc#{tapCurrency => binary_to_list(TapCurrency)});
parse_accounting3(AI, Acc) ->
	parse_accounting4(AI, Acc).
%% @hidden
parse_accounting4(#{tapDecimalPlaces := TapDecimalPlaces} = AI, Acc)
		when TapDecimalPlaces > 0 ->
	parse_accounting5(AI,
		Acc#{tapCurrency => TapDecimalPlaces});
parse_accounting4(AI, Acc) ->
	parse_accounting5(AI, Acc).
%% @hidden
parse_accounting5(#{taxation := Taxation} = _AI, Acc) ->
	case parse_tax(Taxation) of
		TaxValue when is_list(TaxValue) ->
			Acc#{taxation => TaxValue};
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, ?FUNCTION_NAME},
					{error, Reason}]),
			{error, Reason}
	end;
parse_accounting5(_AI, Acc) ->
	Acc.

-spec parse_cci(CCI) -> Result
	when
		CCI :: [map()],
		Result :: map().
%% @doc Parse Converted Currency Info from Import File
%% @private
parse_cci(CCI) when is_list(CCI) ->
	parse_cci(CCI, []).
parse_cci([H | T], Acc) ->
	parse_cci(T, [parse_cci1(H, #{}) | Acc]);
parse_cci([], Acc) ->
	Acc.
%% @hidden
parse_cci1(#{exchangeRate := ExchangeRate} = CCI, Acc)
		when ExchangeRate > 0 ->
	parse_cci2(CCI, Acc#{exchangeRate => ExchangeRate});
parse_cci1(CCI, Acc) ->
	parse_cci2(CCI, Acc).
%% @hidden
parse_cci2(#{exchangeRateCode := ExchangeRateCode} = CCI, Acc)
		when ExchangeRateCode > 0  ->
	parse_cci3(CCI, Acc#{exchangeRateCode => ExchangeRateCode});
parse_cci2(CCI, Acc) ->
	parse_cci3(CCI, Acc).
%% @hidden
parse_cci3(#{numberOfDecimalPlaces := NoDecimalPlaces} = _CCI, Acc)
		when NoDecimalPlaces > 0  ->
	Acc#{numberOfDecimalPlaces => NoDecimalPlaces};
parse_cci3(_CCI, Acc) ->
	Acc.

-spec parse_discounting(Discount) -> Result
	when
		Discount :: map(),
		Result :: map().
%% @doc Parse Discounting Info from Import File
%% @private
parse_discounting(Discount) when is_map(Discount) ->
	parse_discounting(Discount, #{}).
%% @hidden
parse_discounting(#{totalDiscountValue := TotalValue} = Discount, Acc)
		when TotalValue > 0 ->
	parse_discounting1(Discount, Acc#{discountableAmount => TotalValue});
parse_discounting(Discount, Acc) ->
	parse_discounting1(Discount, Acc).
%% @hidden
parse_discounting1(#{discountCode := DisCode}, Acc)
		when is_integer(DisCode) ->
	Acc#{discountCode => DisCode};
parse_discounting1(_Discount, Acc) ->
	Acc.

-spec parse_tax(TaxationList) -> Result
	when
		TaxationList :: [map()],
		Result :: map().
%% @doc Parse Taxation from Import File
%% @private
parse_tax(TL) when is_list(TL) ->
	parse_tax(TL, []).
%% @hidden
parse_tax([H | T], Acc) ->
	parse_tax(T, [parse_tax1(H, #{}) | Acc]);
parse_tax([], Acc) ->
	Acc.
%% @hidden
parse_tax1(#{chargeType := ChargeType} = TaxationList, Acc) ->
	parse_tax2(TaxationList, Acc#{chargeType => ChargeType});
parse_tax1(TaxationList, Acc) ->
	parse_tax2(TaxationList, Acc).
%% @hidden
parse_tax2(#{taxCode := TaxCode} = TaxationList, Acc) ->
	parse_tax3(TaxationList, Acc#{taxCode => TaxCode});
parse_tax2(TaxationList, Acc) ->
	parse_tax3(TaxationList, Acc).
%% @hidden
parse_tax3(#{taxRate := TaxRate} = TaxationList, Acc)
		when TaxRate > 0 ->
	parse_tax4(TaxationList, Acc#{taxRate => TaxRate});
parse_tax3(TaxationList, Acc) ->
	parse_tax4(TaxationList, Acc).
%% @hidden
parse_tax4(#{taxType := TaxType} = TaxationList, Acc)
		when TaxType > 0 ->
	parse_tax5(TaxationList, Acc#{taxType => TaxType});
parse_tax4(TaxationList, Acc) ->
	parse_tax5(TaxationList, Acc).
%% @hidden
parse_tax5(#{taxIndicator := TaxIndicator} = _TaxationList, Acc)
		when is_list(TaxIndicator) ->
	Acc#{taxIndicator => TaxIndicator};
parse_tax5(_TaxationList, Acc) ->
	Acc.

%% @hidden
mobile_originated_call(#{basicCallInformation
		:= MoBasicCallInformation} = MobileOriginatedCall) ->
	BCI = mo_basic_call_information(MoBasicCallInformation),
	Acc = #{<<"basicCallInformation">> => BCI},
	mobile_originated_call1(MobileOriginatedCall, Acc);
mobile_originated_call(MobileOriginatedCall) ->
	mobile_originated_call1(MobileOriginatedCall, #{}).
%% @hidden
mobile_originated_call1(#{locationInformation
		:= LocationInformation} = MobileOriginatedCall, Acc) ->
	LI = location_information(LocationInformation),
	Acc1 = Acc#{<<"locationInformation">> => LI},
	mobile_originated_call2(MobileOriginatedCall, Acc1);
mobile_originated_call1(MobileOriginatedCall, Acc) ->
	mobile_originated_call2(MobileOriginatedCall, Acc).
%% @hidden
mobile_originated_call2(#{basicServiceUsedList
		:= BasicServiceUsedList} = _MobileOriginatedCall, Acc) ->
	BSUL = [basic_service_used(BSU) || BSU <- BasicServiceUsedList],
	Acc#{<<"basicServiceUsedList">> => BSUL};
mobile_originated_call2(_MobileOriginatedCall, Acc) ->
	Acc.

%% @hidden
mobile_terminated_call(#{basicCallInformation
		:= MtBasicCallInformation} = MobileTerminatedCall) ->
	BCI = mt_basic_call_information(MtBasicCallInformation),
	Acc = #{<<"basicCallInformation">> => BCI},
	mobile_terminated_call1(MobileTerminatedCall, Acc);
mobile_terminated_call(MobileTerminatedCall) ->
	mobile_terminated_call1(MobileTerminatedCall, #{}).
%% @hidden
mobile_terminated_call1(#{locationInformation
		:= LocationInformation} = MobileTerminatedCall, Acc) ->
	LI = location_information(LocationInformation),
	Acc1 = Acc#{<<"locationInformation">> => LI},
	mobile_terminated_call2(MobileTerminatedCall, Acc1);
mobile_terminated_call1(MobileTerminatedCall, Acc) ->
	mobile_terminated_call2(MobileTerminatedCall, Acc).
%% @hidden
mobile_terminated_call2(#{basicServiceUsedList
		:= BasicServiceUsedList} = _MobileTerminatedCall, Acc) ->
	BSUL = [basic_service_used(BSU) || BSU <- BasicServiceUsedList],
	Acc#{<<"basicServiceUsedList">> => BSUL};
mobile_terminated_call2(_MobileTerminatedCall, Acc) ->
	Acc.

%% @hidden
gprs_call(#{gprsBasicCallInformation
		:= GprsBasicCallInformation} = GprsCall) ->
	BCI = gprs_basic_call_information(GprsBasicCallInformation),
	Acc = #{<<"gprsBasicCallInformation">> => BCI},
	gprs_call1(GprsCall, Acc);
gprs_call(GprsCall) ->
	gprs_call1(GprsCall, #{}).
%% @hidden
gprs_call1(#{gprsLocationInformation
		:= GprsLocationInformation} = GprsCall, Acc) ->
	Acc1 = Acc#{<<"gprsLocationInformation">> => GprsLocationInformation},
	gprs_call2(GprsCall, Acc1);
gprs_call1(GprsCall, Acc) ->
	gprs_call2(GprsCall, Acc).
%% @hidden
gprs_call2(#{gprsServiceUsed := GprsServiceUsed} = _GprsCall, Acc) ->
	GSU = gprs_service_used(GprsServiceUsed),
	Acc#{<<"gprsServiceUsed">> => GSU};
gprs_call2(_GprsCall, Acc) ->
	Acc.

%% @hidden
mo_basic_call_information(#{chargeableSubscriber := ChargeableSubscriber}
		= MoBasicCallInformation) ->
	CS = chargeable_subscriber(ChargeableSubscriber),
	Acc = #{<<"chargeableSubscriber">> => CS},
	mo_basic_call_information1(MoBasicCallInformation, Acc);
mo_basic_call_information(MoBasicCallInformation) ->
	mo_basic_call_information1(MoBasicCallInformation, #{}).
%% @hidden
mo_basic_call_information1(#{destination := Destination}
		= MoBasicCallInformation, Acc) ->
	Dest = destination(Destination),
	Acc1 = Acc#{<<"destination">> => Dest},
	mo_basic_call_information2(MoBasicCallInformation, Acc1);
mo_basic_call_information1(MoBasicCallInformation, Acc) ->
	mo_basic_call_information2(MoBasicCallInformation, Acc).
%% @hidden
mo_basic_call_information2(#{callEventStartTimeStamp := CallEventStartTimeStamp}
		= MoBasicCallInformation, Acc) ->
	TS = timestamp(CallEventStartTimeStamp),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	mo_basic_call_information3(MoBasicCallInformation, Acc1);
mo_basic_call_information2(MoBasicCallInformation, Acc) ->
	mo_basic_call_information3(MoBasicCallInformation, Acc).
%% @hidden
mo_basic_call_information3(#{totalCallEventDuration := TotalCallEventDuration}
		= MoBasicCallInformation, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	mo_basic_call_information4(MoBasicCallInformation, Acc1);
mo_basic_call_information3(MoBasicCallInformation, Acc) ->
	mo_basic_call_information4(MoBasicCallInformation, Acc).
%% @hidden
mo_basic_call_information4(#{causeForTerm := CauseForTerm}, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
mo_basic_call_information4(_MoBasicCallInformation, Acc) ->
	Acc.

%% @hidden
mt_basic_call_information(#{chargeableSubscriber := ChargeableSubscriber}
		= MtBasicCallInformation) ->
	CS = chargeable_subscriber(ChargeableSubscriber),
	Acc = #{<<"chargeableSubscriber">> => CS},
	mt_basic_call_information1(MtBasicCallInformation, Acc);
mt_basic_call_information(MtBasicCallInformation) ->
	mt_basic_call_information1(MtBasicCallInformation, #{}).
%% @hidden
mt_basic_call_information1(#{callOriginator := CallOriginator}
		= MtBasicCallInformation, Acc) ->
	CO = call_originator(CallOriginator),
	Acc1 = Acc#{<<"callOriginator">> => CO},
	mt_basic_call_information2(MtBasicCallInformation, Acc1);
mt_basic_call_information1(MtBasicCallInformation, Acc) ->
	mt_basic_call_information2(MtBasicCallInformation, Acc).
%% @hidden
mt_basic_call_information2(#{callEventStartTimeStamp := CallEventStartTimeStamp}
		= MtBasicCallInformation, Acc) ->
	TS = timestamp(CallEventStartTimeStamp),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	mt_basic_call_information3(MtBasicCallInformation, Acc1);
mt_basic_call_information2(MtBasicCallInformation, Acc) ->
	mt_basic_call_information3(MtBasicCallInformation, Acc).
%% @hidden
mt_basic_call_information3(#{totalCallEventDuration := TotalCallEventDuration}
		= MtBasicCallInformation, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	mt_basic_call_information4(MtBasicCallInformation, Acc1);
mt_basic_call_information3(MtBasicCallInformation, Acc) ->
	mt_basic_call_information4(MtBasicCallInformation, Acc).
%% @hidden
mt_basic_call_information4(#{causeForTerm := CauseForTerm}, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
mt_basic_call_information4(_MtBasicCallInformation, Acc) ->
	Acc.

%% @hidden
gprs_basic_call_information(#{gprsChargeableSubscriber
		:= GprsChargeableSubscriber} = GprsBasicCallInformation) ->
	GCS = gprs_chargeable_subscriber(GprsChargeableSubscriber),
	Acc = #{<<"gprsChargeableSubscriber">> => GCS},
	gprs_basic_call_information1(GprsBasicCallInformation, Acc);
gprs_basic_call_information(GprsBasicCallInformation) ->
	gprs_basic_call_information1(GprsBasicCallInformation, #{}).
%% @hidden
gprs_basic_call_information1(#{callEventStartTimeStamp
		:= CallEventStartTimeStamp} = GprsBasicCallInformation, Acc) ->
	TS = timestamp(CallEventStartTimeStamp),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	gprs_basic_call_information2(GprsBasicCallInformation, Acc1);
gprs_basic_call_information1(GprsBasicCallInformation, Acc) ->
	gprs_basic_call_information2(GprsBasicCallInformation, Acc).
%% @hidden
gprs_basic_call_information2(#{totalCallEventDuration
		:= TotalCallEventDuration} = GprsBasicCallInformation, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	gprs_basic_call_information3(GprsBasicCallInformation, Acc1);
gprs_basic_call_information2(GprsBasicCallInformation, Acc) ->
	gprs_basic_call_information3(GprsBasicCallInformation, Acc).
%% @hidden
gprs_basic_call_information3(#{causeForTerm := CauseForTerm}, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
gprs_basic_call_information3(_GprsBasicCallInformation, Acc) ->
	Acc.

%% @hidden
chargeable_subscriber({simChargeableSubscriber, SimChargeableSubscriber}) ->
	CS = sim_chargeable_subscriber(SimChargeableSubscriber),
	#{<<"simChargeableSubscriber">> => CS};
chargeable_subscriber({minChargeableSubscriber, MinChargeableSubscriber}) ->
	CS = min_chargeable_subscriber(MinChargeableSubscriber),
	#{<<"minChargeableSubscriber">> => CS}.

%% @hidden
sim_chargeable_subscriber(#{imsi := IMSI} = SimChargeableSubscriber) ->
	Acc = #{<<"imsi">> => bcd(IMSI)},
	sim_chargeable_subscriber1(SimChargeableSubscriber, Acc);
sim_chargeable_subscriber(SimChargeableSubscriber) ->
	sim_chargeable_subscriber1(SimChargeableSubscriber, #{}).
%% @hidden
sim_chargeable_subscriber1(#{msisdn := MSISDN}, Acc) ->
	Acc#{<<"msisdn">> => bcd(MSISDN)};
sim_chargeable_subscriber1(_SimChargeableSubscriber, Acc) ->
	Acc.

%% @hidden
min_chargeable_subscriber(#{min := MIN} = MinChargeableSubscriber) ->
	Acc = #{<<"min">> => bcd(MIN)},
	min_chargeable_subscriber1(MinChargeableSubscriber, Acc);
min_chargeable_subscriber(MinChargeableSubscriber) ->
	min_chargeable_subscriber1(MinChargeableSubscriber, #{}).
%% @hidden
min_chargeable_subscriber1(#{mdn := MDN}, Acc) ->
	Acc#{<<"mdn">> => bcd(MDN)};
min_chargeable_subscriber1(_MinChargeableSubscriber, Acc) ->
	Acc.

%% @hidden
gprs_chargeable_subscriber(#{chargeableSubscriber
		:= ChargeableSubscriber} = _GprsChargeableSubscriber) ->
	CS = chargeable_subscriber(ChargeableSubscriber),
	#{<<"chargeableSubscriber">> => CS};
gprs_chargeable_subscriber(_GprsChargeableSubscriber) ->
	#{}.

%% @hidden
destination(#{calledNumber := CalledNumber} = Destination) ->
	Acc = #{<<"calledNumber">> => bcd(CalledNumber)},
	destination1(Destination, Acc);
destination(Destination) ->
	destination1(Destination, #{}).
%% @hidden
destination1(#{dialledDigits := DialledDigits} = Destination, Acc) ->
	Acc1 = Acc#{<<"dialledDigits">> => DialledDigits},
	destination2(Destination, Acc1);
destination1(Destination, Acc) ->
	destination2(Destination, Acc).
%% @hidden
destination2(#{sMSDestinationNumber := SMSDestinationNumber}, Acc) ->
	Acc#{<<"sMSDestinationNumber">> => SMSDestinationNumber};
destination2(_Destination, Acc) ->
	Acc.

%% @hidden
call_originator(#{callingNumber := CallingNumber} = CallOriginator) ->
	Acc = #{<<"callingNumber">> => bcd(CallingNumber)},
	call_originator1(CallOriginator, Acc);
call_originator(CallOriginator) ->
	call_originator1(CallOriginator, #{}).
%% @hidden
call_originator1(#{sMSOriginator := SMSOriginator}, Acc) ->
	Acc#{<<"sMSOriginator">> => bcd(SMSOriginator)};
call_originator1(_CallOriginator, Acc) ->
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
octet_string(OctetString) when is_binary(OctetString) ->
	ByteSize = byte_size(OctetString),
	FieldWidth = 2 * ByteSize,
	BitSize = ByteSize * 8,
	<<N:BitSize>> = OctetString,
	io_lib:fwrite("~*.16.0b", [FieldWidth, N]).

%% @hidden
timestamp(#{localTimeStamp := <<Year:4/binary, Month:2/binary,
		Day:2/binary, Hour:2/binary, Minute:2/binary, Second:2/binary>>,
		utcTimeOffsetCode := Z}) ->
	<<Year/binary, $-, Month/binary, $-, Day/binary, $T, Hour/binary,
			$:, Minute/binary, $:, Second/binary>>.

%% @hidden
basic_service_used(#{basicService := BasicService} = BasicServiceUsed) ->
	BS = basic_service(BasicService),
	Acc = #{<<"BasicService">> => BS},
	basic_service_used1(BasicServiceUsed, Acc);
basic_service_used(BasicServiceUsed) ->
	basic_service_used1(BasicServiceUsed, #{}).
%% @hidden
basic_service_used1(#{chargingTimeStamp
		:= ChargingTimeStamp} = BasicServiceUsed, Acc) ->
	TS = timestamp(ChargingTimeStamp),
	Acc1 = Acc#{<<"chargingTimeStamp">> => TS},
	basic_service_used2(BasicServiceUsed, Acc1);
basic_service_used1(BasicServiceUsed, Acc) ->
	basic_service_used2(BasicServiceUsed, Acc).
%% @hidden
basic_service_used2(#{chargeInformationList
		:= ChargeInformationList} = BasicServiceUsed, Acc) ->
	CIL = [charge_information(CI) || CI <- ChargeInformationList],
	Acc1 = Acc#{<<"chargeInformationList">> => CIL},
	basic_service_used3(BasicServiceUsed, Acc1);
basic_service_used2(BasicServiceUsed, Acc) ->
	basic_service_used3(BasicServiceUsed, Acc).
%% @hidden
basic_service_used3(#{hSCSDIndicator
		:= HSCSDIndicator} = _BasicServiceUsed, Acc) ->
	Acc#{<<"hSCSDIndicator">> => HSCSDIndicator};
basic_service_used3(_BasicServiceUsed, Acc) ->
	Acc.

%% @hidden
basic_service(#{serviceCode := BasicServiceCode} = BasicService) ->
	BSC = basic_service_code(BasicServiceCode),
	Acc = #{<<"serviceCode">> => BSC},
	basic_service1(BasicService, Acc);
basic_service(BasicService) ->
	basic_service1(BasicService, #{}).
%% @hidden
basic_service1(#{transparencyIndicator
		:= TransparencyIndicator} = BasicService, Acc) ->
	Acc1 = Acc#{<<"transparencyIndicator">> => TransparencyIndicator},
	basic_service2(BasicService, Acc1);
basic_service1(BasicService, Acc) ->
	basic_service2(BasicService, Acc).
%% @hidden
basic_service2(#{fnur := Fnur} = BasicService, Acc) ->
	Acc1 = Acc#{<<"fnur">> => Fnur},
	basic_service3(BasicService, Acc1);
basic_service2(BasicService, Acc) ->
	basic_service3(BasicService, Acc).
%% @hidden
basic_service3(#{userProtocolIndicator
		:= UserProtocolIndicator} = BasicService, Acc) ->
	Acc1 = Acc#{<<"userProtocolIndicator">> => UserProtocolIndicator},
	basic_service4(BasicService, Acc1);
basic_service3(BasicService, Acc) ->
	basic_service4(BasicService, Acc).
%% @hidden
basic_service4(#{guaranteedBitRate := GuaranteedBitRate} = BasicService, Acc) ->
	Acc1 = Acc#{<<"guaranteedBitRate">> => GuaranteedBitRate},
	basic_service5(BasicService, Acc1);
basic_service4(BasicService, Acc) ->
	basic_service5(BasicService, Acc).
%% @hidden
basic_service5(#{maximumBitRate := MaximumBitRate} = _BasicService, Acc) ->
	Acc#{<<"maximumBitRate">> => MaximumBitRate};
basic_service5(_BasicService, Acc) ->
	Acc.

%% @hidden
basic_service_code({teleServiceCode, TeleServiceCode}) ->
	#{<<"teleServiceCode">> => TeleServiceCode};
basic_service_code({bearerServiceCode, BearerServiceCode}) ->
	#{<<"bearerServiceCode">> => BearerServiceCode}.

%% @hidden
location_information(#{geographicalLocation
		:= GeographicalLocation} = LocationInformation) ->
	Acc = #{<<"geographicalLocation">> => GeographicalLocation},
	location_information1(LocationInformation, Acc);
location_information(LocationInformation) ->
	location_information1(LocationInformation, #{}).
%% @hidden
location_information1(#{homeLocationInformation
		:= HomeLocationInformation} = LocationInformation, Acc) ->
	Acc1 = Acc#{<<"homeLocationInformation">> => HomeLocationInformation},
	location_information2(LocationInformation, Acc1);
location_information1(LocationInformation, Acc) ->
	location_information2(LocationInformation, Acc).
%% @hidden
location_information2(#{networkLocation
		:= NetworkLocation} = _LocationInformation, Acc) ->
	NL = network_location(NetworkLocation),
	Acc#{<<"networkLocation">> => NL};
location_information2(_LocationInformation, Acc) ->
	Acc.

%% @hidden
network_location(#{callReference
		:= CallReference} = NetworkLocation)
		when is_binary(CallReference) ->
	Acc = #{<<"callReference">> => octet_string(CallReference)},
	network_location1(NetworkLocation, Acc);
network_location(NetworkLocation) ->
	network_location1(NetworkLocation, #{}).
%% @hidden
network_location1(#{cellId := CellId} = NetworkLocation, Acc) ->
	Acc1 = Acc#{<<"cellId">> => CellId},
	network_location2(NetworkLocation, Acc1);
network_location1(NetworkLocation, Acc) ->
	network_location2(NetworkLocation, Acc).
%% @hidden
network_location2(#{locationArea
		:= LocationArea} = _NetworkLocation, Acc) ->
	Acc#{<<"locationArea">> => LocationArea};
network_location2(_NetworkLocation, Acc) ->
	Acc.

%% @hidden
charge_information(#{chargedItem
		:= ChargedItem} = ChargeInformation) ->
	Acc = #{<<"chargedItem">> => ChargedItem},
	charge_information1(ChargeInformation, Acc);
charge_information(ChargeInformation) ->
	charge_information1(ChargeInformation, #{}).
%% @hidden
charge_information1(#{exchangeRateCode
		:= ExchangeRateCode} = ChargeInformation, Acc) ->
	Acc1 = Acc#{<<"exchangeRateCode">> => ExchangeRateCode},
	charge_information2(ChargeInformation, Acc1);
charge_information1(ChargeInformation, Acc) ->
	charge_information2(ChargeInformation, Acc).
%% @hidden
charge_information2(#{callTypeGroup
		:= CallTypeGroup} = ChargeInformation, Acc) ->
	Acc1 = Acc#{<<"callTypeGroup">> => CallTypeGroup},
	charge_information3(ChargeInformation, Acc1);
charge_information2(ChargeInformation, Acc) ->
	charge_information3(ChargeInformation, Acc).
%% @hidden
charge_information3(#{chargeDetailList
		:= ChargeDetailList} = ChargeInformation, Acc) ->
	CDL = [charge_detail(CD) || CD <- ChargeDetailList],
	Acc1 = Acc#{<<"chargeDetailList">> => CDL},
	charge_information4(ChargeInformation, Acc1);
charge_information3(ChargeInformation, Acc) ->
	charge_information4(ChargeInformation, Acc).
%% @hidden
charge_information4(#{taxInformation
		:= TaxInformationList} = ChargeInformation, Acc) ->
	Acc1 = Acc#{<<"taxInformation">> => TaxInformationList},
	charge_information5(ChargeInformation, Acc1);
charge_information4(ChargeInformation, Acc) ->
	charge_information5(ChargeInformation, Acc).
%% @hidden
charge_information5(#{discountInformation
		:= DiscountInformation} = _ChargeInformation, Acc) ->
	Acc#{<<"discountInformation">> => DiscountInformation};
charge_information5(_ChargeInformation, Acc) ->
	Acc.

%% @hidden
charge_detail(#{chargeType := ChargeType} = ChargeDetail) ->
	Acc = #{<<"chargeType">> => ChargeType},
	charge_detail1(ChargeDetail, Acc);
charge_detail(ChargeDetail) ->
	charge_detail1(ChargeDetail, #{}).
%% @hidden
charge_detail1(#{charge := Charge} = ChargeDetail, Acc) ->
	Acc1 = Acc#{<<"charge">> => Charge},
	charge_detail2(ChargeDetail, Acc1);
charge_detail1(ChargeDetail, Acc) ->
	charge_detail2(ChargeDetail, Acc).
%% @hidden
charge_detail2(#{chargeableUnits
		:= ChargeableUnits} = ChargeDetail, Acc) ->
	Acc1 = Acc#{<<"chargeableUnits">> => ChargeableUnits},
	charge_detail3(ChargeDetail, Acc1);
charge_detail2(ChargeDetail, Acc) ->
	charge_detail3(ChargeDetail, Acc).
%% @hidden
charge_detail3(#{chargedUnits
		:= ChargedUnits} = ChargeDetail, Acc) ->
	Acc1 = Acc#{<<"chargedUnits">> => ChargedUnits},
	charge_detail4(ChargeDetail, Acc1);
charge_detail3(ChargeDetail, Acc) ->
	charge_detail4(ChargeDetail, Acc).
%% @hidden
charge_detail4(#{chargeDetailTimeStamp
		:= ChargeDetailTimeStamp} = _ChargeDetail, Acc) ->
	TS = timestamp(ChargeDetailTimeStamp),
	Acc#{<<"chargeDetailTimeStamp">> => TS};
charge_detail4(_ChargeDetail, Acc) ->
	Acc.

%% @hidden
gprs_service_used(#{iMSSignallingContext
		:= IMSSignallingContext} = GprsServiceUsed) ->
	Acc = #{<<"iMSSignallingContext">> => IMSSignallingContext},
	gprs_service_used1(GprsServiceUsed, Acc);
gprs_service_used(GprsServiceUsed) ->
	gprs_service_used1(GprsServiceUsed, #{}).
%% @hidden
gprs_service_used1(#{dataVolumeIncoming
		:= DataVolumeIncoming} = GprsServiceUsed, Acc) ->
	Acc1 = Acc#{<<"dataVolumeIncoming">> => DataVolumeIncoming},
	gprs_service_used2(GprsServiceUsed, Acc1);
gprs_service_used1(GprsServiceUsed, Acc) ->
	gprs_service_used2(GprsServiceUsed, Acc).
%% @hidden
gprs_service_used2(#{dataVolumeOutgoing
		:= DataVolumeOutgoing} = GprsServiceUsed, Acc) ->
	Acc1 = Acc#{<<"dataVolumeOutgoing">> => DataVolumeOutgoing},
	gprs_service_used3(GprsServiceUsed, Acc1);
gprs_service_used2(GprsServiceUsed, Acc) ->
	gprs_service_used3(GprsServiceUsed, Acc).
%% @hidden
gprs_service_used3(#{chargeInformationList
		:= ChargeInformationList} = _GprsServiceUsed, Acc) ->
	CIL = [charge_information(CI) || CI <- ChargeInformationList],
	Acc#{<<"chargeInformationList">> => CIL};
gprs_service_used3(_GprsServiceUsed, Acc) ->
	Acc.

