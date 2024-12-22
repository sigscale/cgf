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

%% export the public API
-export([import/2, import/3]).

%% export the private API
-export([parse/4]).

-include_lib("kernel/include/logger.hrl").

-type state() :: #{
		zones := #{Code :: integer() => Offset :: binary()},
		entities := #{Code :: integer() => map()}}.

%%----------------------------------------------------------------------
%%  The cgf_tap public API
%%----------------------------------------------------------------------

-spec import(File, Log) -> Result
	when
		File :: file:filename() | binary(),
		Log :: disk_log:log(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @equiv import(File, Log, [])
import(File, Log) ->
	import(File, Log, []).

-spec import(File, Log, Metadata) -> Result
	when
		File :: file:filename() | binary(),
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import TAP3 file and write to Bx interface log.
import(File, Log, Metadata) when is_list(File) ->
	case file:read_file(File) of
		{ok, Bin} ->
			State = #{zones => #{}, entities => #{}},
			import1(Log, Metadata, State, Bin);
		{error, Reason} ->
			{error, Reason}
	end;
import(Bin, Log, Metadata)
		when is_binary(Bin), is_list(Metadata) ->
	State = #{zones => #{}, entities => #{}},
	import1(Log, Metadata, State, Bin).
%% @hidden
import1(Log, Metadata, State, Bin) ->
	case 'TAP-0312':decode('DataInterChange', Bin) of
		{ok, {transferBatch, TransferBatch}, <<>>} ->
			import2(Log, Metadata, State, TransferBatch);
		{ok, {transferBatch, TransferBatch}, Rest} ->
			?LOG_WARNING([{?MODULE, import},
					{reason, ignored},
					{size, byte_size(Rest)}]),
			import2(Log, Metadata, State, TransferBatch);
		{ok, {notification, _Notification}} ->
			{error, not_implemented};
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
import2(Log, Metadata, State,
		#{accountingInfo := AccountingInfo} = TransferBatch) ->
	Metadata1 = case parse_accounting(AccountingInfo) of
		AccountingInfo1 when map_size(AccountingInfo1) > 0 ->
			[{roam_accountingInfo, AccountingInfo1} | Metadata];
		_ ->
			Metadata
	end,
	import3(Log, Metadata1, State, TransferBatch);
import2(Log, Metadata, State, TransferBatch) ->
	import3(Log, Metadata, State, TransferBatch).
%% @hidden
import3(Log, Metadata, State,
		#{batchControlInfo := BatchControlInfo} = TransferBatch)
		when map_size(BatchControlInfo) > 0 ->
	Metadata1 = case parse_batchcontrol(BatchControlInfo, State) of
		BatchControlInfo1 when map_size(BatchControlInfo1) > 0 ->
			[{roam_batchControlInfo, BatchControlInfo1} | Metadata];
		_ ->
			Metadata
	end,
	import4(Log, Metadata1, State, TransferBatch);
import3(Log, Metadata, State, TransferBatch) ->
	import4(Log, Metadata, State, TransferBatch).
%% @hidden
import4(Log, Metadata, State,
		#{networkInfo := NetworkInfo} = TransferBatch)
		when map_size(NetworkInfo) > 0 ->
	State1 = parse_networkinfo(NetworkInfo, State),
	import5(Log, Metadata, State1, TransferBatch);
import4(Log, Metadata, State, TransferBatch) ->
	import5(Log, Metadata, State, TransferBatch).

%% @hidden
import5(Log, Metadata, State,
		#{callEventDetails := CDRs} = _TransferBatch)
		when CDRs /= [] ->
	parse(Log, Metadata, State, CDRs);
import5(_Log, _Metadata, _State, _TransferBatch) ->
	{reason, empty}.

%%----------------------------------------------------------------------
%%  The cgf_tap public API
%%----------------------------------------------------------------------

-dialyzer({no_match, parse/4}).
-spec parse(Log, Metadata, State, CDRs) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		State :: state(),
		AttributeName :: string(),
		AttributeValue :: term(),
		CDRs :: [tuple()],
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse CDRs from the import file.
%% @private
parse(Log, Metadata, State,
		[{mobileOriginatedCall, MobileOriginatedCall} | T]) ->
	case parse_mo_call(Log, Metadata, State, MobileOriginatedCall) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mo_call},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{mobileTerminatedCall, MobileTerminatedCall} | T]) ->
	case parse_mt_call(Log, Metadata, State, MobileTerminatedCall) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_call},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{supplServiceEvent, SupplServiceEvent} | T]) ->
	case parse_mmtel(Log, Metadata, SupplServiceEvent) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mmtel},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{serviceCentreUsage, ServiceCentreUsage} | T]) ->
	case parse_sc_sm(Log, Metadata, ServiceCentreUsage) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_sc_sm},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{gprsCall, GprsCall} | T]) ->
	case parse_gprs(Log, Metadata, State, GprsCall) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_gprs},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{contentTransaction, ContentTransaction} | T]) ->
	case parse_content(Log, Metadata, ContentTransaction) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_content},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{locationService, LocationService} | T]) ->
	case parse_location(Log, Metadata, LocationService) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_location},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{messagingEvent, MessagingEvent} | T]) ->
	case parse_message(Log, Metadata, MessagingEvent) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_message},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(Log, Metadata, State,
		[{mobileSession, MobileSession} | T]) ->
	case parse_session(Log, Metadata, MobileSession) of
		ok ->
			parse(Log, Metadata, State, T);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_session},
					{error, Reason}]),
			parse(Log, Metadata, State, T)
	end;
parse(_Log, _Metadata, _State, []) ->
	ok.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse_mo_call(Log, Metadata, State, MOC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		State :: state(),
		AttributeName :: string(),
		AttributeValue :: term(),
		MOC :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile originated call.
parse_mo_call(Log, Metadata, State, MOC) ->
	Call = mobile_originated_call(MOC, State),
	CDR = [{roam_moCall, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_call(Log, Metadata, State, MTC) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		State :: state(),
		AttributeName :: string(),
		AttributeValue :: term(),
		MTC :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a mobile terminated call.
parse_mt_call(Log, Metadata, State, MTC) ->
	Call = mobile_terminated_call(MTC, State),
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

-spec parse_gprs(Log, Metadata, State, GPRS) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		State :: state(),
		AttributeName :: string(),
		AttributeValue :: term(),
		GPRS :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a TAP event detail for a GPRS session.
parse_gprs(Log, Metadata, State, GPRS) ->
	Call = gprs_call(GPRS, State),
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
			taxation => [map()]}.
%% @doc Parse Accounting Info from the import file.
%% @private
parse_accounting(AccountingInfo) ->
	parse_accounting(AccountingInfo, #{}).
%% @hidden
parse_accounting(#{currencyConversionInfo
		:= CurrencyConversionInfo} = AI, Acc) ->
	CCI = parse_cci(CurrencyConversionInfo),
	parse_accounting1(AI,
			Acc#{currencyConversionInfo => CCI});
parse_accounting(AI, Acc) ->
	parse_accounting1(AI, Acc).
%% @hidden
parse_accounting1(#{auditControlInfo := AuditControlInfo} = AI, Acc) ->
	Discounting = parse_discounting(AuditControlInfo),
	parse_accounting2(AI, Acc#{discounting => Discounting});
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
	Acc#{taxation => parse_tax(Taxation)};
parse_accounting5(_AI, Acc) ->
	Acc.

-spec parse_networkinfo(NetworkInfo, State) -> State
	when
		NetworkInfo :: map(),
		State :: state().
%% @doc Parse Network Info from the import file.
%% @private
parse_networkinfo(#{utcTimeOffsetInfo
		:= UtcTimeOffsetInfoList} = NetworkInfo, State) ->
	Zones = utc_offset_info(UtcTimeOffsetInfoList),
	parse_networkinfo1(NetworkInfo, State#{zones => Zones});
parse_networkinfo(NetworkInfo, State) ->
	parse_networkinfo1(NetworkInfo, State).
%% @hidden
parse_networkinfo1(#{recEntityInfo := RecEntityInfo}, State) ->
	State#{entities => rec_entity_info(RecEntityInfo)};
parse_networkinfo1(_NetworkInfo, State) ->
	State.

-spec parse_batchcontrol(BatchControlInfo, State) -> Result
	when
		BatchControlInfo :: map(),
		State :: state(),
		Result :: map().
%% @doc Parse Batch Control Info from the import file.
%% @private
parse_batchcontrol(BatchControlInfo, State) ->
	parse_batchcontrol(BatchControlInfo, State, #{}).
%% @hidden
parse_batchcontrol(#{sender := Sender}
		= BatchControlInfo, State, Acc) when byte_size(Sender) > 0 ->
	Acc1 = Acc#{<<"sender">> => Sender},
	parse_batchcontrol1(BatchControlInfo, State, Acc1);
parse_batchcontrol(BatchControlInfo, State, Acc) ->
	parse_batchcontrol1(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol1(#{recipient := Recipient}
		= BatchControlInfo, State, Acc) when byte_size(Recipient) > 0 ->
	Acc1 = Acc#{<<"recipient">> => Recipient},
	parse_batchcontrol2(BatchControlInfo, State, Acc1);
parse_batchcontrol1(BatchControlInfo, State, Acc) ->
	parse_batchcontrol2(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol2(#{fileSequenceNumber := FSN}
		= BatchControlInfo, State, Acc) when byte_size(FSN) > 0 ->
	Acc1 = Acc#{<<"fileSequenceNumber">> => binary_to_integer(FSN)},
	parse_batchcontrol3(BatchControlInfo, State, Acc1);
parse_batchcontrol2(BatchControlInfo, State, Acc) ->
	parse_batchcontrol3(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol3(#{fileCreationTimeStamp := TS}
		= BatchControlInfo, State, Acc) when map_size(TS) > 0 ->
	Acc1 = Acc#{<<"fileCreationTimeStamp">> => timestamp(TS, State)},
	parse_batchcontrol4(BatchControlInfo, State, Acc1);
parse_batchcontrol3(BatchControlInfo, State, Acc) ->
	parse_batchcontrol4(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol4(#{transferCutOffTimeStamp := TS}
		= BatchControlInfo, State, Acc) when map_size(TS) > 0 ->
	Acc1 = Acc#{<<"transferCutOffTimeStamp">> => timestamp(TS, State)},
	parse_batchcontrol5(BatchControlInfo, State, Acc1);
parse_batchcontrol4(BatchControlInfo, State, Acc) ->
	parse_batchcontrol5(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol5(#{fileAvailableTimeStamp := TS}
		= BatchControlInfo, State, Acc) when map_size(TS) > 0 ->
	Acc1 = Acc#{<<"fileAvailableTimeStamp">> => timestamp(TS, State)},
	parse_batchcontrol6(BatchControlInfo, State, Acc1);
parse_batchcontrol5(BatchControlInfo, State, Acc) ->
	parse_batchcontrol6(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol6(#{specificationVersionNumber := VSN}
		= BatchControlInfo, State, Acc) when is_integer(VSN) > 0 ->
	Acc1 = Acc#{<<"specificationVersionNumber">> => VSN},
	parse_batchcontrol7(BatchControlInfo, State, Acc1);
parse_batchcontrol6(BatchControlInfo, State, Acc) ->
	parse_batchcontrol7(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol7(#{releaseVersionNumber := VSN}
		= BatchControlInfo, State, Acc) when is_integer(VSN) > 0 ->
	Acc1 = Acc#{<<"releaseVersionNumber">> => VSN},
	parse_batchcontrol8(BatchControlInfo, State, Acc1);
parse_batchcontrol7(BatchControlInfo, State, Acc) ->
	parse_batchcontrol8(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol8(#{fileTypeIndicator := FTI}
		= BatchControlInfo, State, Acc) when byte_size(FTI) > 0 ->
	Acc1 = Acc#{<<"fileTypeIndicator">> => FTI},
	parse_batchcontrol9(BatchControlInfo, State, Acc1);
parse_batchcontrol8(BatchControlInfo, State, Acc) ->
	parse_batchcontrol9(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol9(#{rapFileSequenceNumber := FSN}
		= BatchControlInfo, State, Acc) when byte_size(FSN) > 0 ->
	Acc1 = Acc#{<<"rapFileSequenceNumber">> => binary_to_integer(FSN)},
	parse_batchcontrol10(BatchControlInfo, State, Acc1);
parse_batchcontrol9(BatchControlInfo, State, Acc) ->
	parse_batchcontrol10(BatchControlInfo, State, Acc).
%% @hidden
parse_batchcontrol10(#{operatorSpecInformation := OSI}
		= _BatchControlInfo, _State, Acc) when length(OSI) > 0 ->
	Acc#{<<"operatorSpecInformation">> => OSI};
parse_batchcontrol10(_BatchControlInfo, _State, Acc) ->
	Acc.

-spec parse_cci(CCI) -> Result
	when
		CCI :: [map()],
		Result :: [map()].
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
		Result :: [map()].
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
		:= MoBasicCallInformation} = MobileOriginatedCall, State) ->
	BCI = mo_basic_call_information(MoBasicCallInformation, State),
	Acc = #{<<"basicCallInformation">> => BCI},
	mobile_originated_call1(MobileOriginatedCall, State, Acc);
mobile_originated_call(MobileOriginatedCall, State) ->
	mobile_originated_call1(MobileOriginatedCall, State, #{}).
%% @hidden
mobile_originated_call1(#{locationInformation
		:= LocationInformation} = MobileOriginatedCall, State, Acc) ->
	LI = location_information(LocationInformation),
	Acc1 = Acc#{<<"locationInformation">> => LI},
	mobile_originated_call2(MobileOriginatedCall, State, Acc1);
mobile_originated_call1(MobileOriginatedCall, State, Acc) ->
	mobile_originated_call2(MobileOriginatedCall, State, Acc).
%% @hidden
mobile_originated_call2(#{basicServiceUsedList
		:= BasicServiceUsedList} = _MobileOriginatedCall, State, Acc) ->
	BSUL = [basic_service_used(BSU, State) || BSU <- BasicServiceUsedList],
	Acc#{<<"basicServiceUsedList">> => BSUL};
mobile_originated_call2(_MobileOriginatedCall, _State, Acc) ->
	Acc.

%% @hidden
mobile_terminated_call(#{basicCallInformation
		:= MtBasicCallInformation} = MobileTerminatedCall, State) ->
	BCI = mt_basic_call_information(MtBasicCallInformation, State),
	Acc = #{<<"basicCallInformation">> => BCI},
	mobile_terminated_call1(MobileTerminatedCall, State, Acc);
mobile_terminated_call(MobileTerminatedCall, State) ->
	mobile_terminated_call1(MobileTerminatedCall, State, #{}).
%% @hidden
mobile_terminated_call1(#{locationInformation
		:= LocationInformation} = MobileTerminatedCall, State, Acc) ->
	LI = location_information(LocationInformation),
	Acc1 = Acc#{<<"locationInformation">> => LI},
	mobile_terminated_call2(MobileTerminatedCall, State, Acc1);
mobile_terminated_call1(MobileTerminatedCall, State, Acc) ->
	mobile_terminated_call2(MobileTerminatedCall,State,  Acc).
%% @hidden
mobile_terminated_call2(#{basicServiceUsedList
		:= BasicServiceUsedList} = _MobileTerminatedCall, State, Acc) ->
	BSUL = [basic_service_used(BSU, State) || BSU <- BasicServiceUsedList],
	Acc#{<<"basicServiceUsedList">> => BSUL};
mobile_terminated_call2(_MobileTerminatedCall, _State, Acc) ->
	Acc.

%% @hidden
gprs_call(#{gprsBasicCallInformation
		:= GprsBasicCallInformation} = GprsCall, State) ->
	BCI = gprs_basic_call_information(GprsBasicCallInformation, State),
	Acc = #{<<"gprsBasicCallInformation">> => BCI},
	gprs_call1(GprsCall, State, Acc);
gprs_call(GprsCall, State) ->
	gprs_call1(GprsCall, State, #{}).
%% @hidden
gprs_call1(#{gprsLocationInformation
		:= GprsLocationInformation} = GprsCall, State, Acc) ->
	GLI = gprs_location_information(GprsLocationInformation, State),
	Acc1 = Acc#{<<"gprsLocationInformation">> => GLI},
	gprs_call2(GprsCall, State, Acc1);
gprs_call1(GprsCall, State, Acc) ->
	gprs_call2(GprsCall, State, Acc).
%% @hidden
gprs_call2(#{gprsServiceUsed := GprsServiceUsed} = _GprsCall, State, Acc) ->
	GSU = gprs_service_used(GprsServiceUsed, State),
	Acc#{<<"gprsServiceUsed">> => GSU};
gprs_call2(_GprsCall, _State, Acc) ->
	Acc.

%% @hidden
mo_basic_call_information(#{chargeableSubscriber := ChargeableSubscriber}
		= MoBasicCallInformation, State) ->
	CS = chargeable_subscriber(ChargeableSubscriber),
	Acc = #{<<"chargeableSubscriber">> => CS},
	mo_basic_call_information1(MoBasicCallInformation, State, Acc);
mo_basic_call_information(MoBasicCallInformation, State) ->
	mo_basic_call_information1(MoBasicCallInformation, State, #{}).
%% @hidden
mo_basic_call_information1(#{destination := Destination}
		= MoBasicCallInformation, State, Acc) ->
	Dest = destination(Destination),
	Acc1 = Acc#{<<"destination">> => Dest},
	mo_basic_call_information2(MoBasicCallInformation, State, Acc1);
mo_basic_call_information1(MoBasicCallInformation, State, Acc) ->
	mo_basic_call_information2(MoBasicCallInformation, State, Acc).
%% @hidden
mo_basic_call_information2(#{callEventStartTimeStamp := CallEventStartTimeStamp}
		= MoBasicCallInformation, State, Acc) ->
	TS = timestamp(CallEventStartTimeStamp, State),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	mo_basic_call_information3(MoBasicCallInformation, State, Acc1);
mo_basic_call_information2(MoBasicCallInformation, State, Acc) ->
	mo_basic_call_information3(MoBasicCallInformation, State, Acc).
%% @hidden
mo_basic_call_information3(#{totalCallEventDuration := TotalCallEventDuration}
		= MoBasicCallInformation, State, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	mo_basic_call_information4(MoBasicCallInformation, State, Acc1);
mo_basic_call_information3(MoBasicCallInformation, State, Acc) ->
	mo_basic_call_information4(MoBasicCallInformation, State, Acc).
%% @hidden
mo_basic_call_information4(#{causeForTerm := CauseForTerm}, _State, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
mo_basic_call_information4(_MoBasicCallInformation, _State, Acc) ->
	Acc.

%% @hidden
mt_basic_call_information(#{chargeableSubscriber := ChargeableSubscriber}
		= MtBasicCallInformation, State) ->
	CS = chargeable_subscriber(ChargeableSubscriber),
	Acc = #{<<"chargeableSubscriber">> => CS},
	mt_basic_call_information1(MtBasicCallInformation, State, Acc);
mt_basic_call_information(MtBasicCallInformation, State) ->
	mt_basic_call_information1(MtBasicCallInformation, State, #{}).
%% @hidden
mt_basic_call_information1(#{callOriginator := CallOriginator}
		= MtBasicCallInformation, State, Acc) ->
	CO = call_originator(CallOriginator),
	Acc1 = Acc#{<<"callOriginator">> => CO},
	mt_basic_call_information2(MtBasicCallInformation, State, Acc1);
mt_basic_call_information1(MtBasicCallInformation, State, Acc) ->
	mt_basic_call_information2(MtBasicCallInformation, State, Acc).
%% @hidden
mt_basic_call_information2(#{callEventStartTimeStamp := CallEventStartTimeStamp}
		= MtBasicCallInformation, State, Acc) ->
	TS = timestamp(CallEventStartTimeStamp, State),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	mt_basic_call_information3(MtBasicCallInformation, State, Acc1);
mt_basic_call_information2(MtBasicCallInformation, State, Acc) ->
	mt_basic_call_information3(MtBasicCallInformation, State, Acc).
%% @hidden
mt_basic_call_information3(#{totalCallEventDuration := TotalCallEventDuration}
		= MtBasicCallInformation, State, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	mt_basic_call_information4(MtBasicCallInformation, State, Acc1);
mt_basic_call_information3(MtBasicCallInformation, State, Acc) ->
	mt_basic_call_information4(MtBasicCallInformation, State, Acc).
%% @hidden
mt_basic_call_information4(#{causeForTerm := CauseForTerm}, _State, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
mt_basic_call_information4(_MtBasicCallInformation, _State, Acc) ->
	Acc.

%% @hidden
gprs_basic_call_information(#{gprsChargeableSubscriber
		:= GprsChargeableSubscriber} = GprsBasicCallInformation, State) ->
	GCS = gprs_chargeable_subscriber(GprsChargeableSubscriber),
	Acc = #{<<"gprsChargeableSubscriber">> => GCS},
	gprs_basic_call_information1(GprsBasicCallInformation, State, Acc);
gprs_basic_call_information(GprsBasicCallInformation, State) ->
	gprs_basic_call_information1(GprsBasicCallInformation, State, #{}).
%% @hidden
gprs_basic_call_information1(#{callEventStartTimeStamp
		:= CallEventStartTimeStamp} = GprsBasicCallInformation, State, Acc) ->
	TS = timestamp(CallEventStartTimeStamp, State),
	Acc1 = Acc#{<<"callEventStartTimeStamp">> => TS},
	gprs_basic_call_information2(GprsBasicCallInformation, State, Acc1);
gprs_basic_call_information1(GprsBasicCallInformation, State, Acc) ->
	gprs_basic_call_information2(GprsBasicCallInformation, State, Acc).
%% @hidden
gprs_basic_call_information2(#{totalCallEventDuration
		:= TotalCallEventDuration} = GprsBasicCallInformation, State, Acc) ->
	Acc1 = Acc#{<<"totalCallEventDuration">> => TotalCallEventDuration},
	gprs_basic_call_information3(GprsBasicCallInformation, State, Acc1);
gprs_basic_call_information2(GprsBasicCallInformation, State, Acc) ->
	gprs_basic_call_information3(GprsBasicCallInformation, State, Acc).
%% @hidden
gprs_basic_call_information3(#{causeForTerm := CauseForTerm}, _State, Acc) ->
	Acc#{<<"causeForTerm">> => CauseForTerm};
gprs_basic_call_information3(_GprsBasicCallInformation, _State, Acc) ->
	Acc.

%% @hidden
gprs_location_information(#{gprsNetworkLocation
		:= GprsNetworkLocation} = GprsLocationInformation, State) ->
	GNL = gprs_network_location(GprsNetworkLocation, State),
	Acc = #{<<"gprsNetworkLocation">> => GNL},
	gprs_location_information1(GprsLocationInformation, Acc);
gprs_location_information(GprsLocationInformation, _State) ->
	gprs_location_information1(GprsLocationInformation, #{}).
%% @hidden
gprs_location_information1(#{homeLocationInformation
		:= HomeLocationInformation} = GprsLocationInformation, Acc) ->
	Acc1 = Acc#{<<"homeLocationInformation">> => HomeLocationInformation},
	gprs_location_information2(GprsLocationInformation, Acc1);
gprs_location_information1(GprsLocationInformation, Acc) ->
	gprs_location_information2(GprsLocationInformation, Acc).
%% @hidden
gprs_location_information2(#{geographicalLocation
		:= GeographicalLocation} = _GprsLocationInformation, Acc) ->
	Acc#{<<"geographicalLocation">> => GeographicalLocation};
gprs_location_information2(_GprsLocationInformation, Acc) ->
	Acc.

%% @hidden
gprs_network_location(#{recEntity
		:= RecEntityCodeList} = GprsNetworkLocation,
		#{entities := Entities} = _State) ->
	RE = [maps:get(C, Entities) || C <- RecEntityCodeList],
	Acc = #{<<"recEntity">> => RE},
	gprs_network_location1(GprsNetworkLocation, Acc);
gprs_network_location(GprsNetworkLocation, _State) ->
	gprs_network_location1(GprsNetworkLocation, #{}).
%% @hidden
gprs_network_location1(#{locationArea
		:= LocationArea} = GprsNetworkLocation, Acc) ->
	Acc1 = Acc#{<<"locationArea">> => LocationArea},
	gprs_network_location2(GprsNetworkLocation, Acc1);
gprs_network_location1(GprsNetworkLocation, Acc) ->
	gprs_network_location2(GprsNetworkLocation, Acc).
%% @hidden
gprs_network_location2(#{cellId := CellId} = _GprsNetworkLocation, Acc) ->
	Acc#{<<"cellId">> => CellId};
gprs_network_location2(_GprsNetworkLocation, Acc) ->
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
		utcTimeOffsetCode := Code}, #{zones := Zones} = _State) ->
	<<Sign, H1, H2, M1, M2>> = maps:get(Code, Zones),
	<<Year/binary, $-, Month/binary, $-, Day/binary, $T, Hour/binary,
			$:, Minute/binary, $:, Second/binary, Sign, H1, H2, $:, M1, M2>>;
timestamp(#{localTimeStamp := <<Year:4/binary, Month:2/binary,
		Day:2/binary, Hour:2/binary, Minute:2/binary, Second:2/binary>>,
		utcTimeOffset := <<Sign, H1, H2, M1, M2>>}, _State) ->
	<<Year/binary, $-, Month/binary, $-, Day/binary, $T, Hour/binary,
			$:, Minute/binary, $:, Second/binary, Sign, H1, H2, $:, M1, M2>>.

%% @hidden
utc_offset_info(UtcTimeOffsetInfo) ->
	utc_offset_info(UtcTimeOffsetInfo, #{}).
%% @hidden
utc_offset_info([#{utcTimeOffsetCode := Code,
		utcTimeOffset := Offset} | T], Acc) ->
	Acc1 = Acc#{Code => Offset},
	utc_offset_info(T, Acc1);
utc_offset_info([], Acc) ->
	Acc.

%% @hidden
rec_entity_info(RecEntityInfo) ->
	rec_entity_info(RecEntityInfo, #{}).
%% @hidden
rec_entity_info([#{recEntityCode := Code} = Info | T], Acc) -> 
	Acc1 = Acc#{Code => Info},
	rec_entity_info(T, Acc1);
rec_entity_info([], Acc) ->
	Acc.

%% @hidden
basic_service_used(#{basicService := BasicService} = BasicServiceUsed, State) ->
	BS = basic_service(BasicService),
	Acc = #{<<"BasicService">> => BS},
	basic_service_used1(BasicServiceUsed, State, Acc);
basic_service_used(BasicServiceUsed, State) ->
	basic_service_used1(BasicServiceUsed, State, #{}).
%% @hidden
basic_service_used1(#{chargingTimeStamp
		:= ChargingTimeStamp} = BasicServiceUsed, State, Acc) ->
	TS = timestamp(ChargingTimeStamp, State),
	Acc1 = Acc#{<<"chargingTimeStamp">> => TS},
	basic_service_used2(BasicServiceUsed, State, Acc1);
basic_service_used1(BasicServiceUsed, State, Acc) ->
	basic_service_used2(BasicServiceUsed, State, Acc).
%% @hidden
basic_service_used2(#{chargeInformationList
		:= ChargeInformationList} = BasicServiceUsed, State, Acc) ->
	CIL = [charge_information(CI, State) || CI <- ChargeInformationList],
	Acc1 = Acc#{<<"chargeInformationList">> => CIL},
	basic_service_used3(BasicServiceUsed, State, Acc1);
basic_service_used2(BasicServiceUsed, State, Acc) ->
	basic_service_used3(BasicServiceUsed, State, Acc).
%% @hidden
basic_service_used3(#{hSCSDIndicator
		:= HSCSDIndicator} = _BasicServiceUsed, _State, Acc) ->
	Acc#{<<"hSCSDIndicator">> => HSCSDIndicator};
basic_service_used3(_BasicServiceUsed, _State, Acc) ->
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
		:= ChargedItem} = ChargeInformation, State) ->
	Acc = #{<<"chargedItem">> => ChargedItem},
	charge_information1(ChargeInformation, State, Acc);
charge_information(ChargeInformation, State) ->
	charge_information1(ChargeInformation, State, #{}).
%% @hidden
charge_information1(#{exchangeRateCode
		:= ExchangeRateCode} = ChargeInformation, State, Acc) ->
	Acc1 = Acc#{<<"exchangeRateCode">> => ExchangeRateCode},
	charge_information2(ChargeInformation, State, Acc1);
charge_information1(ChargeInformation, State, Acc) ->
	charge_information2(ChargeInformation, State, Acc).
%% @hidden
charge_information2(#{callTypeGroup
		:= CallTypeGroup} = ChargeInformation, State, Acc) ->
	Acc1 = Acc#{<<"callTypeGroup">> => CallTypeGroup},
	charge_information3(ChargeInformation, State, Acc1);
charge_information2(ChargeInformation, State, Acc) ->
	charge_information3(ChargeInformation, State, Acc).
%% @hidden
charge_information3(#{chargeDetailList
		:= ChargeDetailList} = ChargeInformation, State, Acc) ->
	CDL = [charge_detail(CD, State) || CD <- ChargeDetailList],
	Acc1 = Acc#{<<"chargeDetailList">> => CDL},
	charge_information4(ChargeInformation, Acc1);
charge_information3(ChargeInformation, _State, Acc) ->
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
charge_detail(#{chargeType := ChargeType} = ChargeDetail, State) ->
	Acc = #{<<"chargeType">> => ChargeType},
	charge_detail1(ChargeDetail, State, Acc);
charge_detail(ChargeDetail, State) ->
	charge_detail1(ChargeDetail, State, #{}).
%% @hidden
charge_detail1(#{charge := Charge} = ChargeDetail, State, Acc) ->
	Acc1 = Acc#{<<"charge">> => Charge},
	charge_detail2(ChargeDetail, State, Acc1);
charge_detail1(ChargeDetail, State, Acc) ->
	charge_detail2(ChargeDetail, State, Acc).
%% @hidden
charge_detail2(#{chargeableUnits
		:= ChargeableUnits} = ChargeDetail, State, Acc) ->
	Acc1 = Acc#{<<"chargeableUnits">> => ChargeableUnits},
	charge_detail3(ChargeDetail, State, Acc1);
charge_detail2(ChargeDetail, State, Acc) ->
	charge_detail3(ChargeDetail, State, Acc).
%% @hidden
charge_detail3(#{chargedUnits
		:= ChargedUnits} = ChargeDetail, State, Acc) ->
	Acc1 = Acc#{<<"chargedUnits">> => ChargedUnits},
	charge_detail4(ChargeDetail, State, Acc1);
charge_detail3(ChargeDetail, State, Acc) ->
	charge_detail4(ChargeDetail, State, Acc).
%% @hidden
charge_detail4(#{chargeDetailTimeStamp
		:= ChargeDetailTimeStamp} = _ChargeDetail, State, Acc) ->
	TS = timestamp(ChargeDetailTimeStamp, State),
	Acc#{<<"chargeDetailTimeStamp">> => TS};
charge_detail4(_ChargeDetail, _State, Acc) ->
	Acc.

%% @hidden
gprs_service_used(#{iMSSignallingContext
		:= IMSSignallingContext} = GprsServiceUsed, State) ->
	Acc = #{<<"iMSSignallingContext">> => IMSSignallingContext},
	gprs_service_used1(GprsServiceUsed, State, Acc);
gprs_service_used(GprsServiceUsed, State) ->
	gprs_service_used1(GprsServiceUsed, State, #{}).
%% @hidden
gprs_service_used1(#{dataVolumeIncoming
		:= DataVolumeIncoming} = GprsServiceUsed, State, Acc) ->
	Acc1 = Acc#{<<"dataVolumeIncoming">> => DataVolumeIncoming},
	gprs_service_used2(GprsServiceUsed, State, Acc1);
gprs_service_used1(GprsServiceUsed, State, Acc) ->
	gprs_service_used2(GprsServiceUsed, State, Acc).
%% @hidden
gprs_service_used2(#{dataVolumeOutgoing
		:= DataVolumeOutgoing} = GprsServiceUsed, State, Acc) ->
	Acc1 = Acc#{<<"dataVolumeOutgoing">> => DataVolumeOutgoing},
	gprs_service_used3(GprsServiceUsed, State, Acc1);
gprs_service_used2(GprsServiceUsed, State, Acc) ->
	gprs_service_used3(GprsServiceUsed, State, Acc).
%% @hidden
gprs_service_used3(#{chargeInformationList
		:= ChargeInformationList} = _GprsServiceUsed, State, Acc) ->
	CIL = [charge_information(CI, State) || CI <- ChargeInformationList],
	Acc#{<<"chargeInformationList">> => CIL};
gprs_service_used3(_GprsServiceUsed, _State, Acc) ->
	Acc.

