%%% cgf_ims.erl
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
%%% @doc This library module implements 3GPP CS CDR file handling in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% @reference 3GPP TS <a
%%% 	href="https://webapp.etsi.org/key/key.asp?GSMSpecPart1=32&amp;GSMSpecPart2=298"
%%% 	>32.298</a> Charging Data Record (CDR) Parameter Description.
%%%
-module(cgf_ims).
-copyright('Copyright (c) 2024 SigScale Global Inc.').

%% export the public API
-export([import/2, import/3]).

%% export the private API
-export([parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_ims public API
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
%% @doc Import CDR file and write to Bx interface log.
import(File, Log, Metadata) when is_list(File) ->
	case file:read_file(File) of
		{ok, Bin} ->
			import1(Log, Metadata, Bin);
		{error, Reason} ->
			{error, Reason}
	end;
import(Bin, Log, Metadata)
		when is_binary(Bin), is_list(Metadata) ->
	import1(Log, Metadata, Bin).

%% @hidden
import1(Log, Metadata, Bin) ->
	import2(Log, Metadata,
			'IMSChargingDataTypes':decode('IMSRecord', Bin)).

%% @hidden
import2(Log, Metadata, {ok, CDR, Rest}) ->
	case parse(Log, Metadata, CDR) of
		ok when byte_size(Rest) == 0 ->
			ok;
		ok ->
			import1(Log, Metadata, Rest);
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, element(1, CDR)}, {error, Reason}]),
			import1(Log, Metadata, Rest)
	end;
import2(_Log, _Metadata, {error, Reason}) ->
	{error, Reason}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec parse(Log, Metadata, CDR) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: sCSCFRecord | pCSCFRecord | iCSCFRecord |
				mRFCRecord | mGCFRecord | bGCFRecord | aSRecord |
				eCSCFRecord | iBCFRecord | tRFRecord | tFRecord |
				aTCFRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR.
%% @private
parse(Log, Metadata, {sCSCFRecord, SCSCFRecord} = _CDR) ->
	parse_scscf(Log, Metadata, SCSCFRecord);
parse(Log, Metadata, {pCSCFRecord, PCSCFRecord}) ->
	parse_pcscf(Log, Metadata, PCSCFRecord);
parse(Log, Metadata, {iCSCFRecord, ICSCFRecord}) ->
	parse_icscf(Log, Metadata, ICSCFRecord);
parse(Log, Metadata, {mRFCRecord, MRFCRecord}) ->
	parse_mrfc(Log, Metadata, MRFCRecord);
parse(Log, Metadata, {mGCFRecord, MGCFRecord}) ->
	parse_mgcf(Log, Metadata, MGCFRecord);
parse(Log, Metadata, {bGCFRecord, BGCFRecord}) ->
	parse_bgcf(Log, Metadata, BGCFRecord);
parse(Log, Metadata, {aSRecord, ASRecord}) ->
	parse_as(Log, Metadata, ASRecord);
parse(Log, Metadata, {eCSCFRecord, ECSCFRecord}) ->
	parse_ecscf(Log, Metadata, ECSCFRecord);
parse(Log, Metadata, {iBCFRecord, IBCFRecord}) ->
	parse_ibcf(Log, Metadata, IBCFRecord);
parse(Log, Metadata, {tRFRecord, TRFRecord}) ->
	parse_trf(Log, Metadata, TRFRecord);
parse(Log, Metadata, {tFRecord, TFRecord}) ->
	parse_tf(Log, Metadata, TFRecord);
parse(Log, Metadata, {aTCFRecord, ATCFRecord}) ->
	parse_atcf(Log, Metadata, ATCFRecord).

-spec parse_scscf(Log, Metadata, SCSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SCSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SCSCFRecord.
parse_scscf(Log, Metadata, SCSCFRecord) ->
	Call = scscf_record(SCSCFRecord),
	CDR = [{scscf_record, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_pcscf(Log, Metadata, PCSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		PCSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an PCSCFRecord.
parse_pcscf(Log, Metadata, PCSCFRecord) ->
	Call = pcscf_record(PCSCFRecord),
	CDR = [{pcscf_record, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_icscf(Log, Metadata, ICSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ICSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ICSCFRecord.
parse_icscf(_Log, _Metadata, _ICSCFRecord) ->
	{error, not_implemented}.

-spec parse_mrfc(Log, Metadata, MRFCRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MRFCRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MRFCRecord.
parse_mrfc(_Log, _Metadata, _MRFCRecord) ->
	{error, not_implemented}.

-spec parse_mgcf(Log, Metadata, MGCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MGCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MGCFRecord.
parse_mgcf(_Log, _Metadata, _MGCFRecord) ->
	{error, not_implemented}.

-spec parse_bgcf(Log, Metadata, BGCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		BGCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an BGCFRecord.
parse_bgcf(_Log, _Metadata, _BGCFRecord) ->
	{error, not_implemented}.

-spec parse_as(Log, Metadata, ASRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ASRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ASRecord.
parse_as(Log, Metadata, ASRecord) ->
	Call = as_record(ASRecord),
	CDR = [{ac_record, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_ecscf(Log, Metadata, ECSCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ECSCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ECSCFRecord.
parse_ecscf(_Log, _Metadata, _ECSCFRecord) ->
	{error, not_implemented}.

-spec parse_ibcf(Log, Metadata, IBCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		IBCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an IBCFRecord.
parse_ibcf(_Log, _Metadata, _IBCFRecord) ->
	{error, not_implemented}.

-spec parse_trf(Log, Metadata, TRFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TRFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an TRFRecord.
parse_trf(_Log, _Metadata, _TRFRecord) ->
	{error, not_implemented}.

-spec parse_tf(Log, Metadata, TFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an TFRecord.
parse_tf(_Log, _Metadata, _TFRecord) ->
	{error, not_implemented}.

-spec parse_atcf(Log, Metadata, ATCFRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		ATCFRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an ATCFRecord.
parse_atcf(_Log, _Metadata, _ATCFRecord) ->
	{error, not_implemented}.

%% @hidden
as_record(#{accessNetworkInformation := AccessNetworkInformation} = Record) ->
	Acc= #{<<"accessNetworkInformation">> =>
			cgf_lib:octet_string(AccessNetworkInformation)},
	as_record1(Record, Acc);
as_record(Record) ->
	as_record1(Record, #{}).
%% @hidden
as_record1(#{additionalAccessNetworkInformation := AdditionalAccessNetworkInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"additionalAccessNetworkInformation">> =>
			cgf_lib:octet_string(AdditionalAccessNetworkInformation)},
	as_record2(Record, Acc1);
as_record1(Record, Acc) ->
	as_record2(Record, Acc).
%% @hidden
as_record2(#{alternateChargedPartyAddress := AlternateChargedPartyAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"alternateChargedPartyAddress">> => AlternateChargedPartyAddress},
	as_record3(Record, Acc1);
as_record2(Record, Acc) ->
	as_record3(Record, Acc).
%% @hidden
as_record3(#{'called-Party-Address' := CalledPartyAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"called-Party-Address">> => cgf_lib:octet_string(CalledPartyAddress)},
	as_record4(Record, Acc1);
as_record3(Record, Acc) ->
	as_record4(Record, Acc).
%% @hidden
as_record4(#{carrierSelectRouting := CarrierSelectRouting} = Record, Acc) ->
	Acc1 = Acc#{<<"carrierSelectRouting">> => CarrierSelectRouting},
	as_record5(Record, Acc1);
as_record4(Record, Acc) ->
	as_record5(Record, Acc).
%% @hidden
as_record5(#{causeForRecordClosing := CauseForRecordClosing} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForRecordClosing">> => CauseForRecordClosing},
	as_record6(Record, Acc1);
as_record5(Record, Acc) ->
	as_record6(Record, Acc).
%% @hidden
as_record6(#{cellularNetworkInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"cellularNetworkInformation">> => cgf_lib:octet_string(Info)},
	as_record7(Record, Acc1);
as_record6(Record, Acc) ->
	as_record7(Record, Acc).
%% @hidden
as_record7(#{event := Event} = Record, Acc) ->
	Acc1 = Acc#{<<"event">> => Event},
	as_record8(Record, Acc1);
as_record7(Record, Acc) ->
	as_record8(Record, Acc).
%% @hidden
as_record8(#{expiresInformation := ExpiresInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"expiresInformation">> => ExpiresInformation},
	as_record9(Record, Acc1);
as_record8(Record, Acc) ->
	as_record9(Record, Acc).
%% @hidden
as_record9(#{fEIdentifierList := FEIdentifierList} = Record, Acc) ->
	Acc1 = Acc#{<<"fEIdentifierList">> => FEIdentifierList},
	as_record10(Record, Acc1);
as_record9(Record, Acc) ->
	as_record10(Record, Acc).
%% @hidden
as_record10(#{fromAddress := FromAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"fromAddress">> => cgf_lib:bcd_dn(FromAddress)},
	as_record11(Record, Acc1);
as_record10(Record, Acc) ->
	as_record11(Record, Acc).
%% @hidden
as_record11(#{gGSNaddress := GGSNAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"gGSNaddress">> => cgf_lib:ip_address(GGSNAddress)},
	as_record12(Record, Acc1);
as_record11(Record, Acc) ->
	as_record12(Record, Acc).
%% @hidden
as_record12(#{'iMS-Charging-Identifier' := IMSChargingIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"iMS-Charging-Identifier">> => IMSChargingIdentifier},
	as_record13(Record, Acc1);
as_record12(Record, Acc) ->
	as_record13(Record, Acc).
%% @hidden
as_record13(#{iMSCommunicationServiceIdentifier := IMSCommunicationServiceIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSCommunicationServiceIdentifier">> => IMSCommunicationServiceIdentifier},
	as_record14(Record, Acc1);
as_record13(Record, Acc) ->
	as_record14(Record, Acc).
%% @hidden
as_record14(#{iMSVisitedNetworkIdentifier := IMSVisitedNetworkIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSVisitedNetworkIdentifier">> => IMSVisitedNetworkIdentifier},
	as_record15(Record, Acc1);
as_record14(Record, Acc) ->
	as_record15(Record, Acc).
%% @hidden
as_record15(#{'incomplete-CDR-Indication' := IncompleteCDRIndication} = Record, Acc) ->
	Acc1 = Acc#{<<"incomplete-CDR-Indication">> => IncompleteCDRIndication},
	as_record16(Record, Acc1);
as_record15(Record, Acc) ->
	as_record16(Record, Acc).
%% @hidden
as_record16(#{initialIMSChargingIdentifier := InitialIMSChargingIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"initialIMSChargingIdentifier">> => InitialIMSChargingIdentifier},
	as_record17(Record, Acc1);
as_record16(Record, Acc) ->
	as_record17(Record, Acc).
%% @hidden
as_record17(#{instanceId := InstanceId} = Record, Acc) ->
	Acc1 = Acc#{<<"instanceId">> => InstanceId},
	as_record18(Record, Acc1);
as_record17(Record, Acc) ->
	as_record18(Record, Acc).
%% @hidden
as_record18(#{interOperatorIdentifiers := InterOperatorIdentifiers} = Record, Acc) ->
	Acc1 = Acc#{<<"interOperatorIdentifiers">> => InterOperatorIdentifiers},
	as_record19(Record, Acc1);
as_record18(Record, Acc) ->
	as_record19(Record, Acc).
%% @hidden
as_record19(#{'list-Of-AccessNetworkInfoChange' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-AccessNetworkInfoChange">> => access_info(List)},
	as_record20(Record, Acc1);
as_record19(Record, Acc) ->
	as_record20(Record, Acc).
%% @hidden
as_record20(#{'list-Of-AccessTransferInformation' := ListOfAccessTransferInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-AccessTransferInformation">> => ListOfAccessTransferInformation},
	as_record21(Record, Acc1);
as_record20(Record, Acc) ->
	as_record21(Record, Acc).
%% @hidden
as_record21(#{'list-Of-Called-Asserted-Identity' := ListOfCalledAssertedIdentity} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-Called-Asserted-Identity">> => ListOfCalledAssertedIdentity},
	as_record22(Record, Acc1);
as_record21(Record, Acc) ->
	as_record22(Record, Acc).
%% @hidden
as_record22(#{'list-Of-Calling-Party-Address' := List} = Record, Acc) ->
	PartyList = [party_address(Party) || Party <- List],
	Acc1 = Acc#{<<"listOfCallingPartyAddress">> => PartyList},
	as_record23(Record, Acc1);
as_record22(Record, Acc) ->
	as_record23(Record, Acc).
%% @hidden
as_record23(#{'list-Of-Early-SDP-Media-Components' := List} = Record, Acc) ->
	Components = [media_components(Media) || Media <- List],
	Acc1 = Acc#{<<"listOfEarlySDPMediaComponents">> => Components},
	as_record24(Record, Acc1);
as_record23(Record, Acc) ->
	as_record24(Record, Acc).
%% @hidden
as_record24(#{'list-Of-Message-Bodies' := ListOfMessageBodies} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-Message-Bodies">> => ListOfMessageBodies},
	as_record25(Record, Acc1);
as_record24(Record, Acc) ->
	as_record25(Record, Acc).
%% @hidden
as_record25(#{'list-Of-SDP-Media-Components' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-SDP-Media-Components">> => media_components(List)},
	as_record26(Record, Acc1);
as_record25(Record, Acc) ->
	as_record26(Record, Acc).
%% @hidden
as_record26(#{'list-of-Requested-Party-Address' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-of-Requested-Party-Address">> => party_address(List)},
	as_record27(Record, Acc1);
as_record26(Record, Acc) ->
	as_record27(Record, Acc).
%% @hidden
as_record27(#{'list-of-subscription-ID' := ListOfSubscriptionID} = Record, Acc) ->
	Acc1 = Acc#{<<"list-of-subscription-ID">> => ListOfSubscriptionID},
	as_record28(Record, Acc1);
as_record27(Record, Acc) ->
	as_record28(Record, Acc).
%% @hidden
as_record28(#{listOfCalledIdentityChanges := ListOfCalledIdentityChanges} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfCalledIdentityChanges">> => ListOfCalledIdentityChanges},
	as_record29(Record, Acc1);
as_record28(Record, Acc) ->
	as_record29(Record, Acc).
%% @hidden
as_record29(#{listOfReasonHeader := ListOfReasonHeader} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfReasonHeader">> => ListOfReasonHeader},
	as_record30(Record, Acc1);
as_record29(Record, Acc) ->
	as_record30(Record, Acc).
%% @hidden
as_record30(#{localRecordSequenceNumber := LocalRecordSequenceNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"localRecordSequenceNumber">> => LocalRecordSequenceNumber},
	as_record31(Record, Acc1);
as_record30(Record, Acc) ->
	as_record31(Record, Acc).
%% @hidden
as_record31(#{mSTimeZone := MSTimeZone} = Record, Acc) ->
	Acc1 = Acc#{<<"mSTimeZone">> => cgf_lib:bcd_date_time(MSTimeZone)},
	as_record32(Record, Acc1);
as_record31(Record, Acc) ->
	as_record32(Record, Acc).
%% @hidden
as_record32(#{'msc-Address' := MSCAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"msc-Address">> => MSCAddress},
	as_record33(Record, Acc1);
as_record32(Record, Acc) ->
	as_record33(Record, Acc).
%% @hidden
as_record33(#{'nNI-Information' := NNIInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"nNI-Information">> => NNIInformation},
	as_record34(Record, Acc1);
as_record33(Record, Acc) ->
	as_record34(Record, Acc).
%% @hidden
as_record34(#{nodeAddress := NodeAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"nodeAddress">> => NodeAddress},
	as_record35(Record, Acc1);
as_record34(Record, Acc) ->
	as_record35(Record, Acc).
%% @hidden
as_record35(#{numberPortabilityRouting := NumberPortabilityRouting} = Record, Acc) ->
	Acc1 = Acc#{<<"numberPortabilityRouting">> => NumberPortabilityRouting},
	as_record36(Record, Acc1);
as_record35(Record, Acc) ->
	as_record36(Record, Acc).
%% @hidden
as_record36(#{'online-charging-flag' := OnlineChargingFlag} = Record, Acc) ->
	Acc1 = Acc#{<<"online-charging-flag">> => OnlineChargingFlag},
	as_record37(Record, Acc1);
as_record36(Record, Acc) ->
	as_record37(Record, Acc).
%% @hidden
as_record37(#{outgoingSessionId := OutgoingSessionId} = Record, Acc) ->
	Acc1 = Acc#{<<"outgoingSessionId">> => OutgoingSessionId},
	as_record38(Record, Acc1);
as_record37(Record, Acc) ->
	as_record38(Record, Acc).
%% @hidden
as_record38(#{privateUserID := PrivateUserID} = Record, Acc) ->
	Acc1 = Acc#{<<"privateUserID">> => PrivateUserID},
	as_record39(Record, Acc1);
as_record38(Record, Acc) ->
	as_record39(Record, Acc).
%% @hidden
as_record39(#{realTimeTariffInformation := RealTimeTariffInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"realTimeTariffInformation">> => RealTimeTariffInformation},
	as_record40(Record, Acc1);
as_record39(Record, Acc) ->
	as_record40(Record, Acc).
%% @hidden
as_record40(#{recordClosureTime := RecordClosureTime} = Record, Acc) ->
	Acc1 = Acc#{<<"recordClosureTime">> => cgf_lib:bcd_date_time(RecordClosureTime)},
	as_record41(Record, Acc1);
as_record40(Record, Acc) ->
	as_record41(Record, Acc).
%% @hidden
as_record41(#{recordExtensions := RecordExtensions} = Record, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => RecordExtensions},
	as_record42(Record, Acc1);
as_record41(Record, Acc) ->
	as_record42(Record, Acc).
%% @hidden
as_record42(#{recordOpeningTime := RecordOpeningTime} = Record, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(RecordOpeningTime)},
	as_record43(Record, Acc1);
as_record42(Record, Acc) ->
	as_record43(Record, Acc).
%% @hidden
as_record43(#{recordSequenceNumber := RecordSequenceNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => RecordSequenceNumber},
	as_record44(Record, Acc1);
as_record43(Record, Acc) ->
	as_record44(Record, Acc).
%% @hidden
as_record44(#{'requested-Party-Address' := RequestedPartyAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"requested-Party-Address">> => RequestedPartyAddress},
	as_record45(Record, Acc1);
as_record44(Record, Acc) ->
	as_record45(Record, Acc).
%% @hidden
as_record45(#{retransmission := Retransmission} = Record, Acc) ->
	Acc1 = Acc#{<<"retransmission">> => Retransmission},
	as_record46(Record, Acc1);
as_record45(Record, Acc) ->
	as_record46(Record, Acc).
%% @hidden
as_record46(#{'role-of-Node' := RoleOfNode} = Record, Acc) ->
	Acc1 = Acc#{<<"role-of-Node">> => RoleOfNode},
	as_record47(Record, Acc1);
as_record46(Record, Acc) ->
	as_record47(Record, Acc).
%% @hidden
as_record47(#{'sIP-Method' := SIPMethod} = Record, Acc) ->
	Acc1 = Acc#{<<"sIP-Method">> => SIPMethod},
	as_record48(Record, Acc1);
as_record47(Record, Acc) ->
	as_record48(Record, Acc).
%% @hidden
as_record48(#{serviceContextID := ServiceContextID} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceContextID">> => ServiceContextID},
	as_record49(Record, Acc1);
as_record48(Record, Acc) ->
	as_record49(Record, Acc).
%% @hidden
as_record49(#{serviceDeliveryEndTimeStamp := ServiceDeliveryEndTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStamp">> =>
		cgf_lib:bcd_date_time(ServiceDeliveryEndTimeStamp)},
	as_record50(Record, Acc1);
as_record49(Record, Acc) ->
	as_record50(Record, Acc).
%% @hidden
as_record50(#{serviceDeliveryEndTimeStampFraction := ServiceDeliveryEndTimeStampFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStampFraction">> =>
		cgf_lib:bcd_date_time(ServiceDeliveryEndTimeStampFraction)},
	as_record51(Record, Acc1);
as_record50(Record, Acc) ->
	as_record51(Record, Acc).
%% @hidden
as_record51(#{serviceDeliveryStartTimeStamp := ServiceDeliveryStartTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryStartTimeStamp">> =>
			cgf_lib:bcd_date_time(ServiceDeliveryStartTimeStamp)},
	as_record52(Record, Acc1);
as_record51(Record, Acc) ->
	as_record52(Record, Acc).
%% @hidden
as_record52(#{serviceDeliveryStartTimeStampFraction := ServiceDeliveryStartTimeStampFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryStartTimeStampFraction">> =>
			cgf_lib:bcd_date_time(ServiceDeliveryStartTimeStampFraction)},
	as_record53(Record, Acc1);
as_record52(Record, Acc) ->
	as_record53(Record, Acc).
%% @hidden
as_record53(#{serviceReasonReturnCode := ServiceReasonReturnCode} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceReasonReturnCode">> => ServiceReasonReturnCode},
	as_record54(Record, Acc1);
as_record53(Record, Acc) ->
as_record54(Record, Acc).
%% @hidden
as_record54(#{serviceRequestTimeStamp := ServiceRequestTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStamp">> =>
			cgf_lib:bcd_date_time(ServiceRequestTimeStamp)},
	as_record55(Record, Acc1);
as_record54(Record, Acc) ->
	as_record55(Record, Acc).
as_record55(#{serviceRequestTimeStampFraction := ServiceRequestTimeStampFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStampFraction">> =>
			cgf_lib:bcd_date_time(ServiceRequestTimeStampFraction)},
	as_record56(Record, Acc1);
as_record55(Record, Acc) ->
	as_record56(Record, Acc).
%% @hidden
as_record56(#{serviceSpecificInfo := ServiceSpecificInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceSpecificInfo">> => ServiceSpecificInfo},
	as_record57(Record, Acc1);
as_record56(Record, Acc) ->
	as_record57(Record, Acc).
%% @hidden
as_record57(#{'session-Id' := SessionId} = Record, Acc) ->
	Acc1 = Acc#{<<"session-Id">> => SessionId},
	as_record58(Record, Acc1);
as_record57(Record, Acc) ->
	as_record58(Record, Acc).
%% @hidden
as_record58(#{sessionPriority := SessionPriority} = Record, Acc) ->
	Acc1 = Acc#{<<"sessionPriority">> => SessionPriority},
	as_record59(Record, Acc1);
as_record58(Record, Acc) ->
	as_record59(Record, Acc).
%% @hidden
as_record59(#{subscriberEquipmentNumber := SubscriberEquipmentNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"subscriberEquipmentNumber">> => SubscriberEquipmentNumber},
	as_record60(Record, Acc1);
as_record59(Record, Acc) ->
	as_record60(Record, Acc).
%% @hidden
as_record60(#{'tADS-Identifier' := TADSIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"tADS-Identifier">> => TADSIdentifier},
	as_record61(Record, Acc1);
as_record60(Record, Acc) ->
	as_record61(Record, Acc).
%% @hidden
as_record61(#{'threeGPPPSDataOffStatus' := ThreeGPPPSDataOffStatus} = Record, Acc) ->
	Acc1 = Acc#{<<"threeGPPPSDataOffStatus">> => ThreeGPPPSDataOffStatus},
	as_record62(Record, Acc1);
as_record61(Record, Acc) ->
	as_record62(Record, Acc).
%% @hidden
as_record62(#{'transit-IOI-Lists' := TransitIOILists} = Record, Acc) ->
	Acc1 = Acc#{<<"transit-IOI-Lists">> => TransitIOILists},
	as_record63(Record, Acc1);
as_record62(Record, Acc) ->
	as_record63(Record, Acc).
%% @hidden
as_record63(#{userLocationInformation := UserLocationInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => UserLocationInformation},
	as_record64(Record, Acc1);
as_record63(Record, Acc) ->
	as_record64(Record, Acc).
%% @hidden
as_record64(#{'vlr-Number' := VLRNumber} = _Record, Acc) ->
	Acc#{<<"vlr-Number">> => VLRNumber};
as_record64(_Record, Acc) ->
	Acc.

%% @hidden
scscf_record(#{listOfCalledIdentityChanges := List} = Record) ->
	Acc = #{<<"listOfCalledIdentityChanges">> => identity_changes(List)},
	scscf_record1(Record, Acc);
scscf_record(Record) ->
	scscf_record1(Record, #{}).
%% @hidden
scscf_record1(#{'list-Of-Associated-URI' := Uri} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfAssociatedURI">> => associated_uri(Uri)},
	scscf_record2(Record, Acc1);
scscf_record1(Record, Acc) ->
	scscf_record2(Record, Acc).
%% @hidden
scscf_record2(#{iMSCommunicationServiceIdentifier := Identifier} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSCommunicationServiceIdentifier">> => Identifier},
	scscf_record3(Record, Acc1);
scscf_record2(Record, Acc) ->
	scscf_record3(Record, Acc).
%% @hidden
scscf_record3(#{'list-Of-AccessNetworkInfoChange' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfAccessNetworkInfoChange">> => access_info(List)},
	scscf_record4(Record, Acc1);
scscf_record3(Record, Acc) ->
	scscf_record4(Record, Acc).
%% @hidden
scscf_record4(#{realTimeTariffInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"realTimeTariffInformation">> => Info},
	scscf_record5(Record, Acc1);
scscf_record4(Record, Acc) ->
	scscf_record5(Record, Acc).
%% @hidden
scscf_record5(#{iMSVisitedNetworkIdentifier := Id} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSVisitedNetworkIdentifier">> => Id},
	scscf_record6(Record, Acc1);
scscf_record5(Record, Acc) ->
	scscf_record6(Record, Acc).
%% @hidden
scscf_record6(#{expiresInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"expiresInformation">> => Info},
	scscf_record7(Record, Acc1);
scscf_record6(Record, Acc) ->
	scscf_record7(Record, Acc).
%% @hidden
scscf_record7(#{'list-Of-Message-Bodies' := Bodies} = Record, Acc) ->
	MessageBodies = [message_body(Message) || Message <- Bodies],
	Acc1 = Acc#{<<"listOfMessageBodies">> => MessageBodies},
	scscf_record8(Record, Acc1);
scscf_record7(Record, Acc) ->
	scscf_record8(Record, Acc).
%% @hidden
scscf_record8(#{listOfReasonHeader := Reason} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfReasonHeader">> => Reason},
	scscf_record9(Record, Acc1);
scscf_record8(Record, Acc) ->
	scscf_record9(Record, Acc).
%% @hidden
scscf_record9(#{serviceDeliveryEndTimeStampFraction := Fraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStampFraction">> =>
				cgf_lib:bcd_date_time(Fraction)},
	scscf_record10(Record, Acc1);
scscf_record9(Record, Acc) ->
	scscf_record10(Record, Acc).
%% @hidden
scscf_record10(#{recordExtensions := Extensions} = Record, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => Extensions},
	scscf_record11(Record, Acc1);
scscf_record10(Record, Acc) ->
	scscf_record11(Record, Acc).
%% @hidden
scscf_record11(#{applicationServersInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"applicationServersInformation">> => server_info(Info)},
	scscf_record12(Record, Acc1);
scscf_record11(Record, Acc) ->
	scscf_record12(Record, Acc).
%% @hidden
scscf_record12(#{serviceRequestTimeStampFraction := Fraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStampFraction">> =>
			 cgf_lib:bcd_date_time(Fraction)},
	scscf_record13(Record, Acc1);
scscf_record12(Record, Acc) ->
	scscf_record13(Record, Acc).
%% @hidden
scscf_record13(#{serviceRequestTimeStamp := Timestamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStamp">> =>
			 cgf_lib:bcd_date_time(Timestamp)},
	scscf_record14(Record, Acc1);
scscf_record13(Record, Acc) ->
	scscf_record14(Record, Acc).
%% @hidden
scscf_record14(#{serviceDeliveryEndTimeStamp := Timestamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStamp">> =>
			 cgf_lib:bcd_date_time(Timestamp)},
	scscf_record15(Record, Acc1);
scscf_record14(Record, Acc) ->
	scscf_record15(Record, Acc).
%% @hidden
scscf_record15(#{causeForRecordClosing := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForRecordClosing">> => Cause},
	scscf_record16(Record, Acc1);
scscf_record15(Record, Acc) ->
	scscf_record16(Record, Acc).
%% @hidden
scscf_record16(#{serviceReasonReturnCode := Reason} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceReasonReturnCode">> => Reason},
	scscf_record17(Record, Acc1);
scscf_record16(Record, Acc) ->
	scscf_record17(Record, Acc).
%% @hidden
scscf_record17(#{recordSequenceNumber := Sequence} = Record, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => Sequence},
	scscf_record18(Record, Acc1);
scscf_record17(Record, Acc) ->
	scscf_record18(Record, Acc).
%% @hidden
scscf_record18(#{cellularNetworkInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"cellularNetworkInformation">> => cgf_lib:bcd_dn(Info)},
	scscf_record19(Record, Acc1);
scscf_record18(Record, Acc) ->
	scscf_record19(Record, Acc).
%% @hidden
scscf_record19(#{userLocationInformation := Location} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:bcd_dn(Location)},
	scscf_record20(Record, Acc1);
scscf_record19(Record, Acc) ->
	scscf_record20(Record, Acc).
%% @hidden
scscf_record20(#{recordType := Type} = Record, Acc) ->
	Acc1 = Acc#{<<"recordType">> => Type},
	scscf_record21(Record, Acc1);
scscf_record20(Record, Acc) ->
	scscf_record21(Record, Acc).
%% @hidden
scscf_record21(#{recordOpeningTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(Time)},
	scscf_record22(Record, Acc1);
scscf_record21(Record, Acc) ->
	scscf_record22(Record, Acc).
%% @hidden
scscf_record22(#{'list-Of-Called-Asserted-Identity' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfCalledAssertedIdentity">> => asserted_identity(List)},
	scscf_record23(Record, Acc1);
scscf_record22(Record, Acc) ->
	scscf_record23(Record, Acc).
%% @hidden
scscf_record23(#{instanceId := Id} = Record, Acc) ->
	Acc1 = Acc#{<<"instanceId">> => Id},
	scscf_record24(Record, Acc1);
scscf_record23(Record, Acc) ->
	scscf_record24(Record, Acc).
%% @hidden
scscf_record24(#{mSTimeZone := TimeZone} = Record, Acc) ->
	Acc1 = Acc#{<<"mSTimeZone">> => TimeZone},
	scscf_record25(Record, Acc1);
scscf_record24(Record, Acc) ->
	scscf_record25(Record, Acc).
%% @hidden
scscf_record25(#{routeHeaderReceived := Route} = Record, Acc) ->
	Acc1 = Acc#{<<"routeHeaderReceived">> => Route},
	scscf_record26(Record, Acc1);
scscf_record25(Record, Acc) ->
	scscf_record26(Record, Acc).
%% @hidden
scscf_record26(#{'transit-IOI-Lists' := IOI} = Record, Acc) ->
	Acc1 = Acc#{<<"transitIOILists">> => IOI},
	scscf_record27(Record, Acc1);
scscf_record26(Record, Acc) ->
	scscf_record27(Record, Acc).
%% @hidden
scscf_record27(#{routeHeaderTransmitted := Route} = Record, Acc) ->
	Acc1 = Acc#{<<"routeHeaderTransmitted">> => Route},
	scscf_record28(Record, Acc1);
scscf_record27(Record, Acc) ->
	scscf_record28(Record, Acc).
%% @hidden
scscf_record28(#{gGSNaddress := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"gGSNaddress">> => cgf_lib:ip_address(Address)},
	scscf_record29(Record, Acc1);
scscf_record28(Record, Acc) ->
	scscf_record29(Record, Acc).
%% @hidden
scscf_record29(#{'list-Of-SDP-Media-Components' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfSDPMediaComponents">> => media_components(List)},
	scscf_record30(Record, Acc1);
scscf_record29(Record, Acc) ->
	scscf_record30(Record, Acc).
%% @hidden
scscf_record30(#{'requested-Party-Address' := Address} = Record, Acc) ->
	Acc1 = Acc#{<<"requestedPartyAddress">> => cgf_lib:ip_address(Address)},
	scscf_record31(Record, Acc1);
scscf_record30(Record, Acc) ->
	scscf_record31(Record, Acc).
%% @hidden
scscf_record31(#{sessionPriority := Priority} = Record, Acc) ->
	Acc1 = Acc#{<<"sessionPriority">> => Priority},
	scscf_record32(Record, Acc1);
scscf_record31(Record, Acc) ->
	scscf_record32(Record, Acc).
%% @hidden
scscf_record32(#{carrierSelectRouting := Routing} = Record, Acc) ->
	Acc1 = Acc#{<<"carrierSelectRouting">> => Routing},
	scscf_record33(Record, Acc1);
scscf_record32(Record, Acc) ->
	scscf_record33(Record, Acc).
%%%% @hidden
scscf_record33(#{serviceContextID := ContextID} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceContextID">> => ContextID},
	scscf_record34(Record, Acc1);
scscf_record33(Record, Acc) ->
	scscf_record34(Record, Acc).
%% @hidden
scscf_record34(#{subscriberEquipmentNumber := EquipmentNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"subscriberEquipmentNumber">> => EquipmentNumber},
	scscf_record35(Record, Acc1);
scscf_record34(Record, Acc) ->
	scscf_record35(Record, Acc).
%% @hidden
scscf_record35(#{nodeAddress := NodeAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"nodeAddress">> => NodeAddress},
	scscf_record36(Record, Acc1);
scscf_record35(Record, Acc) ->
	scscf_record36(Record, Acc).
%% @hidden
scscf_record36(#{fromAddress := FromAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"fromAddress">> => cgf_lib:bcd_dn(FromAddress)},
	scscf_record37(Record, Acc1);
scscf_record36(Record, Acc) ->
	scscf_record37(Record, Acc).
%% @hidden
scscf_record37(#{'list-of-subscription-ID' := SubscriptionList} = Record, Acc) ->
	Acc1 = Acc#{<<"listofsubscriptionID">> => SubscriptionList},
	scscf_record38(Record, Acc1);
scscf_record37(Record, Acc) ->
	scscf_record38(Record, Acc).
%% @hidden
scscf_record38(#{'nNI-Information' := NNIInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"nNIInformation">> => NNIInfo},
	scscf_record39(Record, Acc1);
scscf_record38(Record, Acc) ->
	scscf_record39(Record, Acc).
%% @hidden
scscf_record39(#{event := Event} = Record, Acc) ->
	Acc1 = Acc#{<<"event">> => Event},
	scscf_record40(Record, Acc1);
scscf_record39(Record, Acc) ->
	scscf_record40(Record, Acc).
%% @hidden
scscf_record40(#{'list-Of-Calling-Party-Address' := List} = Record, Acc) ->
	PartyList = [party_address(Party) || Party <- List],
	Acc1 = Acc#{<<"listOfCallingPartyAddress">> => PartyList},
	scscf_record41(Record, Acc1);
scscf_record40(Record, Acc) ->
	scscf_record41(Record, Acc).
%% @hidden
scscf_record41(#{'list-Of-Early-SDP-Media-Components' := MediaList} = Record, Acc) ->
	Components = [media_components(Media) || Media <- MediaList],
	Acc1 = Acc#{<<"listOfEarlySDPMediaComponents">> => Components},
	scscf_record42(Record, Acc1);
scscf_record41(Record, Acc) ->
	scscf_record42(Record, Acc).
%% @hidden
scscf_record42(#{iMSEmergencyIndicator := _EmergencyIndicator} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSEmergencyIndicator">> => undefined},
	scscf_record43(Record, Acc1);
scscf_record42(Record, Acc) ->
	scscf_record43(Record, Acc).
%% @hidden
scscf_record43(#{'role-of-Node' := RoleNode} = Record, Acc) ->
	Acc1 = Acc#{<<"roleofNode">> => RoleNode},
	scscf_record44(Record, Acc1);
scscf_record43(Record, Acc) ->
	scscf_record44(Record, Acc).
%% @hidden
scscf_record44(#{numberPortabilityRouting := Routing} = Record, Acc) ->
	Acc1 = Acc#{<<"numberPortabilityRouting">> => Routing},
	scscf_record45(Record, Acc1);
scscf_record44(Record, Acc) ->
	scscf_record45(Record, Acc).
%% @hidden
scscf_record45(#{privateUserID := UserID} = Record, Acc) ->
	Acc1 = Acc#{<<"privateUserID">> => UserID},
	scscf_record46(Record, Acc1);
scscf_record45(Record, Acc) ->
	scscf_record46(Record, Acc).
%% @hidden
scscf_record46(#{'sIP-Method' := SIPMethod} = Record, Acc) ->
	Acc1 = Acc#{<<"sIPMethod">> => SIPMethod},
	scscf_record47(Record, Acc1);
scscf_record46(Record, Acc) ->
	scscf_record47(Record, Acc).
%% @hidden
scscf_record47(#{fEIdentifierList := FEList} = Record, Acc) ->
	Acc1 = Acc#{<<"fEIdentifierList">> => FEList},
	scscf_record48(Record, Acc1);
scscf_record47(Record, Acc) ->
	scscf_record48(Record, Acc).
%% @hidden
scscf_record48(#{'iMS-Charging-Identifier' := ChargingID} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSChargingIdentifier">> => ChargingID},
	scscf_record49(Record, Acc1);
scscf_record48(Record, Acc) ->
	scscf_record49(Record, Acc).
%% @hidden
scscf_record49(#{serviceDeliveryStartTimeStampFraction := Fraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryStartTimeStampFraction">> =>
				cgf_lib:bcd_date_time(Fraction)},
	scscf_record50(Record, Acc1);
scscf_record49(Record, Acc) ->
	scscf_record50(Record, Acc).
%% @hidden
scscf_record50(#{localRecordSequenceNumber := SequenceNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"localRecordSequenceNumber">> => SequenceNumber},
	scscf_record51(Record, Acc1);
scscf_record50(Record, Acc) ->
	scscf_record51(Record, Acc).
%% @hidden
scscf_record51(#{'called-Party-Address' := PartyAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"called_Party_Address">> => cgf_lib:bcd_dn(PartyAddress)},
	scscf_record52(Record, Acc1);
scscf_record51(Record, Acc) ->
	scscf_record52(Record, Acc).
%% @hidden
scscf_record52(#{retransmission := _Retransmission} = Record, Acc) ->
	Acc1 = Acc#{<<"retransmission">> => undefined},
	scscf_record53(Record, Acc1);
scscf_record52(Record, Acc) ->
	scscf_record53(Record, Acc).
%% @hidden
scscf_record53(#{additionalAccessNetworkInformation := AdditionalInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"additionalAccessNetworkInformation">> => AdditionalInfo},
	scscf_record54(Record, Acc1);
scscf_record53(Record, Acc) ->
	scscf_record54(Record, Acc).
%% @hidden
scscf_record54(#{recordClosureTime := ClosureTime} = _Record, Acc) ->
	Acc#{<<"recordClosureTime">> => cgf_lib:bcd_date_time(ClosureTime)};
scscf_record54(_Record, Acc) ->
	Acc.

%% @hidden
identity_changes(_Identity) ->
	{error, not_implmented}.

%% @hidden
associated_uri(_Uri) ->
	{error, not_implmented}.

-dialyzer({no_unused, network_info_change/1}).
%% @hidden
network_info_change(_List) ->
	{error, not_implmented}.

%% @hidden
message_body(_Body) ->
	{error, not_implmented}.

%% @hidden
server_info(_Info) ->
	{error, not_implmented}.

%% @hidden
asserted_identity(_List) ->
	{error, not_implmented}.

%% @hidden
party_address(_Party) ->
	{error, not_implmented}.

%% @hidden
media_components(_Media) ->
	{error, not_implmented}.

%% @hidden
pcscf_record(#{listOfCalledIdentityChanges := List} = Record) ->
	Acc = #{<<"listOfCalledIdentityChanges">> => identity_changes(List)},
	pcscf_record1(Record, Acc);
pcscf_record(Record) ->
	pcscf_record1(Record, #{}).
%% @hidden
pcscf_record1(#{'list-Of-Associated-URI' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-Associated-URI">> => associated_uri(List)},
	pcscf_record2(Record, Acc1);
pcscf_record1(Record, Acc) ->
	pcscf_record2(Record, Acc).
%% @hidden
pcscf_record2(#{iMSCommunicationServiceIdentifier := ServiceId} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSCommunicationServiceIdentifier">> => ServiceId},
	pcscf_record3(Record, Acc1);
pcscf_record2(Record, Acc) ->
	pcscf_record3(Record, Acc).
%% @hidden
pcscf_record3(#{'list-Of-AccessNetworkInfoChange' := AccessInfoChange} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-AccessNetworkInfoChange">> => access_info(AccessInfoChange)},
	pcscf_record4(Record, Acc1);
pcscf_record3(Record, Acc) ->
	pcscf_record4(Record, Acc).
%% @hidden
pcscf_record4(#{'list-Of-AccessTransferInformation' := TransferInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-AccessTransferInformation">> => TransferInfo},
	pcscf_record5(Record, Acc1);
pcscf_record4(Record, Acc) ->
	pcscf_record5(Record, Acc).
%% @hidden
pcscf_record5(#{iMSVisitedNetworkIdentifier := VisitedNetworkId} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSVisitedNetworkIdentifier">> => VisitedNetworkId},
	pcscf_record6(Record, Acc1);
pcscf_record5(Record, Acc) ->
	pcscf_record6(Record, Acc).
%% @hidden
pcscf_record6(#{expiresInformation := ExpiresInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"expiresInformation">> => ExpiresInfo},
	pcscf_record7(Record, Acc1);
pcscf_record6(Record, Acc) ->
	pcscf_record7(Record, Acc).
%% @hidden
pcscf_record7(#{'list-Of-Message-Bodies' := MessageBodies} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-Message-Bodies">> => message_bodies(MessageBodies)},
	pcscf_record8(Record, Acc1);
pcscf_record7(Record, Acc) ->
	pcscf_record8(Record, Acc).
%% @hidden
pcscf_record8(#{listOfReasonHeader := ReasonHeader} = Record, Acc) ->
	Acc1 = Acc#{<<"listOfReasonHeader">> => ReasonHeader},
	pcscf_record9(Record, Acc1);
pcscf_record8(Record, Acc) ->
	pcscf_record9(Record, Acc).
%% @hidden
pcscf_record9(#{serviceDeliveryEndTimeStampFraction := TimeStampFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStampFraction">> =>
			cgf_lib:bcd_date_time(TimeStampFraction)},
	pcscf_record10(Record, Acc1);
pcscf_record9(Record, Acc) ->
	pcscf_record10(Record, Acc).
%% @hidden
pcscf_record10(#{recordExtensions := Extensions} = Record, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => Extensions},
	pcscf_record11(Record, Acc1);
pcscf_record10(Record, Acc) ->
	pcscf_record11(Record, Acc).
%% @hidden
pcscf_record11(#{'initialIMS-Charging-Identifier' := ChargingId} = Record, Acc) ->
	Acc1 = Acc#{<<"initialIMS-Charging-Identifier">> => ChargingId},
	pcscf_record12(Record, Acc1);
pcscf_record11(Record, Acc) ->
	pcscf_record12(Record, Acc).
%% @hidden
pcscf_record12(#{serviceRequestTimeStampFraction := RequestTimeFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStampFraction">> =>
			cgf_lib:bcd_date_time(RequestTimeFraction)},
	pcscf_record13(Record, Acc1);
pcscf_record12(Record, Acc) ->
	pcscf_record13(Record, Acc).
%% @hidden
pcscf_record13(#{serviceRequestTimeStamp := RequestTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceRequestTimeStamp">> =>
			cgf_lib:bcd_date_time(RequestTimeStamp)},
	pcscf_record14(Record, Acc1);
pcscf_record13(Record, Acc) ->
	pcscf_record14(Record, Acc).
%% @hidden
pcscf_record14(#{serviceDeliveryEndTimeStamp := EndTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryEndTimeStamp">> =>
			cgf_lib:bcd_date_time(EndTimeStamp)},
	pcscf_record15(Record, Acc1);
pcscf_record14(Record, Acc) ->
	pcscf_record15(Record, Acc).
%% @hidden
pcscf_record15(#{causeForRecordClosing := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForRecordClosing">> => Cause},
	pcscf_record16(Record, Acc1);
pcscf_record15(Record, Acc) ->
	pcscf_record16(Record, Acc).
%% @hidden
pcscf_record16(#{serviceReasonReturnCode := ReasonCode} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceReasonReturnCode">> => ReasonCode},
	pcscf_record17(Record, Acc1);
pcscf_record16(Record, Acc) ->
	pcscf_record17(Record, Acc).
%% @hidden
pcscf_record17(#{'list-of-Requested-Party-Address' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-of-Requested-Party-Address">> => party_list(List)},
	pcscf_record18(Record, Acc1);
pcscf_record17(Record, Acc) ->
	pcscf_record18(Record, Acc).
%% @hidden
pcscf_record18(#{recordSequenceNumber := SeqNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"recordSequenceNumber">> => SeqNumber},
	pcscf_record19(Record, Acc1);
pcscf_record18(Record, Acc) ->
	pcscf_record19(Record, Acc).
%% @hidden
pcscf_record19(#{cellularNetworkInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"cellularNetworkInformation">> => cgf_lib:bcd_dn(Info)},
	pcscf_record20(Record, Acc1);
pcscf_record19(Record, Acc) ->
	pcscf_record20(Record, Acc).
%% @hidden
pcscf_record20(#{userLocationInformation := Info} = Record, Acc) ->
	Acc1 = Acc#{<<"userLocationInformation">> => cgf_lib:bcd_dn(Info)},
	pcscf_record21(Record, Acc1);
pcscf_record20(Record, Acc) ->
	pcscf_record21(Record, Acc).
%% @hidden
pcscf_record21(#{relatedICIDGenerationNode := IcidNode} = Record, Acc) ->
	Acc1 = Acc#{<<"relatedICIDGenerationNode">> => IcidNode},
	pcscf_record22(Record, Acc1);
pcscf_record21(Record, Acc) ->
	pcscf_record22(Record, Acc).
%% @hidden
pcscf_record22(#{recordType := RecordType} = Record, Acc) ->
	Acc1 = Acc#{<<"recordType">> => RecordType},
	pcscf_record23(Record, Acc1);
pcscf_record22(Record, Acc) ->
	pcscf_record23(Record, Acc).
%% @hidden
pcscf_record23(#{recordOpeningTime := OpeningTime} = Record, Acc) ->
	Acc1 = Acc#{<<"recordOpeningTime">> => cgf_lib:bcd_date_time(OpeningTime)},
	pcscf_record24(Record, Acc1);
pcscf_record23(Record, Acc) ->
	pcscf_record24(Record, Acc).
%% @hidden
pcscf_record24(#{<<"list-Of-Called-Asserted-Identity">> := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-Called-Asserted-Identity">> => party_list(List)},
	pcscf_record25(Record, Acc1);
pcscf_record24(Record, Acc) ->
	pcscf_record25(Record, Acc).
%% @hidden
pcscf_record25(#{instanceId := InstanceId} = Record, Acc) ->
	Acc1 = Acc#{<<"instanceId">> => InstanceId},
	pcscf_record26(Record, Acc1);
pcscf_record25(Record, Acc) ->
	pcscf_record26(Record, Acc).
%% @hidden
pcscf_record26(#{mSTimeZone := MSTimeZone} = Record, Acc) ->
	Acc1 = Acc#{<<"mSTimeZone">> => MSTimeZone},
	pcscf_record27(Record, Acc1);
pcscf_record26(Record, Acc) ->
	pcscf_record27(Record, Acc).
%% @hidden
pcscf_record27(#{routeHeaderReceived := RouteHeader} = Record, Acc) ->
	Acc1 = Acc#{<<"routeHeaderReceived">> => RouteHeader},
	pcscf_record28(Record, Acc1);
pcscf_record27(Record, Acc) ->
	pcscf_record28(Record, Acc).
%% @hidden
pcscf_record28(#{'transit-IOI-Lists' := IOILists} = Record, Acc) ->
	Acc1 = Acc#{<<"transit-IOI-Lists">> => IOILists},
	pcscf_record29(Record, Acc1);
pcscf_record28(Record, Acc) ->
	pcscf_record29(Record, Acc).
%% @hidden
pcscf_record29(#{routeHeaderTransmitted := RouteTransmitted} = Record, Acc) ->
	Acc1 = Acc#{<<"routeHeaderTransmitted">> => RouteTransmitted},
	pcscf_record30(Record, Acc1);
pcscf_record29(Record, Acc) ->
	pcscf_record30(Record, Acc).
%% @hidden
pcscf_record30(#{gGSNaddress := GGSNAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"gGSNaddress">> => cgf_lib:ip_address(GGSNAddress)},
	pcscf_record31(Record, Acc1);
pcscf_record30(Record, Acc) ->
	pcscf_record31(Record, Acc).
%% @hidden
	pcscf_record31(#{'list-Of-SDP-Media-Components' := List} = Record, Acc) ->
	Acc1 = Acc#{<<"list-Of-SDP-Media-Components">> => media_components(List)},
pcscf_record32(Record, Acc1);
pcscf_record31(Record, Acc) ->
	pcscf_record32(Record, Acc).
%% @hidden
pcscf_record32(#{sessionPriority := SessionPriority} = Record, Acc) ->
	Acc1 = Acc#{<<"sessionPriority">> => SessionPriority},
	pcscf_record33(Record, Acc1);
pcscf_record32(Record, Acc) ->
	pcscf_record33(Record, Acc).
%% @hidden
pcscf_record33(#{serviceContextID := ServiceContextId} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceContextID">> => ServiceContextId},
	pcscf_record34(Record, Acc1);
pcscf_record33(Record, Acc) ->
	pcscf_record34(Record, Acc).
%% @hidden
pcscf_record34(#{subscriberEquipmentNumber := EquipmentNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"subscriberEquipmentNumber">> => EquipmentNumber},
	pcscf_record35(Record, Acc1);
pcscf_record34(Record, Acc) ->
	pcscf_record35(Record, Acc).
%% @hidden
pcscf_record35(#{nodeAddress := NodeAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"nodeAddress">> => NodeAddress},
	pcscf_record36(Record, Acc1);
pcscf_record35(Record, Acc) ->
	pcscf_record36(Record, Acc).
%% @hidden
pcscf_record36(#{fromAddress := FromAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"fromAddress">> => cgf_lib:bcd_dn(FromAddress)},
	pcscf_record37(Record, Acc1);
pcscf_record36(Record, Acc) ->
	pcscf_record37(Record, Acc).
%% @hidden
pcscf_record37(#{'list-of-subscription-ID' := SubscriptionId} = Record, Acc) ->
	Acc1 = Acc#{<<"list-of-subscription-ID">> => SubscriptionId},
	pcscf_record38(Record, Acc1);
pcscf_record37(Record, Acc) ->
	pcscf_record38(Record, Acc).
%% @hidden
pcscf_record38(#{'nNI-Information' := NNIInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"nNI-Information">> => nni_information(NNIInformation)},
	pcscf_record39(Record, Acc1);
pcscf_record38(Record, Acc) ->
	pcscf_record39(Record, Acc).
%% @hidden
pcscf_record39(#{event := Event} = Record, Acc) ->
	Acc1 = Acc#{<<"event">> => Event},
	pcscf_record40(Record, Acc1);
pcscf_record39(Record, Acc) ->
	pcscf_record40(Record, Acc).
%% @hidden
pcscf_record40(#{'list-Of-Calling-Party-Address' := List} = Record, Acc) ->
	PartyList = [party_address(Party) || Party <- List],
	Acc1 = Acc#{<<"listOfCallingPartyAddress">> => PartyList},
	pcscf_record41(Record, Acc1);
pcscf_record40(Record, Acc) ->
	pcscf_record41(Record, Acc).
%% @hidden
pcscf_record41(#{'list-Of-Early-SDP-Media-Components' := List} = Record, Acc) ->
	Components = [media_components(Media) || Media <- List],
	Acc1 = Acc#{<<"listOfEarlySDPMediaComponents">> => Components},
	pcscf_record42(Record, Acc1);
pcscf_record41(Record, Acc) ->
	pcscf_record42(Record, Acc).
%% @hidden
pcscf_record42(#{iMSEmergencyIndicator := EmergencyIndicator} = Record, Acc) ->
	Acc1 = Acc#{<<"iMSEmergencyIndicator">> => EmergencyIndicator},
	pcscf_record43(Record, Acc1);
pcscf_record42(Record, Acc) ->
	pcscf_record43(Record, Acc).
%% @hidden
pcscf_record43(#{'role-of-Node' := NodeRole} = Record, Acc) ->
	Acc1 = Acc#{<<"role-of-Node">> => NodeRole},
	pcscf_record44(Record, Acc1);
pcscf_record43(Record, Acc) ->
	pcscf_record44(Record, Acc).
%% @hidden
pcscf_record44(#{numberPortabilityRouting := PortabilityRouting} = Record, Acc) ->
	Acc1 = Acc#{<<"numberPortabilityRouting">> => PortabilityRouting},
	pcscf_record45(Record, Acc1);
pcscf_record44(Record, Acc) ->
	pcscf_record45(Record, Acc).
%% @hidden
pcscf_record45(#{privateUserID := PrivateUserId} = Record, Acc) ->
	Acc1 = Acc#{<<"privateUserID">> => PrivateUserId},
	pcscf_record46(Record, Acc1);
pcscf_record45(Record, Acc) ->
	pcscf_record46(Record, Acc).
%% @hidden
pcscf_record46(#{relatedICID := RelatedICID} = Record, Acc) ->
	Acc1 = Acc#{<<"relatedICID">> => RelatedICID},
	pcscf_record47(Record, Acc1);
pcscf_record46(Record, Acc) ->
	pcscf_record47(Record, Acc).
%% @hidden
pcscf_record47(#{'sIP-Method' := SIPMethod} = Record, Acc) ->
	Acc1 = Acc#{<<"sIP-Method">> => SIPMethod},
	pcscf_record48(Record, Acc1);
pcscf_record47(Record, Acc) ->
	pcscf_record48(Record, Acc).
%% @hidden
pcscf_record48(#{servedPartyIPAddress := PartyIPAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"servedPartyIPAddress">> =>
			cgf_lib:octet_ip_address(PartyIPAddress)},
	pcscf_record49(Record, Acc1);
pcscf_record48(Record, Acc) ->
	pcscf_record49(Record, Acc).
%% @hidden
pcscf_record49(#{fEIdentifierList := FEList} = Record, Acc) ->
	Acc1 = Acc#{<<"fEIdentifierList">> => FEList},
	pcscf_record50(Record, Acc1);
pcscf_record49(Record, Acc) ->
	pcscf_record50(Record, Acc).
%% @hidden
pcscf_record50(#{'iMS-Charging-Identifier' := ChargingId} = Record, Acc) ->
	Acc1 = Acc#{<<"iMS-Charging-Identifier">> => ChargingId},
	pcscf_record51(Record, Acc1);
pcscf_record50(Record, Acc) ->
	pcscf_record51(Record, Acc).
%% @hidden
pcscf_record51(#{serviceDeliveryStartTimeStampFraction := StartTimeFraction} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryStartTimeStampFraction">> =>
			cgf_lib:bcd_date_time(StartTimeFraction)},
	pcscf_record52(Record, Acc1);
pcscf_record51(Record, Acc) ->
	pcscf_record52(Record, Acc).
%% @hidden
pcscf_record52(#{localRecordSequenceNumber := SeqNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"localRecordSequenceNumber">> => SeqNumber},
	pcscf_record53(Record, Acc1);
pcscf_record52(Record, Acc) ->
	pcscf_record53(Record, Acc).
%% @hidden
pcscf_record53(#{'called-Party-Address' := CalledPartyAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"called-Party-Address">> => cgf_lib:octet_string(CalledPartyAddress)},
	pcscf_record54(Record, Acc1);
pcscf_record53(Record, Acc) ->
	pcscf_record54(Record, Acc).
%% @hidden
pcscf_record54(#{retransmission := Retransmission} = Record, Acc) ->
	Acc1 = Acc#{<<"retransmission">> => Retransmission},
	pcscf_record55(Record, Acc1);
pcscf_record54(Record, Acc) ->
	pcscf_record55(Record, Acc).
%% @hidden
pcscf_record55(#{additionalAccessNetworkInformation := AdditionalNetworkInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"additionalAccessNetworkInformation">> =>
			 cgf_lib:octet_string(AdditionalNetworkInfo)},
	pcscf_record56(Record, Acc1);
pcscf_record55(Record, Acc) ->
	pcscf_record56(Record, Acc).
%% @hidden
pcscf_record56(#{<<"recordClosureTime">> := ClosureTime} = Record, Acc) ->
	Acc1 = Acc#{<<"recordClosureTime">> => cgf_lib:bcd_date_time(ClosureTime)},
	pcscf_record57(Record, Acc1);
pcscf_record56(Record, Acc) ->
	pcscf_record57(Record, Acc).
%% @hidden
pcscf_record57(#{'session-Id' := SessionId} = Record, Acc) ->
	Acc1 = Acc#{<<"session-Id">> => SessionId},
	pcscf_record58(Record, Acc1);
pcscf_record57(Record, Acc) ->
	pcscf_record58(Record, Acc).
%% @hidden
pcscf_record58(#{serviceDeliveryStartTimeStamp := StartTimeStamp} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceDeliveryStartTimeStamp">> =>
			cgf_lib:bcd_date_time(StartTimeStamp)},
	pcscf_record59(Record, Acc1);
pcscf_record58(Record, Acc) ->
	pcscf_record59(Record, Acc).
%% @hidden
pcscf_record59(#{interOperatorIdentifiers := OperatorIds} = Record, Acc) ->
	Acc1 = Acc#{<<"interOperatorIdentifiers">> => OperatorIds},
	pcscf_record60(Record, Acc1);
pcscf_record59(Record, Acc) ->
	pcscf_record60(Record, Acc).
%% @hidden
pcscf_record60(#{'incomplete-CDR-Indication' := IncompleteCDR} = Record, Acc) ->
	Acc1 = Acc#{<<"incomplete-CDR-Indication">> => IncompleteCDR},
	pcscf_record61(Record, Acc1);
pcscf_record60(Record, Acc) ->
	pcscf_record61(Record, Acc).
%% @hidden
pcscf_record61(#{accessNetworkInformation := AccessNetworkInfo} = _Record, Acc) ->
	Acc#{<<"accessNetworkInformation">> =>
			cgf_lib:octet_string(AccessNetworkInfo)};
pcscf_record61(_Record, Acc) ->
	Acc.

%% @hidden
access_info(_List) ->
	{error, not_implemented}.

%% @hidden
message_bodies(_MessageBodies) ->
	{error, not_implemented}.

%% @hidden
party_list(_List) ->
	{error, not_implemented}.

%% @hidden
nni_information(_NNIInformation) ->
	{error, not_implemented}.

