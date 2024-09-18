%%% cgf_cs.erl
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
-module(cgf_cs).
-copyright('Copyright (c) 2024 SigScale Global Inc.').

-export([import/2, import/3, parse/3]).

-include_lib("kernel/include/logger.hrl").

%%----------------------------------------------------------------------
%%  The cgf_cs public API
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
			'CSChargingDataTypes':decode('CSRecord', Bin)).

%% @hidden
import2(Log, Metadata, {ok, CDR, Rest}) ->
	case parse(Log, Metadata, CDR) of
		ok when byte_size(Rest) == 0 ->
			ok;
		_ ->
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
		RecordType :: moCallRecord | mtCallRecord | moSMSRecord
				| mtSMSGWRecord,
		Record :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR.
%% @private
parse(Log, Metadata, {moCallRecord, MOCallRecord} = _CDR) ->
	case parse_mo_call(Log, Metadata, MOCallRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mo_call},
					{error, Reason}])
	end;
parse(Log, Metadata, {mtCallRecord, MTCallRecord}) ->
	case parse_mt_call(Log, Metadata, MTCallRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_call},
					{error, Reason}])
	end;
parse(Log, Metadata, {moSMSRecord, MOSMSRecord}) ->
	case parse_mo_sms(Log, Metadata, MOSMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mo_sms},
					{error, Reason}])
	end;
parse(Log, Metadata, {mtSMSGWRecord, MTSMSGWRecord}) ->
	case parse_mt_smsgw(Log, Metadata, MTSMSGWRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_smsgw},
					{error, Reason}])
	end.

-spec parse_mo_call(Log, Metadata, MOCallRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOCallRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MO Call Record.
parse_mo_call(Log, Metadata, MOCallRecord) ->
	Call = mo_call_record(MOCallRecord),
	CDR = [{moCall, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_call(Log, Metadata, MTCallRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTCallRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MT Call Record.
parse_mt_call(_Log, _Metadata, _MTCallRecord) ->
	{error, not_implemented}.

-spec parse_mo_sms(Log, Metadata, MOSMSRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MOSMSRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MO SMS Record.
parse_mo_sms(Log, Metadata, MOSMSRecord) ->
	Call = mo_sms_record(MOSMSRecord),
	CDR = [{mo_sms, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_smsgw(Log, Metadata, MTSMSGWRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTSMSGWRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MT SMSGW Record.
parse_mt_smsgw(_Log, _Metadata, _MTSMSGWRecord) ->
	{error, not_implemented}.

%% @hidden
mo_call_record(#{servedIMSI := ServedIMSI} = MOCallRecord) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:bcd_dn(ServedIMSI)},
	mo_call_record1(MOCallRecord, Acc);
mo_call_record(MOCallRecord) ->
	mo_call_record1(MOCallRecord, #{}).
%% @hidden
mo_call_record1(#{servedIMEI := ServedIMEI} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:bcd_dn(ServedIMEI)},
	mo_call_record2(MOCallRecord, Acc1);
mo_call_record1(MOCallRecord, Acc) ->
	mo_call_record2(MOCallRecord, Acc).
%% @hidden
mo_call_record2(#{servedMSISDN := ServedMSISDN} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"servedMSISDN">> => cgf_lib:bcd_dn(ServedMSISDN)},
	mo_call_record3(MOCallRecord, Acc1);
mo_call_record2(MOCallRecord, Acc) ->
	mo_call_record3(MOCallRecord, Acc).
%% @hidden
mo_call_record3(#{callingNumber := CallingNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"callingNumber">> => cgf_lib:bcd_dn(CallingNumber)},
	mo_call_record4(MOCallRecord, Acc1);
mo_call_record3(MOCallRecord, Acc) ->
	mo_call_record4(MOCallRecord, Acc).
%% @hidden
mo_call_record4(#{calledNumber := CalledNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"calledNumber">> => cgf_lib:bcd_dn(CalledNumber)},
	mo_call_record5(MOCallRecord, Acc1);
mo_call_record4(MOCallRecord, Acc) ->
	mo_call_record5(MOCallRecord, Acc).
%% @hidden
mo_call_record5(#{translatedNumber := TranslatedNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"translatedNumber">> => cgf_lib:bcd_dn(TranslatedNumber)},
	mo_call_record6(MOCallRecord, Acc1);
mo_call_record5(MOCallRecord, Acc) ->
	mo_call_record6(MOCallRecord, Acc).
%% @hidden
mo_call_record6(#{connectedNumber := ConnectedNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"connectedNumber">> => cgf_lib:bcd_dn(ConnectedNumber)},
	mo_call_record7(MOCallRecord, Acc1);
mo_call_record6(MOCallRecord, Acc) ->
	mo_call_record7(MOCallRecord, Acc).
%% @hidden
mo_call_record7(#{roamingNumber := RoamingNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"roamingNumber">> => cgf_lib:bcd_dn(RoamingNumber)},
	mo_call_record8(MOCallRecord, Acc1);
mo_call_record7(MOCallRecord, Acc) ->
	mo_call_record8(MOCallRecord, Acc).
%% @hidden
mo_call_record8(#{recordingEntity := MOCallRecordingEntity} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => MOCallRecordingEntity},
	mo_call_record9(MOCallRecord, Acc1);
mo_call_record8(MOCallRecord, Acc) ->
	mo_call_record9(MOCallRecord, Acc).
%% @hidden
mo_call_record9(#{mscIncomingTKGP := MscIncomingTKGP} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => MscIncomingTKGP},
	mo_call_record10(MOCallRecord, Acc1);
mo_call_record9(MOCallRecord, Acc) ->
mo_call_record10(MOCallRecord, Acc).
%% @hidden
mo_call_record10(#{mscOutgoingTKGP := MscOutgoingTKGP} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => MscOutgoingTKGP},
	mo_call_record11(MOCallRecord, Acc1);
mo_call_record10(MOCallRecord, Acc) ->
	mo_call_record11(MOCallRecord, Acc).
%% @hidden
mo_call_record11(#{location := Location} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"location">> => cgf_lib:octet_string(Location)},
	mo_call_record12(MOCallRecord, Acc1);
mo_call_record11(MOCallRecord, Acc) ->
	mo_call_record12(MOCallRecord, Acc).
%% @hidden
mo_call_record12(#{changeOfLocation := ChangeOfLocation} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfLocation">> => cgf_lib:octet_string(ChangeOfLocation)},
	mo_call_record13(MOCallRecord, Acc1);
mo_call_record12(MOCallRecord, Acc) ->
	mo_call_record13(MOCallRecord, Acc).
%% @hidden
mo_call_record13(#{basicService := BasicService} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"basicService">> => BasicService},
	mo_call_record14(MOCallRecord, Acc1);
mo_call_record13(MOCallRecord, Acc) ->
	mo_call_record14(MOCallRecord, Acc).
%% @hidden
mo_call_record14(#{transparencyIndicator := TransparencyIndicator} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"transparencyIndicator">> => TransparencyIndicator},
	mo_call_record15(MOCallRecord, Acc1);
mo_call_record14(MOCallRecord, Acc) ->
	mo_call_record15(MOCallRecord, Acc).
%% @hidden
mo_call_record15(#{changeOfService := ChangeOfService} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfService">> => change_of_service(ChangeOfService)},
	mo_call_record16(MOCallRecord, Acc1);
mo_call_record15(MOCallRecord, Acc) ->
	mo_call_record16(MOCallRecord, Acc).
%% @hidden
mo_call_record16(#{supplServicesUsed := SupplServicesUsed} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"supplServicesUsed">> => supply_service_used(SupplServicesUsed)},
	mo_call_record17(MOCallRecord, Acc1);
mo_call_record16(MOCallRecord, Acc) ->
	mo_call_record17(MOCallRecord, Acc).
%% @hidden
mo_call_record17(#{aocParameters := AOCParameters} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"aocParameters">> => AOCParameters},
	mo_call_record18(MOCallRecord, Acc1);
mo_call_record17(MOCallRecord, Acc) ->
	mo_call_record18(MOCallRecord, Acc).
%% @hidden
mo_call_record18(#{changeOfAOCParms := ChangeOfAOCParms} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfAOCParms">> => ChangeOfAOCParms},
	mo_call_record19(MOCallRecord, Acc1);
mo_call_record18(MOCallRecord, Acc) ->
	mo_call_record19(MOCallRecord, Acc).
%% @hidden
mo_call_record19(#{msClassmark := MSClassmark} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"msClassmark">> => MSClassmark},
	mo_call_record20(MOCallRecord, Acc1);
mo_call_record19(MOCallRecord, Acc) ->
	mo_call_record20(MOCallRecord, Acc).
%% @hidden
mo_call_record20(#{changeOfClassmark := ChangeOfClassmark} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfClassmark">> => ChangeOfClassmark},
	mo_call_record21(MOCallRecord, Acc1);
mo_call_record20(MOCallRecord, Acc) ->
	mo_call_record21(MOCallRecord, Acc).
%% @hidden
mo_call_record21(#{seizureTime := SeizureTime} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:octet_string(SeizureTime)},
	mo_call_record22(MOCallRecord, Acc1);
mo_call_record21(MOCallRecord, Acc) ->
	mo_call_record22(MOCallRecord, Acc).
%% @hidden
mo_call_record22(#{answerTime := AnswerTime} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:octet_string(AnswerTime)},
	mo_call_record23(MOCallRecord, Acc1);
mo_call_record22(MOCallRecord, Acc) ->
	mo_call_record23(MOCallRecord, Acc).
%% @hidden
mo_call_record23(#{releaseTime := ReleaseTime} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:octet_string(ReleaseTime)},
	mo_call_record24(MOCallRecord, Acc1);
mo_call_record23(MOCallRecord, Acc) ->
	mo_call_record24(MOCallRecord, Acc).
%% @hidden
mo_call_record24(#{callDuration := CallDuration} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => cgf_lib:octet_string(CallDuration)},
	mo_call_record25(MOCallRecord, Acc1);
mo_call_record24(MOCallRecord, Acc) ->
	mo_call_record25(MOCallRecord, Acc).
%% @hidden
mo_call_record25(#{dataVolume := DataVolume} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => DataVolume},
	mo_call_record26(MOCallRecord, Acc1);
mo_call_record25(MOCallRecord, Acc) ->
	mo_call_record26(MOCallRecord, Acc).
%% @hidden
mo_call_record26(#{radioChanRequested := RadioChanRequested} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"radioChanRequested">> => RadioChanRequested},
	mo_call_record27(MOCallRecord, Acc1);
mo_call_record26(MOCallRecord, Acc) ->
	mo_call_record27(MOCallRecord, Acc).
%% @hidden
mo_call_record27(#{radioChanUsed := RadioChanUsed} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"radioChanUsed">> => RadioChanUsed},
	mo_call_record28(MOCallRecord, Acc1);
mo_call_record27(MOCallRecord, Acc) ->
	mo_call_record28(MOCallRecord, Acc).
%% @hidden
mo_call_record28(#{changeOfRadioChan := ChangeOfRadioChan} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfRadioChan">> => ChangeOfRadioChan},
	mo_call_record29(MOCallRecord, Acc1);
mo_call_record28(MOCallRecord, Acc) ->
	mo_call_record29(MOCallRecord, Acc).
%% @hidden
mo_call_record29(#{causeForTerm := CauseForTerm} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => CauseForTerm},
	mo_call_record30(MOCallRecord, Acc1);
mo_call_record29(MOCallRecord, Acc) ->
	mo_call_record30(MOCallRecord, Acc).
%% @hidden
mo_call_record30(#{diagnostics := Diagnostics} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => Diagnostics},
	mo_call_record31(MOCallRecord, Acc1);
mo_call_record30(MOCallRecord, Acc) ->
	mo_call_record31(MOCallRecord, Acc).
%% @hidden
mo_call_record31(#{callReference := CallReference} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"callReference">> => CallReference},
	mo_call_record32(MOCallRecord, Acc1);
mo_call_record31(MOCallRecord, Acc) ->
	mo_call_record32(MOCallRecord, Acc).
%% @hidden
mo_call_record32(#{sequenceNumber := SequenceNumber} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => SequenceNumber},
	mo_call_record33(MOCallRecord, Acc1);
mo_call_record32(MOCallRecord, Acc) ->
	mo_call_record33(MOCallRecord, Acc).
%% @hidden
mo_call_record33(#{additionalChgInfo := AdditionalChgInfo} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"additionalChgInfo">> => AdditionalChgInfo},
	mo_call_record34(MOCallRecord, Acc1);
mo_call_record33(MOCallRecord, Acc) ->
	mo_call_record34(MOCallRecord, Acc).
%% @hidden
mo_call_record34(#{recordExtensions := MOCallRecordExtensions} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => MOCallRecordExtensions},
	mo_call_record35(MOCallRecord, Acc1);
mo_call_record34(MOCallRecord, Acc) ->
	mo_call_record35(MOCallRecord, Acc).
%% @hidden
mo_call_record35(#{'gsm-SCFAddress' := GsmSCFAddress} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"gsm-SCFAddress">> => cgf_lib:bcd_dn(GsmSCFAddress)},
	mo_call_record36(MOCallRecord, Acc1);
mo_call_record35(MOCallRecord, Acc) ->
	mo_call_record36(MOCallRecord, Acc).
%% @hidden
mo_call_record36(#{serviceKey := ServiceKey} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceKey">> => ServiceKey},
	mo_call_record37(MOCallRecord, Acc1);
mo_call_record36(MOCallRecord, Acc) ->
	mo_call_record37(MOCallRecord, Acc).
%% @hidden
mo_call_record37(#{networkCallReference := NetworkCallReference} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"networkCallReference">> => NetworkCallReference},
	mo_call_record38(MOCallRecord, Acc1);
mo_call_record37(MOCallRecord, Acc) ->
	mo_call_record38(MOCallRecord, Acc).
%% @hidden
mo_call_record38(#{mSCAddress := MSCAddress} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mSCAddress">> => cgf_lib:bcd_dn(MSCAddress)},
	mo_call_record39(MOCallRecord, Acc1);
mo_call_record38(MOCallRecord, Acc) ->
	mo_call_record39(MOCallRecord, Acc).
%% @hidden
mo_call_record39(#{cAMELInitCFIndicator := CAMELInitCFIndicator} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"cAMELInitCFIndicator">> => CAMELInitCFIndicator},
	mo_call_record40(MOCallRecord, Acc1);
mo_call_record39(MOCallRecord, Acc) ->
	mo_call_record40(MOCallRecord, Acc).
%% @hidden
mo_call_record40(#{defaultCallHandling := DefaultCallHandling} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"defaultCallHandling">> => DefaultCallHandling},
	mo_call_record41(MOCallRecord, Acc1);
mo_call_record40(MOCallRecord, Acc) ->
	mo_call_record41(MOCallRecord, Acc).
%% @hidden
mo_call_record41(#{hSCSDChanRequested := HSCSDChanRequested} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanRequested">> => HSCSDChanRequested},
	mo_call_record42(MOCallRecord, Acc1);
mo_call_record41(MOCallRecord, Acc) ->
	mo_call_record42(MOCallRecord, Acc).
%% @hidden
mo_call_record42(#{hSCSDChanAllocated := HSCSDChanAllocated} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanAllocated">> => HSCSDChanAllocated},
	mo_call_record43(MOCallRecord, Acc1);
mo_call_record42(MOCallRecord, Acc) ->
	mo_call_record43(MOCallRecord, Acc).
%% @hidden
mo_call_record43(#{changeOfHSCSDParms := ChangeOfHSCSDParms} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfHSCSDParms">> => ChangeOfHSCSDParms},
	mo_call_record44(MOCallRecord, Acc1);
mo_call_record43(MOCallRecord, Acc) ->
	mo_call_record44(MOCallRecord, Acc).
%% @hidden
mo_call_record44(#{fnur := FNUR} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"fnur">> => FNUR},
	mo_call_record45(MOCallRecord, Acc1);
mo_call_record44(MOCallRecord, Acc) ->
	mo_call_record45(MOCallRecord, Acc).
%% @hidden
mo_call_record45(#{aiurRequested := AIURRequested} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"aiurRequested">> => AIURRequested},
	mo_call_record46(MOCallRecord, Acc1);
mo_call_record45(MOCallRecord, Acc) ->
	mo_call_record46(MOCallRecord, Acc).
%% @hidden
mo_call_record46(#{chanCodingsAcceptable := ChanCodingsAcceptable} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"chanCodingsAcceptable">> => ChanCodingsAcceptable},
	mo_call_record47(MOCallRecord, Acc1);
mo_call_record46(MOCallRecord, Acc) ->
	mo_call_record47(MOCallRecord, Acc).
%% @hidden
mo_call_record47(#{chanCodingUsed := ChanCodingUsed} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"chanCodingUsed">> => ChanCodingUsed},
	mo_call_record48(MOCallRecord, Acc1);
mo_call_record47(MOCallRecord, Acc) ->
	mo_call_record48(MOCallRecord, Acc).
%% @hidden
mo_call_record48(#{speechVersionSupported := SpeechVersionSupported} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"speechVersionSupported">> => SpeechVersionSupported},
	mo_call_record49(MOCallRecord, Acc1);
mo_call_record48(MOCallRecord, Acc) ->
	mo_call_record49(MOCallRecord, Acc).
%% @hidden
mo_call_record49(#{speechVersionUsed := SpeechVersionUsed} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"speechVersionUsed">> => SpeechVersionUsed},
	mo_call_record50(MOCallRecord, Acc1);
mo_call_record49(MOCallRecord, Acc) ->
	mo_call_record50(MOCallRecord, Acc).
%% @hidden
mo_call_record50(#{numberOfDPEncountered := NumberOfDPEncountered} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"numberOfDPEncountered">> => NumberOfDPEncountered},
	mo_call_record51(MOCallRecord, Acc1);
mo_call_record50(MOCallRecord, Acc) ->
	mo_call_record51(MOCallRecord, Acc).
%% @hidden
mo_call_record51(#{levelOfCAMELService := LevelOfCAMELService} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"levelOfCAMELService">> => LevelOfCAMELService},
	mo_call_record52(MOCallRecord, Acc1);
mo_call_record51(MOCallRecord, Acc) ->
	mo_call_record52(MOCallRecord, Acc).
%% @hidden
mo_call_record52(#{freeFormatData := FreeFormatData} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatData">> => FreeFormatData},
	mo_call_record53(MOCallRecord, Acc1);
mo_call_record52(MOCallRecord, Acc) ->
	mo_call_record53(MOCallRecord, Acc).
%% @hidden
mo_call_record53(#{cAMELCallLegInformation := CAMELCallLegInformation} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"cAMELCallLegInformation">> => CAMELCallLegInformation},
	mo_call_record54(MOCallRecord, Acc1);
mo_call_record53(MOCallRecord, Acc) ->
	mo_call_record54(MOCallRecord, Acc).
%% @hidden
mo_call_record54(#{freeFormatDataAppend := FreeFormatDataAppend} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatDataAppend">> => FreeFormatDataAppend},
	mo_call_record55(MOCallRecord, Acc1);
mo_call_record54(MOCallRecord, Acc) ->
	mo_call_record55(MOCallRecord, Acc).
%% @hidden
mo_call_record55(#{defaultCallHandling2 := DefaultCallHandling2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"defaultCallHandling-2">> => DefaultCallHandling2},
	mo_call_record56(MOCallRecord, Acc1);
mo_call_record55(MOCallRecord, Acc) ->
	mo_call_record56(MOCallRecord, Acc).
%% @hidden
mo_call_record56(#{'gsm-SCFAddress2' := GsmSCFAddress2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"gsm-SCFAddressa-2">> => GsmSCFAddress2},
	mo_call_record57(MOCallRecord, Acc1);
mo_call_record56(MOCallRecord, Acc) ->
	mo_call_record57(MOCallRecord, Acc).
%% @hidden
mo_call_record57(#{serviceKey2 := ServiceKey2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceKey-2">> => ServiceKey2},
	mo_call_record58(MOCallRecord, Acc1);
mo_call_record57(MOCallRecord, Acc) ->
	mo_call_record58(MOCallRecord, Acc).
%% @hidden
mo_call_record58(#{freeFormatData2 := FreeFormatData2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatData-2">> => FreeFormatData2},
	mo_call_record59(MOCallRecord, Acc1);
mo_call_record58(MOCallRecord, Acc) ->
	mo_call_record59(MOCallRecord, Acc).
%% @hidden
mo_call_record59(#{freeFormatDataAppend2 := FreeFormatDataAppend2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatDataAppend-2">> => FreeFormatDataAppend2},
	mo_call_record60(MOCallRecord, Acc1);
mo_call_record59(MOCallRecord, Acc) ->
	mo_call_record60(MOCallRecord, Acc).
%% @hidden
mo_call_record60(#{systemType := SystemType} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"systemType">> => SystemType},
	mo_call_record61(MOCallRecord, Acc1);
mo_call_record60(MOCallRecord, Acc) ->
	mo_call_record61(MOCallRecord, Acc).
%% @hidden
mo_call_record61(#{rateIndication := RateIndication} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"rateIndication">> => RateIndication},
	mo_call_record62(MOCallRecord, Acc1);
mo_call_record61(MOCallRecord, Acc) ->
	mo_call_record62(MOCallRecord, Acc).
%% @hidden
mo_call_record62(#{locationRoutNum := LocationRoutNum} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => LocationRoutNum},
	mo_call_record63(MOCallRecord, Acc1);
mo_call_record62(MOCallRecord, Acc) ->
	mo_call_record63(MOCallRecord, Acc).
%% @hidden
mo_call_record63(#{lrnSoInd := LrnSoInd} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => LrnSoInd},
	mo_call_record64(MOCallRecord, Acc1);
mo_call_record63(MOCallRecord, Acc) ->
	mo_call_record64(MOCallRecord, Acc).
%% @hidden
mo_call_record64(#{lrnQuryStatus := LrnQuryStatus} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => LrnQuryStatus},
	mo_call_record65(MOCallRecord, Acc1);
mo_call_record64(MOCallRecord, Acc) ->
	mo_call_record65(MOCallRecord, Acc).
%% @hidden
mo_call_record65(#{jIPPara := JIPPara} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => JIPPara},
	mo_call_record66(MOCallRecord, Acc1);
mo_call_record65(MOCallRecord, Acc) ->
	mo_call_record66(MOCallRecord, Acc).
%% @hidden
mo_call_record66(#{jIPSoInd := JIPSoInd} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => JIPSoInd},
	mo_call_record67(MOCallRecord, Acc1);
mo_call_record66(MOCallRecord, Acc) ->
	mo_call_record67(MOCallRecord, Acc).
%% @hidden
mo_call_record67(#{jIPQuryStatus := JIPQuryStatus} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => JIPQuryStatus},
	mo_call_record68(MOCallRecord, Acc1);
mo_call_record67(MOCallRecord, Acc) ->
	mo_call_record68(MOCallRecord, Acc).
%% @hidden
mo_call_record68(#{partialMOCallRecordType := PartialMOCallRecordType} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"partialMOCallRecordType">> => PartialMOCallRecordType},
	mo_call_record69(MOCallRecord, Acc1);
mo_call_record68(MOCallRecord, Acc) ->
	mo_call_record69(MOCallRecord, Acc).
%% @hidden
mo_call_record69(#{guaranteedBitRate := GuaranteedBitRate} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"guaranteedBitRate">> => GuaranteedBitRate},
	mo_call_record70(MOCallRecord, Acc1);
mo_call_record69(MOCallRecord, Acc) ->
	mo_call_record70(MOCallRecord, Acc).
%% @hidden
mo_call_record70(#{maximumBitRate := MaximumBitRate} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"maximumBitRate">> => MaximumBitRate},
	mo_call_record71(MOCallRecord, Acc1);
mo_call_record70(MOCallRecord, Acc) ->
	mo_call_record71(MOCallRecord, Acc).
%% @hidden
mo_call_record71(#{redial := Redial} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"redial">> => Redial},
	mo_call_record72(MOCallRecord, Acc1);
mo_call_record71(MOCallRecord, Acc) ->
	mo_call_record72(MOCallRecord, Acc).
%% @hidden
mo_call_record72(#{reasonForServiceChange := ReasonForServiceChange} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"reasonForServiceChange">> => ReasonForServiceChange},
	mo_call_record73(MOCallRecord, Acc1);
mo_call_record72(MOCallRecord, Acc) ->
	mo_call_record73(MOCallRecord, Acc).
%% @hidden
mo_call_record73(#{serviceChangeInitiator := ServiceChangeInitiator} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceChangeInitiator">> => ServiceChangeInitiator},
	mo_call_record74(MOCallRecord, Acc1);
mo_call_record73(MOCallRecord, Acc) ->
	mo_call_record74(MOCallRecord, Acc).
%% @hidden
mo_call_record74(#{iCSI2ActiveFlag := ICSI2ActiveFlag} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"iCSI2ActiveFlag">> => ICSI2ActiveFlag},
	mo_call_record75(MOCallRecord, Acc1);
mo_call_record74(MOCallRecord, Acc) ->
	mo_call_record75(MOCallRecord, Acc).
%% @hidden
mo_call_record75(#{'iMS-Charging-Identifier' := IMSChargingIdentifier} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"iMS-Charging-Identifier">> => IMSChargingIdentifier},
	mo_call_record76(MOCallRecord, Acc1);
mo_call_record75(MOCallRecord, Acc) ->
	mo_call_record76(MOCallRecord, Acc).
%% @hidden
mo_call_record76(#{privateUserID := PrivateUserID} = _MOCallRecord, Acc) ->
	Acc#{<<"privateUserID">> => PrivateUserID};
mo_call_record76(_MOCallRecord, Acc) ->
	Acc.

change_of_service(_ChangeOfService) ->
	{error, not_implemented}.

supply_service_used(_SupplServicesUsed) ->
	{error, not_implemented}.

%% @hidden
mo_sms_record(#{servedIMSI := ServedIMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:bcd_dn(ServedIMSI)},
	mo_sms_record1(Record, Acc);
mo_sms_record(Record) ->
	mo_sms_record1(Record, #{}).
%% @hidden
mo_sms_record1(#{servedIMEI := ServedIMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:bcd_dn(ServedIMEI)},
	mo_sms_record2(Record, Acc1);
mo_sms_record1(Record, Acc) ->
	mo_sms_record2(Record, Acc).
%% @hidden
mo_sms_record2(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	Acc1 = Acc#{<<"servedMSISDN">> => cgf_lib:bcd_dn(ServedMSISDN)},
	mo_sms_record3(Record, Acc1);
mo_sms_record2(Record, Acc) ->
	mo_sms_record3(Record, Acc).
%% @hidden
mo_sms_record3(#{msClassmark := MSClassmark} = Record, Acc) ->
	Acc1 = Acc#{<<"msClassmark">> => cgf_lib:octet_string(MSClassmark)},
	mo_sms_record4(Record, Acc1);
mo_sms_record3(Record, Acc) ->
	mo_sms_record4(Record, Acc).
%% @hidden
mo_sms_record4(#{serviceCentre := ServiceCentre} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceCentre">> => cgf_lib:bcd_dn(ServiceCentre)},
	mo_sms_record5(Record, Acc1);
mo_sms_record4(Record, Acc) ->
	mo_sms_record5(Record, Acc).
%% @hidden
mo_sms_record5(#{recordingEntity := RecordingEntity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => RecordingEntity},
	mo_sms_record6(Record, Acc1);
mo_sms_record5(Record, Acc) ->
	mo_sms_record6(Record, Acc).
%% @hidden
mo_sms_record6(#{location := Location} = Record, Acc) ->
	Acc1 = Acc#{<<"location">> => cgf_lib:octet_string(Location)},
	mo_sms_record7(Record, Acc1);
mo_sms_record6(Record, Acc) ->
	mo_sms_record7(Record, Acc).
%% @hidden
mo_sms_record7(#{messageReference := MessageReference} = Record, Acc) ->
	Acc1 = Acc#{<<"messageReference">> => MessageReference},
	mo_sms_record8(Record, Acc1);
mo_sms_record7(Record, Acc) ->
	mo_sms_record8(Record, Acc).
%% @hidden
mo_sms_record8(#{originationTime := OriginationTime} = Record, Acc) ->
	Acc1 = Acc#{<<"originationTime">> => cgf_lib:octet_string(OriginationTime)},
	mo_sms_record9(Record, Acc1);
mo_sms_record8(Record, Acc) ->
	mo_sms_record9(Record, Acc).
%% @hidden
mo_sms_record9(#{smsResult := SMSResult} = Record, Acc) ->
	Acc1 = Acc#{<<"smsResult">> => SMSResult},
	mo_sms_record10(Record, Acc1);
mo_sms_record9(Record, Acc) ->
	mo_sms_record10(Record, Acc).
%% @hidden
mo_sms_record10(#{recordExtensions := RecordExtensions} = Record, Acc) ->
	Acc1 = Acc#{<<"recordExtensions">> => RecordExtensions},
	mo_sms_record11(Record, Acc1);
mo_sms_record10(Record, Acc) ->
	mo_sms_record11(Record, Acc).
%% @hidden
mo_sms_record11(#{destinationNumber := DestinationNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"destinationNumber">> => cgf_lib:octet_string(DestinationNumber)},
	mo_sms_record12(Record, Acc1);
mo_sms_record11(Record, Acc) ->
	mo_sms_record12(Record, Acc).
%% @hidden
mo_sms_record12(#{cAMELSMSInformation := CAMELSMSInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"cAMELSMSInformation">> => CAMELSMSInformation},
	mo_sms_record13(Record, Acc1);
mo_sms_record12(Record, Acc) ->
	mo_sms_record13(Record, Acc).
%% @hidden
mo_sms_record13(#{systemType := SystemType} = Record, Acc) ->
	Acc1 = Acc#{<<"systemType">> => SystemType},
	mo_sms_record14(Record, Acc1);
mo_sms_record13(Record, Acc) ->
	mo_sms_record14(Record, Acc).
%% @hidden
mo_sms_record14(#{locationExtension := LocationExtension} = _Record, Acc) ->
	Acc#{<<"locationExtension">> => LocationExtension};
mo_sms_record14(_Record, Acc) ->
	Acc.

