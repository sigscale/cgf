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

%% export the public API
-export([import/2, import/3]).

%% export the private API
-export([parse/3]).

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
%%  The cgf_cs public API
%%----------------------------------------------------------------------

-spec parse(Log, Metadata, CDR) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		CDR :: {RecordType, Record},
		RecordType :: moCallRecord | mtCallRecord
				| moSMSRecord | mtSMSRecord
				| ssActionRecord
				| incGatewayRecord | outGatewayRecord | transitRecord
				| roamingRecord,
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
parse(Log, Metadata, {mtSMSRecord, MTSMSRecord}) ->
	case parse_mt_sms(Log, Metadata, MTSMSRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_mt_sms},
					{error, Reason}])
	end;
parse(Log, Metadata, {ssActionRecord, SSActionRecord}) ->
	case parse_ss_action(Log, Metadata, SSActionRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_ss_action},
					{error, Reason}])
	end;
parse(Log, Metadata, {incGatewayRecord, IncGatewayRecord}) ->
	case parse_inc_gateway(Log, Metadata, IncGatewayRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_inc_gateway},
					{error, Reason}])
	end;
parse(Log, Metadata, {outGatewayRecord, OutGatewayRecord}) ->
	case parse_out_gateway(Log, Metadata, OutGatewayRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_out_gateway},
					{error, Reason}])
	end;
parse(Log, Metadata, {transitRecord, TransitCallRecord}) ->
	case parse_transit(Log, Metadata, TransitCallRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_transit},
					{error, Reason}])
	end;
parse(Log, Metadata, {roamingRecord, RoamingRecord}) ->
	case parse_roaming(Log, Metadata, RoamingRecord) of
		ok ->
			ok;
		{error, Reason} ->
			?LOG_ERROR([{?MODULE, parse_roaming},
					{error, Reason}])
	end.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

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
erlang:display({?MODULE, ?FUNCTION_NAME, Call}),
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
parse_mt_call(Log, Metadata, MTCallRecord) ->
	Call = mt_call_record(MTCallRecord),
	CDR = [{mtCall, Call} | Metadata],
	cgf_log:blog(Log, CDR).

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
	SMS = mo_sms_record(MOSMSRecord),
	CDR = [{moSMS, SMS} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_mt_sms(Log, Metadata, MTSMSGWRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		MTSMSGWRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an MT SMSGW Record.
parse_mt_sms(Log, Metadata, MTSMSRecord) ->
	SMS = mt_sms_record(MTSMSRecord),
	CDR = [{mtSMS, SMS} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_ss_action(Log, Metadata, SSActionRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		SSActionRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an SS Action Record.
parse_ss_action(Log, Metadata, SSActionRecord) ->
	SSA = ss_action_record(SSActionRecord),
	CDR = [{ssAction, SSA} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_inc_gateway(Log, Metadata, IncGatewayRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		IncGatewayRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an Inc Gateway Record.
parse_inc_gateway(Log, Metadata, IncGatewayRecord) ->
	Inc = inc_gateway_record(IncGatewayRecord),
	CDR = [{incGateway, Inc} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_out_gateway(Log, Metadata, OutGatewayRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		OutGatewayRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for an Out Gateway Record.
parse_out_gateway(Log, Metadata, OutGatewayRecord) ->
	Out = out_gateway_record(OutGatewayRecord),
	CDR = [{outGateway, Out} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_transit(Log, Metadata, TransitCallRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		TransitCallRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a Transit Call Record.
parse_transit(Log, Metadata, TransitCallRecord) ->
	Call = transit_call_record(TransitCallRecord),
	CDR = [{transit, Call} | Metadata],
	cgf_log:blog(Log, CDR).

-spec parse_roaming(Log, Metadata, RoamingRecord) -> Result
	when
		Log :: disk_log:log(),
		Metadata :: [{AttributeName, AttributeValue}],
		AttributeName :: string(),
		AttributeValue :: term(),
		RoamingRecord :: map(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Parse a CDR event detail for a Roaming Record.
parse_roaming(Log, Metadata, RoamingRecord) ->
	Roam = roaming_record(RoamingRecord),
	CDR = [{roaming, Roam} | Metadata],
	cgf_log:blog(Log, CDR).

%% @hidden
mo_call_record(#{servedIMSI := ServedIMSI} = MOCallRecord) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	mo_call_record1(MOCallRecord, Acc);
mo_call_record(MOCallRecord) ->
	mo_call_record1(MOCallRecord, #{}).
%% @hidden
mo_call_record1(#{servedIMEI := ServedIMEI} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	mo_call_record2(MOCallRecord, Acc1);
mo_call_record1(MOCallRecord, Acc) ->
	mo_call_record2(MOCallRecord, Acc).
%% @hidden
mo_call_record2(#{servedMSISDN := ServedMSISDN} = MOCallRecord, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
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
mo_call_record8(#{recordingEntity := RecordingEntity} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	mo_call_record9(MOCallRecord, Acc1);
mo_call_record8(MOCallRecord, Acc) ->
	mo_call_record9(MOCallRecord, Acc).
%% @hidden
mo_call_record9(#{mscIncomingTKGP := TrunkGroup} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	mo_call_record10(MOCallRecord, Acc1);
mo_call_record9(MOCallRecord, Acc) ->
mo_call_record10(MOCallRecord, Acc).
%% @hidden
mo_call_record10(#{mscOutgoingTKGP := TrunkGroup} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	mo_call_record11(MOCallRecord, Acc1);
mo_call_record10(MOCallRecord, Acc) ->
	mo_call_record11(MOCallRecord, Acc).
%% @hidden
mo_call_record11(#{location := LocationAreaAndCell} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"location">> => location_area_and_cell(LocationAreaAndCell)},
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
	Acc1 = Acc#{<<"basicService">> => basic_service_code(BasicService)},
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
	SSU = [suppl_service_used(Service) || Service <- SupplServicesUsed],
	Acc1 = Acc#{<<"supplServicesUsed">> => SSU},
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
	Acc1 = Acc#{<<"msClassmark">> => cgf_lib:octet_string(MSClassmark)},
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
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:bcd_date_time(SeizureTime)},
	mo_call_record22(MOCallRecord, Acc1);
mo_call_record21(MOCallRecord, Acc) ->
	mo_call_record22(MOCallRecord, Acc).
%% @hidden
mo_call_record22(#{answerTime := AnswerTime} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:bcd_date_time(AnswerTime)},
	mo_call_record23(MOCallRecord, Acc1);
mo_call_record22(MOCallRecord, Acc) ->
	mo_call_record23(MOCallRecord, Acc).
%% @hidden
mo_call_record23(#{releaseTime := ReleaseTime} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:bcd_date_time(ReleaseTime)},
	mo_call_record24(MOCallRecord, Acc1);
mo_call_record23(MOCallRecord, Acc) ->
	mo_call_record24(MOCallRecord, Acc).
%% @hidden
mo_call_record24(#{callDuration := Duration} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
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
mo_call_record29(#{causeForTerm := Cause} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	mo_call_record30(MOCallRecord, Acc1);
mo_call_record29(MOCallRecord, Acc) ->
	mo_call_record30(MOCallRecord, Acc).
%% @hidden
mo_call_record30(#{diagnostics := Diagnostics} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	mo_call_record31(MOCallRecord, Acc1);
mo_call_record30(MOCallRecord, Acc) ->
	mo_call_record31(MOCallRecord, Acc).
%% @hidden
mo_call_record31(#{callReference := CallReference} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(CallReference)},
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
mo_call_record34(#{'gsm-SCFAddress' := GsmSCFAddress} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"gsm-SCFAddress">> => cgf_lib:bcd_dn(GsmSCFAddress)},
	mo_call_record35(MOCallRecord, Acc1);
mo_call_record34(MOCallRecord, Acc) ->
	mo_call_record35(MOCallRecord, Acc).
%% @hidden
mo_call_record35(#{serviceKey := ServiceKey} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceKey">> => ServiceKey},
	mo_call_record36(MOCallRecord, Acc1);
mo_call_record35(MOCallRecord, Acc) ->
	mo_call_record36(MOCallRecord, Acc).
%% @hidden
mo_call_record36(#{networkCallReference := Reference} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"networkCallReference">> => cgf_lib:octet_string(Reference)},
	mo_call_record38(MOCallRecord, Acc1);
mo_call_record36(MOCallRecord, Acc) ->
	mo_call_record37(MOCallRecord, Acc).
%% @hidden
mo_call_record37(#{mSCAddress := MSCAddress} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"mSCAddress">> => cgf_lib:bcd_dn(MSCAddress)},
	mo_call_record38(MOCallRecord, Acc1);
mo_call_record37(MOCallRecord, Acc) ->
	mo_call_record38(MOCallRecord, Acc).
%% @hidden
mo_call_record38(#{cAMELInitCFIndicator := CAMELInitCFIndicator} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"cAMELInitCFIndicator">> => CAMELInitCFIndicator},
	mo_call_record39(MOCallRecord, Acc1);
mo_call_record38(MOCallRecord, Acc) ->
	mo_call_record39(MOCallRecord, Acc).
%% @hidden
mo_call_record39(#{defaultCallHandling := DefaultCallHandling} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"defaultCallHandling">> => DefaultCallHandling},
	mo_call_record40(MOCallRecord, Acc1);
mo_call_record39(MOCallRecord, Acc) ->
	mo_call_record40(MOCallRecord, Acc).
%% @hidden
mo_call_record40(#{hSCSDChanRequested := HSCSDChanRequested} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanRequested">> => HSCSDChanRequested},
	mo_call_record41(MOCallRecord, Acc1);
mo_call_record40(MOCallRecord, Acc) ->
	mo_call_record41(MOCallRecord, Acc).
%% @hidden
mo_call_record41(#{hSCSDChanAllocated := HSCSDChanAllocated} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanAllocated">> => HSCSDChanAllocated},
	mo_call_record42(MOCallRecord, Acc1);
mo_call_record41(MOCallRecord, Acc) ->
	mo_call_record42(MOCallRecord, Acc).
%% @hidden
mo_call_record42(#{changeOfHSCSDParms := ChangeOfHSCSDParms} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"changeOfHSCSDParms">> => ChangeOfHSCSDParms},
	mo_call_record43(MOCallRecord, Acc1);
mo_call_record42(MOCallRecord, Acc) ->
	mo_call_record43(MOCallRecord, Acc).
%% @hidden
mo_call_record43(#{fnur := FNUR} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"fnur">> => FNUR},
	mo_call_record44(MOCallRecord, Acc1);
mo_call_record43(MOCallRecord, Acc) ->
	mo_call_record44(MOCallRecord, Acc).
%% @hidden
mo_call_record44(#{aiurRequested := AIURRequested} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"aiurRequested">> => AIURRequested},
	mo_call_record45(MOCallRecord, Acc1);
mo_call_record44(MOCallRecord, Acc) ->
	mo_call_record45(MOCallRecord, Acc).
%% @hidden
mo_call_record45(#{chanCodingsAcceptable := ChanCodingsAcceptable} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"chanCodingsAcceptable">> => ChanCodingsAcceptable},
	mo_call_record46(MOCallRecord, Acc1);
mo_call_record45(MOCallRecord, Acc) ->
	mo_call_record46(MOCallRecord, Acc).
%% @hidden
mo_call_record46(#{chanCodingUsed := ChanCodingUsed} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"chanCodingUsed">> => ChanCodingUsed},
	mo_call_record47(MOCallRecord, Acc1);
mo_call_record46(MOCallRecord, Acc) ->
	mo_call_record47(MOCallRecord, Acc).
%% @hidden
mo_call_record47(#{speechVersionSupported := Identifier} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"speechVersionSupported">> => cgf_lib:octet_string(Identifier)},
	mo_call_record48(MOCallRecord, Acc1);
mo_call_record47(MOCallRecord, Acc) ->
	mo_call_record48(MOCallRecord, Acc).
%% @hidden
mo_call_record48(#{speechVersionUsed := Identifier} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"speechVersionUsed">> => cgf_lib:octet_string(Identifier)},
	mo_call_record49(MOCallRecord, Acc1);
mo_call_record48(MOCallRecord, Acc) ->
	mo_call_record49(MOCallRecord, Acc).
%% @hidden
mo_call_record49(#{numberOfDPEncountered := NumberOfDPEncountered} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"numberOfDPEncountered">> => NumberOfDPEncountered},
	mo_call_record50(MOCallRecord, Acc1);
mo_call_record49(MOCallRecord, Acc) ->
	mo_call_record50(MOCallRecord, Acc).
%% @hidden
mo_call_record50(#{levelOfCAMELService := Level} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"levelOfCAMELService">> => level_camel_service(Level)},
	mo_call_record51(MOCallRecord, Acc1);
mo_call_record50(MOCallRecord, Acc) ->
	mo_call_record51(MOCallRecord, Acc).
%% @hidden
mo_call_record51(#{freeFormatData := FreeFormatData} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatData">> => cgf_lib:octet_string(FreeFormatData)},
	mo_call_record52(MOCallRecord, Acc1);
mo_call_record51(MOCallRecord, Acc) ->
	mo_call_record52(MOCallRecord, Acc).
%% @hidden
mo_call_record52(#{cAMELCallLegInformation := CAMELCallLegInformation} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"cAMELCallLegInformation">> => CAMELCallLegInformation},
	mo_call_record53(MOCallRecord, Acc1);
mo_call_record52(MOCallRecord, Acc) ->
	mo_call_record53(MOCallRecord, Acc).
%% @hidden
mo_call_record53(#{freeFormatDataAppend := FreeFormatDataAppend} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatDataAppend">> => FreeFormatDataAppend},
	mo_call_record54(MOCallRecord, Acc1);
mo_call_record53(MOCallRecord, Acc) ->
	mo_call_record54(MOCallRecord, Acc).
%% @hidden
mo_call_record54(#{defaultCallHandling2 := DefaultCallHandling2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"defaultCallHandling-2">> => DefaultCallHandling2},
	mo_call_record55(MOCallRecord, Acc1);
mo_call_record54(MOCallRecord, Acc) ->
	mo_call_record55(MOCallRecord, Acc).
%% @hidden
mo_call_record55(#{'gsm-SCFAddress2' := GsmSCFAddress2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"gsm-SCFAddressa-2">> => GsmSCFAddress2},
	mo_call_record56(MOCallRecord, Acc1);
mo_call_record55(MOCallRecord, Acc) ->
	mo_call_record56(MOCallRecord, Acc).
%% @hidden
mo_call_record56(#{serviceKey2 := ServiceKey2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceKey-2">> => ServiceKey2},
	mo_call_record57(MOCallRecord, Acc1);
mo_call_record56(MOCallRecord, Acc) ->
	mo_call_record57(MOCallRecord, Acc).
%% @hidden
mo_call_record57(#{freeFormatData2 := FreeFormatData2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatData-2">> => cgf_lib:octet_string(FreeFormatData2)},
	mo_call_record58(MOCallRecord, Acc1);
mo_call_record57(MOCallRecord, Acc) ->
	mo_call_record58(MOCallRecord, Acc).
%% @hidden
mo_call_record58(#{freeFormatDataAppend2 := FreeFormatDataAppend2} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"freeFormatDataAppend-2">> => FreeFormatDataAppend2},
	mo_call_record59(MOCallRecord, Acc1);
mo_call_record58(MOCallRecord, Acc) ->
	mo_call_record59(MOCallRecord, Acc).
%% @hidden
mo_call_record59(#{systemType := SystemType} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"systemType">> => atom_to_binary(SystemType)},
	mo_call_record60(MOCallRecord, Acc1);
mo_call_record59(MOCallRecord, Acc) ->
	mo_call_record60(MOCallRecord, Acc).
%% @hidden
mo_call_record60(#{rateIndication := RateIndication} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"rateIndication">> => RateIndication},
	mo_call_record61(MOCallRecord, Acc1);
mo_call_record60(MOCallRecord, Acc) ->
	mo_call_record61(MOCallRecord, Acc).
%% @hidden
mo_call_record61(#{locationRoutNum := LocationRoutNum} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(LocationRoutNum)},
	mo_call_record62(MOCallRecord, Acc1);
mo_call_record61(MOCallRecord, Acc) ->
	mo_call_record62(MOCallRecord, Acc).
%% @hidden
mo_call_record62(#{lrnSoInd := LrnSoInd} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => LrnSoInd},
	mo_call_record63(MOCallRecord, Acc1);
mo_call_record62(MOCallRecord, Acc) ->
	mo_call_record63(MOCallRecord, Acc).
%% @hidden
mo_call_record63(#{lrnQuryStatus := LrnQuryStatus} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => LrnQuryStatus},
	mo_call_record64(MOCallRecord, Acc1);
mo_call_record63(MOCallRecord, Acc) ->
	mo_call_record64(MOCallRecord, Acc).
%% @hidden
mo_call_record64(#{jIPPara := JIPPara} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => JIPPara},
	mo_call_record65(MOCallRecord, Acc1);
mo_call_record64(MOCallRecord, Acc) ->
	mo_call_record65(MOCallRecord, Acc).
%% @hidden
mo_call_record65(#{jIPSoInd := JIPSoInd} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => JIPSoInd},
	mo_call_record66(MOCallRecord, Acc1);
mo_call_record65(MOCallRecord, Acc) ->
	mo_call_record66(MOCallRecord, Acc).
%% @hidden
mo_call_record66(#{jIPQuryStatus := JIPQuryStatus} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => JIPQuryStatus},
	mo_call_record67(MOCallRecord, Acc1);
mo_call_record66(MOCallRecord, Acc) ->
	mo_call_record67(MOCallRecord, Acc).
%% @hidden
mo_call_record67(#{partialRecordType := PartialRecordType} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"partialRecordType">> => PartialRecordType},
	mo_call_record68(MOCallRecord, Acc1);
mo_call_record67(MOCallRecord, Acc) ->
	mo_call_record68(MOCallRecord, Acc).
%% @hidden
mo_call_record68(#{guaranteedBitRate := GuaranteedBitRate} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"guaranteedBitRate">> => GuaranteedBitRate},
	mo_call_record69(MOCallRecord, Acc1);
mo_call_record68(MOCallRecord, Acc) ->
	mo_call_record69(MOCallRecord, Acc).
%% @hidden
mo_call_record69(#{maximumBitRate := MaximumBitRate} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"maximumBitRate">> => MaximumBitRate},
	mo_call_record70(MOCallRecord, Acc1);
mo_call_record69(MOCallRecord, Acc) ->
	mo_call_record70(MOCallRecord, Acc).
%% @hidden
mo_call_record70(#{redial := Redial} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"redial">> => Redial},
	mo_call_record71(MOCallRecord, Acc1);
mo_call_record70(MOCallRecord, Acc) ->
	mo_call_record71(MOCallRecord, Acc).
%% @hidden
mo_call_record71(#{reasonForServiceChange := ReasonForServiceChange} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"reasonForServiceChange">> => ReasonForServiceChange},
	mo_call_record72(MOCallRecord, Acc1);
mo_call_record71(MOCallRecord, Acc) ->
	mo_call_record72(MOCallRecord, Acc).
%% @hidden
mo_call_record72(#{serviceChangeInitiator := ServiceChangeInitiator} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"serviceChangeInitiator">> => ServiceChangeInitiator},
	mo_call_record73(MOCallRecord, Acc1);
mo_call_record72(MOCallRecord, Acc) ->
	mo_call_record73(MOCallRecord, Acc).
%% @hidden
mo_call_record73(#{iCSI2ActiveFlag := ICSI2ActiveFlag} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"iCSI2ActiveFlag">> => ICSI2ActiveFlag},
	mo_call_record74(MOCallRecord, Acc1);
mo_call_record73(MOCallRecord, Acc) ->
	mo_call_record74(MOCallRecord, Acc).
%% @hidden
mo_call_record74(#{'iMS-Charging-Identifier' := IMSChargingIdentifier} = MOCallRecord, Acc) ->
	Acc1 = Acc#{<<"iMS-Charging-Identifier">> => IMSChargingIdentifier},
	mo_call_record75(MOCallRecord, Acc1);
mo_call_record74(MOCallRecord, Acc) ->
	mo_call_record75(MOCallRecord, Acc).
%% @hidden
mo_call_record75(#{privateUserID := PrivateUserID} = _MOCallRecord, Acc) ->
	Acc#{<<"privateUserID">> => PrivateUserID};
mo_call_record75(_MOCallRecord, Acc) ->
	Acc.

%% @hidden
trunk_group({tkgpNumber, Number}) ->
	#{<<"tkgpNumber">> => Number};
trunk_group({tkgpName, Name}) ->
	#{<<"tkgpName">> => Name}.

%% @hidden
basic_service_code({bearerService, Code}) ->
	#{<<"bearerService">> => cgf_lib:octet_string(Code)};
basic_service_code({teleservice, Code}) ->
	#{<<"teleservice">> => cgf_lib:octet_string(Code)}.

%% @hidden
change_of_service(#{transparencyInd := TransparencyInd} = ChangeOfService) ->
	Acc = #{<<"transparencyInd">> => TransparencyInd},
	change_of_service1(ChangeOfService, Acc);
change_of_service(ChangeOfService) ->
	change_of_service1(ChangeOfService, #{}).
%% @hidden
change_of_service1(#{rateIndication := RateIndication} = ChangeOfService, Acc) ->
	Acc1 = Acc#{<<"rateIndication">> => RateIndication},
	change_of_service2(ChangeOfService, Acc1);
change_of_service1(ChangeOfService, Acc) ->
	change_of_service2(ChangeOfService, Acc).
%% @hidden
change_of_service2(#{fnur := Fnur} = ChangeOfService, Acc) ->
	Acc1 = Acc#{<<"fnur">> => Fnur},
	change_of_service3(ChangeOfService, Acc1);
change_of_service2(ChangeOfService, Acc) ->
	change_of_service3(ChangeOfService, Acc).
%% @hidden
change_of_service3(#{changeTime := ChangeTime} = ChangeOfService, Acc) ->
	Acc1 = Acc#{<<"changeTime">> => cgf_lib:bcd_date_time(ChangeTime)},
	change_of_service4(ChangeOfService, Acc1);
change_of_service3(ChangeOfService, Acc) ->
	change_of_service4(ChangeOfService, Acc).
%% @hidden
change_of_service4(#{basicService := BasicService} = _ChangeOfService, Acc) ->
	Acc#{<<"basicService">> => basic_service_code(BasicService)};
change_of_service4(_ChangeOfService, Acc) ->
	Acc.

%% @hidden
suppl_service_used(#{ssTime := SSTime} = SupplServicesUsed) ->
	Acc = #{<<"ssTime">> => cgf_lib:bcd_date_time(SSTime)},
	suppl_service_used1(SupplServicesUsed, Acc);
suppl_service_used(SupplServicesUsed) ->
	suppl_service_used1(SupplServicesUsed, #{}).
%% @hidden
suppl_service_used1(#{ssCode := Code}, Acc) ->
	Acc#{<<"ssCode">> => cgf_lib:octet_string(Code)};
suppl_service_used1(_SupplServicesUsed, Acc) ->
	Acc.

%% @hidden
level_camel_service(Level) ->
	level_camel_service(Level, []).
%% @hidden
level_camel_service([H | T], Acc) ->
	level_camel_service(T, [atom_to_list(H) | Acc]);
level_camel_service([], Acc) ->
	lists:reverse(Acc).

%% @hidden
mo_sms_record(#{servedIMSI := ServedIMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	mo_sms_record1(Record, Acc);
mo_sms_record(Record) ->
	mo_sms_record1(Record, #{}).
%% @hidden
mo_sms_record1(#{servedIMEI := ServedIMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	mo_sms_record2(Record, Acc1);
mo_sms_record1(Record, Acc) ->
	mo_sms_record2(Record, Acc).
%% @hidden
mo_sms_record2(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
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
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	mo_sms_record6(Record, Acc1);
mo_sms_record5(Record, Acc) ->
	mo_sms_record6(Record, Acc).
%% @hidden
mo_sms_record6(#{location := LocationAreaAndCell} = Record, Acc) ->
	Acc1 = Acc#{<<"location">> => location_area_and_cell(LocationAreaAndCell)},
	mo_sms_record7(Record, Acc1);
mo_sms_record6(Record, Acc) ->
	mo_sms_record7(Record, Acc).
%% @hidden
mo_sms_record7(#{messageReference := MessageReference} = Record, Acc) ->
	Acc1 = Acc#{<<"messageReference">> => cgf_lib:octet_string(MessageReference)},
	mo_sms_record8(Record, Acc1);
mo_sms_record7(Record, Acc) ->
	mo_sms_record8(Record, Acc).
%% @hidden
mo_sms_record8(#{originationTime := OriginationTime} = Record, Acc) ->
	Acc1 = Acc#{<<"originationTime">> => cgf_lib:bcd_date_time(OriginationTime)},
	mo_sms_record9(Record, Acc1);
mo_sms_record8(Record, Acc) ->
	mo_sms_record9(Record, Acc).
%% @hidden
mo_sms_record9(#{smsResult := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"smsResult">> => Diagnostics},
	mo_sms_record10(Record, Acc1);
mo_sms_record9(Record, Acc) ->
	mo_sms_record10(Record, Acc).
%% @hidden
mo_sms_record10(#{destinationNumber := DestinationNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"destinationNumber">> => cgf_lib:bcd_dn(DestinationNumber)},
	mo_sms_record11(Record, Acc1);
mo_sms_record10(Record, Acc) ->
	mo_sms_record11(Record, Acc).
%% @hidden
mo_sms_record11(#{cAMELSMSInformation := CAMELSMSInformation} = Record, Acc) ->
	Acc1 = Acc#{<<"cAMELSMSInformation">> => CAMELSMSInformation},
	mo_sms_record12(Record, Acc1);
mo_sms_record11(Record, Acc) ->
	mo_sms_record12(Record, Acc).
%% @hidden
mo_sms_record12(#{systemType := SystemType} = _Record, Acc) ->
	Acc#{<<"systemType">> => atom_to_binary(SystemType)};
mo_sms_record12(_Record, Acc) ->
	Acc.

%% @hidden
mt_call_record(#{location := LocationAreaAndCell} = Record) ->
	Acc = #{<<"location">> => location_area_and_cell(LocationAreaAndCell)},
	mt_call_record1(Record, Acc);
mt_call_record(Record) ->
	mt_call_record1(Record, #{}).
%% @hidden
mt_call_record1(#{supplServicesUsed := SupplServicesUsed} = Record, Acc) ->
	SSU = [suppl_service_used(Service) || Service <- SupplServicesUsed],
	Acc1 = Acc#{<<"supplServicesUsed">> => SSU},
	mt_call_record2(Record, Acc1);
mt_call_record1(Record, Acc) ->
	mt_call_record2(Record, Acc).
%% @hidden
mt_call_record2(#{releaseTime := ReleaseTime} = Record, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:bcd_date_time(ReleaseTime)},
	mt_call_record3(Record, Acc1);
mt_call_record2(Record, Acc) ->
	mt_call_record3(Record, Acc).
%% @hidden
mt_call_record3(#{changeOfAOCParms := ChangeOfAOCParms} = Record, Acc) ->
	NewChangeOfAOCParms = [aoc_params(Params) || Params <- ChangeOfAOCParms],
	Acc1 = Acc#{<<"changeOfAOCParms">> => NewChangeOfAOCParms},
	mt_call_record4(Record, Acc1);
mt_call_record3(Record, Acc) ->
	mt_call_record4(Record, Acc).
%% @hidden
mt_call_record4(#{fnur := FNUR} = Record, Acc) ->
	Acc1 = Acc#{<<"fnur">> => FNUR},
	mt_call_record5(Record, Acc1);
mt_call_record4(Record, Acc) ->
	mt_call_record5(Record, Acc).
%% @hidden
mt_call_record5(#{recordingEntity := RecordingEntity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	mt_call_record6(Record, Acc1);
mt_call_record5(Record, Acc) ->
	mt_call_record6(Record, Acc).
%% @hidden
mt_call_record6(#{reasonForServiceChange := ReasonForServiceChange} = Record, Acc) ->
	Acc1 = Acc#{<<"reasonForServiceChange">> => ReasonForServiceChange},
	mt_call_record7(Record, Acc1);
mt_call_record6(Record, Acc) ->
	mt_call_record7(Record, Acc).
%% @hidden
mt_call_record7(#{partialRecordType := PartialRecordType} = Record, Acc) ->
	Acc1 = Acc#{<<"partialRecordType">> => PartialRecordType},
	mt_call_record8(Record, Acc1);
mt_call_record7(Record, Acc) ->
	mt_call_record8(Record, Acc).
%% @hidden
mt_call_record8(#{causeForTerm := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	mt_call_record9(Record, Acc1);
mt_call_record8(Record, Acc) ->
	mt_call_record9(Record, Acc).
%% @hidden
mt_call_record9(#{speechVersionSupported := Identifier} = Record, Acc) ->
	Acc1 = Acc#{<<"speechVersionSupported">> => cgf_lib:octet_string(Identifier)},
	mt_call_record10(Record, Acc1);
mt_call_record9(Record, Acc) ->
	mt_call_record10(Record, Acc).
%% @hidden
mt_call_record10(#{speechVersionUsed := Identifier} = Record, Acc) ->
	Acc1 = Acc#{<<"speechVersionUsed">> => cgf_lib:octet_string(Identifier)},
	mt_call_record11(Record, Acc1);
mt_call_record10(Record, Acc) ->
	mt_call_record11(Record, Acc).
%% @hidden
mt_call_record11(#{changeOfService := ChangeOfService} = Record, Acc) ->
	Acc1 = Acc#{<<"changeOfService">> => change_of_service(ChangeOfService)},
	mt_call_record12(Record, Acc1);
mt_call_record11(Record, Acc) ->
	mt_call_record12(Record, Acc).
%% @hidden
mt_call_record12(#{answerTime := AnswerTime} = Record, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:bcd_date_time(AnswerTime)},
	mt_call_record13(Record, Acc1);
mt_call_record12(Record, Acc) ->
	mt_call_record13(Record, Acc).
%% @hidden
mt_call_record13(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	mt_call_record14(Record, Acc1);
mt_call_record13(Record, Acc) ->
	mt_call_record14(Record, Acc).
%% @hidden
mt_call_record14(#{changeOfHSCSDParms := ChangeOfHSCSDParms} = Record, Acc) ->
	NewParams = [change_of_params(Param) || Param <- ChangeOfHSCSDParms],
	Acc1 = Acc#{<<"changeOfHSCSDParms">> => NewParams},
	mt_call_record15(Record, Acc1);
mt_call_record14(Record, Acc) ->
	mt_call_record15(Record, Acc).
%% @hidden
mt_call_record15(#{jIPQuryStatus := JIPQuryStatus} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => JIPQuryStatus},
	mt_call_record16(Record, Acc1);
mt_call_record15(Record, Acc) ->
	mt_call_record16(Record, Acc).
%% @hidden
mt_call_record16(#{aiurRequested := AIURRequested} = Record, Acc) ->
	Acc1 = Acc#{<<"aiurRequested">> => AIURRequested},
	mt_call_record17(Record, Acc1);
mt_call_record16(Record, Acc) ->
	mt_call_record17(Record, Acc).
%% @hidden
mt_call_record17(#{callingNumber := CallingNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"callingNumber">> => cgf_lib:bcd_dn(CallingNumber)},
	mt_call_record18(Record, Acc1);
mt_call_record17(Record, Acc) ->
	mt_call_record18(Record, Acc).
%% @hidden
mt_call_record18(#{lrnQuryStatus := LrnQuryStatus} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => LrnQuryStatus},
	mt_call_record19(Record, Acc1);
mt_call_record18(Record, Acc) ->
	mt_call_record19(Record, Acc).
%% @hidden
mt_call_record19(#{aocParameters := AOCParameters} = Record, Acc) ->
	Acc1 = Acc#{<<"aocParameters">> => AOCParameters},
	mt_call_record20(Record, Acc1);
mt_call_record19(Record, Acc) ->
	mt_call_record20(Record, Acc).
%% @hidden
mt_call_record20(#{callDuration := Duration} = Record, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
	mt_call_record21(Record, Acc1);
mt_call_record20(Record, Acc) ->
	mt_call_record21(Record, Acc).
%% @hidden
mt_call_record21(#{mSCAddress := {_, {_, MSCAddress}} } = Record, Acc) ->
	Acc1 = Acc#{<<"mSCAddress">> => cgf_lib:ip_address(MSCAddress)},
	mt_call_record22(Record, Acc1);
mt_call_record21(Record, Acc) ->
	mt_call_record22(Record, Acc).
%% @hidden
mt_call_record22(#{privateUserID := PrivateUserID} = Record, Acc) ->
	Acc1 = Acc#{<<"privateUserID">> => PrivateUserID},
	mt_call_record23(Record, Acc1);
mt_call_record22(Record, Acc) ->
	mt_call_record23(Record, Acc).
%% @hidden
mt_call_record23(#{iCSI2ActiveFlag := _ICSI2ActiveFlag} = Record, Acc) ->
	Acc1 = Acc#{<<"iCSI2ActiveFlag">> => undefined},
	mt_call_record24(Record, Acc1);
mt_call_record23(Record, Acc) ->
	mt_call_record24(Record, Acc).
%% @hidden
mt_call_record24(#{jIPPara := JIPPara} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => JIPPara},
	mt_call_record25(Record, Acc1);
mt_call_record24(Record, Acc) ->
	mt_call_record25(Record, Acc).
%% @hidden
mt_call_record25(#{servedIMSI := ServedIMSI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	mt_call_record26(Record, Acc1);
mt_call_record25(Record, Acc) ->
	mt_call_record26(Record, Acc).
%% @hidden
mt_call_record26(#{seizureTime := SeizureTime} = Record, Acc) ->
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:bcd_date_time(SeizureTime)},
	mt_call_record27(Record, Acc1);
mt_call_record26(Record, Acc) ->
	mt_call_record27(Record, Acc).
%% @hidden
mt_call_record27(#{serviceChangeInitiator := ServiceChangeInitiator} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceChangeInitiator">> => ServiceChangeInitiator},
	mt_call_record28(Record, Acc1);
mt_call_record27(Record, Acc) ->
	mt_call_record28(Record, Acc).
%% @hidden
mt_call_record28(#{connectedNumber := ConnectedNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"connectedNumber">> => cgf_lib:bcd_dn(ConnectedNumber)},
	mt_call_record29(Record, Acc1);
mt_call_record28(Record, Acc) ->
	mt_call_record29(Record, Acc).
%% @hidden
mt_call_record29(#{'iMS-Charging-Identifier' := IMSChargingIdentifier} = Record, Acc) ->
	Acc1 = Acc#{<<"iMS-Charging-Identifier">> => cgf_lib:octet_string(IMSChargingIdentifier)},
	mt_call_record30(Record, Acc1);
mt_call_record29(Record, Acc) ->
	mt_call_record30(Record, Acc).
%% @hidden
mt_call_record30(#{serviceKey := ServiceKey} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceKey">> => ServiceKey},
	mt_call_record31(Record, Acc1);
mt_call_record30(Record, Acc) ->
	mt_call_record31(Record, Acc).
%% @hidden
mt_call_record31(#{chanCodingUsed := ChanCodingUsed} = Record, Acc) ->
	Acc1 = Acc#{<<"chanCodingUsed">> => ChanCodingUsed},
	mt_call_record32(Record, Acc1);
mt_call_record31(Record, Acc) ->
	mt_call_record32(Record, Acc).
%% @hidden
mt_call_record32(#{changeOfClassmark := ChangeOfClassmark} = Record, Acc) ->
	Acc1 = Acc#{<<"changeOfClassmark">> => ChangeOfClassmark},
	mt_call_record33(Record, Acc1);
mt_call_record32(Record, Acc) ->
	mt_call_record33(Record, Acc).
%% @hidden
mt_call_record33(#{dataVolume := DataVolume} = Record, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => DataVolume},
	mt_call_record34(Record, Acc1);
mt_call_record33(Record, Acc) ->
	mt_call_record34(Record, Acc).
%% @hidden
mt_call_record34(#{hSCSDChanAllocated := HSCSDChanAllocated} = Record, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanAllocated">> => HSCSDChanAllocated},
	mt_call_record35(Record, Acc1);
mt_call_record34(Record, Acc) ->
	mt_call_record35(Record, Acc).
%% @hidden
mt_call_record35(#{hSCSDChanRequested := HSCSDChanRequested} = Record, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanRequested">> => HSCSDChanRequested},
	mt_call_record36(Record, Acc1);
mt_call_record35(Record, Acc) ->
	mt_call_record36(Record, Acc).
%% @hidden
mt_call_record36(#{lrnSoInd := LrnSoInd} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => LrnSoInd},
	mt_call_record37(Record, Acc1);
mt_call_record36(Record, Acc) ->
	mt_call_record37(Record, Acc).
%% @hidden
mt_call_record37(#{systemType := SystemType} = Record, Acc) ->
	Acc1 = Acc#{<<"systemType">> => atom_to_binary(SystemType)},
	mt_call_record38(Record, Acc1);
mt_call_record37(Record, Acc) ->
	mt_call_record38(Record, Acc).
%% @hidden
mt_call_record38(#{'gsm-SCFAddress' := GsmSCFAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"gsm-SCFAddress">> => GsmSCFAddress},
	mt_call_record39(Record, Acc1);
mt_call_record38(Record, Acc) ->
	mt_call_record39(Record, Acc).
%% @hidden
mt_call_record39(#{mscOutgoingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	mt_call_record40(Record, Acc1);
mt_call_record39(Record, Acc) ->
	mt_call_record40(Record, Acc).
%% @hidden
mt_call_record40(#{sequenceNumber := SequenceNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => SequenceNumber},
	mt_call_record41(Record, Acc1);
mt_call_record40(Record, Acc) ->
	mt_call_record41(Record, Acc).
%% @hidden
mt_call_record41(#{chanCodingsAcceptable := ChanCodingsAcceptable} = Record, Acc) ->
	Acc1 = Acc#{<<"chanCodingsAcceptable">> => ChanCodingsAcceptable},
	mt_call_record42(Record, Acc1);
mt_call_record41(Record, Acc) ->
	mt_call_record42(Record, Acc).
%% @hidden
mt_call_record42(#{locationRoutNum := LocationRoutNum} = Record, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(LocationRoutNum)},
	mt_call_record43(Record, Acc1);
mt_call_record42(Record, Acc) ->
	mt_call_record43(Record, Acc).
%% @hidden
mt_call_record43(#{msClassmark := MSClassmark} = Record, Acc) ->
	Acc1 = Acc#{<<"msClassmark">> => cgf_lib:octet_string(MSClassmark)},
	mt_call_record44(Record, Acc1);
mt_call_record43(Record, Acc) ->
	mt_call_record44(Record, Acc).
%% @hidden
mt_call_record44(#{jIPSoInd := JIPSoInd} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => JIPSoInd},
	mt_call_record45(Record, Acc1);
mt_call_record44(Record, Acc) ->
	mt_call_record45(Record, Acc).
%% @hidden
mt_call_record45(#{networkCallReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"networkCallReference">> => cgf_lib:octet_string(Reference)},
	mt_call_record46(Record, Acc1);
mt_call_record45(Record, Acc) ->
	mt_call_record46(Record, Acc).
%% @hidden
mt_call_record46(#{radioChanUsed := RadioChanUsed} = Record, Acc) ->
	Acc1 = Acc#{<<"radioChanUsed">> => RadioChanUsed},
	mt_call_record47(Record, Acc1);
mt_call_record46(Record, Acc) ->
	mt_call_record47(Record, Acc).
%% @hidden
mt_call_record47(#{maximumBitRate := MaximumBitRate} = Record, Acc) ->
	Acc1 = Acc#{<<"maximumBitRate">> => MaximumBitRate},
	mt_call_record48(Record, Acc1);
mt_call_record47(Record, Acc) ->
	mt_call_record48(Record, Acc).
%% @hidden
mt_call_record48(#{radioChanRequested := RadioChanRequested} = Record, Acc) ->
	Acc1 = Acc#{<<"radioChanRequested">> => RadioChanRequested},
	mt_call_record49(Record, Acc1);
mt_call_record48(Record, Acc) ->
	mt_call_record49(Record, Acc).
%% @hidden
mt_call_record49(#{basicService := BasicService} = Record, Acc) ->
	Acc1 = Acc#{<<"basicService">> => basic_service_code(BasicService)},
	mt_call_record50(Record, Acc1);
mt_call_record49(Record, Acc) ->
	mt_call_record50(Record, Acc).
%% @hidden
mt_call_record50(#{guaranteedBitRate := GuaranteedBitRate} = Record, Acc) ->
	Acc1 = Acc#{<<"guaranteedBitRate">> => GuaranteedBitRate},
	mt_call_record51(Record, Acc1);
mt_call_record50(Record, Acc) ->
	mt_call_record51(Record, Acc).
%% @hidden
mt_call_record51(#{additionalChgInfo := AdditionalChgInfo} = Record, Acc) ->
	Acc1 = Acc#{<<"additionalChgInfo">> => AdditionalChgInfo},
	mt_call_record52(Record, Acc1);
mt_call_record51(Record, Acc) ->
	mt_call_record52(Record, Acc).
%% @hidden
mt_call_record52(#{changeOfLocation := ChangeOfLocation} = Record, Acc) ->
	Acc1 = Acc#{<<"changeOfLocation">> => ChangeOfLocation},
	mt_call_record53(Record, Acc1);
mt_call_record52(Record, Acc) ->
	mt_call_record53(Record, Acc).
%% @hidden
mt_call_record53(#{servedIMEI := ServedIMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	mt_call_record54(Record, Acc1);
mt_call_record53(Record, Acc) ->
	mt_call_record54(Record, Acc).
%% @hidden
mt_call_record54(#{diagnostics := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	mt_call_record55(Record, Acc1);
mt_call_record54(Record, Acc) ->
	mt_call_record55(Record, Acc).
%% @hidden
mt_call_record55(#{transparencyIndicator := TransparencyIndicator} = Record, Acc) ->
	Acc1 = Acc#{<<"transparencyIndicator">> => TransparencyIndicator},
	mt_call_record56(Record, Acc1);
mt_call_record55(Record, Acc) ->
	mt_call_record56(Record, Acc).
%% @hidden
mt_call_record56(#{mscIncomingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	mt_call_record57(Record, Acc1);
mt_call_record56(Record, Acc) ->
	mt_call_record57(Record, Acc).
%% @hidden
mt_call_record57(#{callReference := CallReference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(CallReference)},
	mt_call_record58(Record, Acc1);
mt_call_record57(Record, Acc) ->
	mt_call_record58(Record, Acc).
%% @hidden
mt_call_record58(#{rateIndication := RateIndication} = Record, Acc) ->
	Acc1 = Acc#{<<"rateIndication">> => RateIndication},
	mt_call_record59(Record, Acc1);
mt_call_record58(Record, Acc) ->
	mt_call_record59(Record, Acc).
%% @hidden
mt_call_record59(#{changeOfRadioChan := ChangeOfRadioChan} = _Record, Acc) ->
	Acc#{<<"changeOfRadioChan">> => ChangeOfRadioChan};
mt_call_record59(_Record, Acc) ->
	Acc.

aoc_params(#{newParameters := NewParameters} = Params) ->
	Acc = #{<<"newParameters">> => NewParameters},
	aoc_params1(Params, Acc);
aoc_params(Params) ->
	aoc_params1(Params, #{}).
%% @hidden
aoc_params1(#{changeTime := ChangeTime} = _Params, Acc) ->
	Acc#{<<"changeTime">> => cgf_lib:bcd_date_time(ChangeTime)};
aoc_params1(_Params, Acc) ->
	Acc.

change_of_params(#{initiatingParty := InitiatingParty} = ChangeOfHSCSDParms) ->
	Acc = #{<<"initiatingParty">> => InitiatingParty},
	change_of_params1(ChangeOfHSCSDParms, Acc);
change_of_params(ChangeOfHSCSDParms) ->
	change_of_params1(ChangeOfHSCSDParms, #{}).
%% @hidden
change_of_params1(#{hSCSDChanRequested := HSCSDChanRequested} =
		ChangeOfHSCSDParms, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanRequested">> => HSCSDChanRequested},
	change_of_params2(ChangeOfHSCSDParms, Acc1);
change_of_params1(ChangeOfHSCSDParms, Acc) ->
	change_of_params2(ChangeOfHSCSDParms, Acc).
%% @hidden
change_of_params2(#{hSCSDChanAllocated := HSCSDChanAllocated} =
		ChangeOfHSCSDParms, Acc) ->
	Acc1 = Acc#{<<"hSCSDChanAllocated">> => HSCSDChanAllocated},
	change_of_params3(ChangeOfHSCSDParms, Acc1);
change_of_params2(ChangeOfHSCSDParms, Acc) ->
	change_of_params3(ChangeOfHSCSDParms, Acc).
%% @hidden
change_of_params3(#{changeTime := ChangeTime} = ChangeOfHSCSDParms, Acc) ->
	Acc1 = Acc#{<<"changeTime">> => cgf_lib:bcd_date_time(ChangeTime)},
	change_of_params4(ChangeOfHSCSDParms, Acc1);
change_of_params3(ChangeOfHSCSDParms, Acc) ->
	change_of_params4(ChangeOfHSCSDParms, Acc).
%% @hidden
change_of_params4(#{chanCodingUsed := ChanCodingUsed} =
		ChangeOfHSCSDParms, Acc) ->
	Acc1 = Acc#{<<"chanCodingUsed">> => ChanCodingUsed},
	change_of_params5(ChangeOfHSCSDParms, Acc1);
change_of_params4(ChangeOfHSCSDParms, Acc) ->
	change_of_params5(ChangeOfHSCSDParms, Acc).
%% @hidden
change_of_params5(#{aiurRequested := AiurRequested} =
		_ChangeOfHSCSDParms, Acc) ->
	Acc#{<<"aiurRequested">> => AiurRequested};
change_of_params5(_ChangeOfHSCSDParms, Acc) ->
	Acc.

%% @hidden
mt_sms_record(#{systemType := SystemType} = Record) ->
	Acc = #{<<"systemType">> => atom_to_binary(SystemType)},
	mt_sms_record1(Record, Acc);
mt_sms_record(Record) ->
	mt_sms_record1(Record, #{}).
%% @hidden
mt_sms_record1(#{smsResult := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"smsResult">> => Diagnostics},
	mt_sms_record2(Record, Acc1);
mt_sms_record1(Record, Acc) ->
	mt_sms_record2(Record, Acc).
%% @hidden
mt_sms_record2(#{serviceCentre := ServiceCentre} = Record, Acc) ->
	Acc1 = Acc#{<<"serviceCentre">> => cgf_lib:bcd_dn(ServiceCentre)},
	mt_sms_record3(Record, Acc1);
mt_sms_record2(Record, Acc) ->
	mt_sms_record3(Record, Acc).
%% @hidden
mt_sms_record3(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	mt_sms_record4(Record, Acc1);
mt_sms_record3(Record, Acc) ->
	mt_sms_record4(Record, Acc).
%% @hidden
mt_sms_record4(#{servedIMSI := ServedIMSI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	mt_sms_record5(Record, Acc1);
mt_sms_record4(Record, Acc) ->
	mt_sms_record5(Record, Acc).
%% @hidden
mt_sms_record5(#{servedIMEI := ServedIMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	mt_sms_record6(Record, Acc1);
mt_sms_record5(Record, Acc) ->
	mt_sms_record6(Record, Acc).
%% @hidden
mt_sms_record6(#{recordingEntity := RecordingEntity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	mt_sms_record7(Record, Acc1);
mt_sms_record6(Record, Acc) ->
	mt_sms_record7(Record, Acc).
%% @hidden
mt_sms_record7(#{msClassmark := MSClassmark} = Record, Acc) ->
	Acc1 = Acc#{<<"msClassmark">> => cgf_lib:octet_string(MSClassmark)},
	mt_sms_record8(Record, Acc1);
mt_sms_record7(Record, Acc) ->
	mt_sms_record8(Record, Acc).
%% @hidden
mt_sms_record8(#{location := LocationAreaAndCell} = Record, Acc) ->
	Acc1 = Acc#{<<"location">> => location_area_and_cell(LocationAreaAndCell)},
	mt_sms_record9(Record, Acc1);
mt_sms_record8(Record, Acc) ->
	mt_sms_record9(Record, Acc).
%% @hidden
mt_sms_record9(#{deliveryTime := DeliveryTime} = Record, Acc) ->
	Acc1 = Acc#{<<"deliveryTime">> => cgf_lib:bcd_date_time(DeliveryTime)},
	mt_sms_record10(Record, Acc1);
mt_sms_record9(Record, Acc) ->
	mt_sms_record10(Record, Acc).
%% @hidden
mt_sms_record10(#{cAMELSMSInformation := CAMELSMSInformation} = _Record, Acc) ->
	Acc#{<<"cAMELSMSInformation">> => CAMELSMSInformation};
mt_sms_record10(_Record, Acc) ->
	Acc.

%% @hidden
ss_action_record(#{servedIMSI := ServedIMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	ss_action_record1(Record, Acc);
ss_action_record(Record) ->
	ss_action_record1(Record, #{}).
%% @hidden
ss_action_record1(#{servedIMEI := ServedIMEI} = Record, Acc) ->
	Acc1 = Acc#{<<"servedIMEI">> => cgf_lib:tbcd(ServedIMEI)},
	ss_action_record2(Record, Acc1);
ss_action_record1(Record, Acc) ->
	ss_action_record2(Record, Acc).
%% @hidden
ss_action_record2(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	ss_action_record3(Record, Acc1);
ss_action_record2(Record, Acc) ->
	ss_action_record3(Record, Acc).
%% @hidden
ss_action_record3(#{msClassmark := MSClassmark} = Record, Acc) ->
	Acc1 = Acc#{<<"msClassmark">> => cgf_lib:octet_string(MSClassmark)},
	ss_action_record4(Record, Acc1);
ss_action_record3(Record, Acc) ->
	ss_action_record4(Record, Acc).
%% @hidden
ss_action_record4(#{recordingEntity:= RecordingEntity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	ss_action_record5(Record, Acc1);
ss_action_record4(Record, Acc) ->
	ss_action_record5(Record, Acc).
%% @hidden
ss_action_record5(#{location := LocationAreaAndCell} = Record, Acc) ->
	Acc1 = Acc#{<<"location">> => location_area_and_cell(LocationAreaAndCell)},
	ss_action_record6(Record, Acc1);
ss_action_record5(Record, Acc) ->
	ss_action_record6(Record, Acc).
%% @hidden
ss_action_record6(#{basicServices := BasicServices} = Record, Acc) ->
	Codes = [basic_service_code(BS) || BS <- BasicServices],
	Acc1 = Acc#{<<"basicServices">> => Codes},
	ss_action_record7(Record, Acc1);
ss_action_record6(Record, Acc) ->
	ss_action_record7(Record, Acc).
%% @hidden
ss_action_record7(#{supplService := Code} = Record, Acc) ->
	Acc1 = Acc#{<<"supplService">> => cgf_lib:octet_string(Code)},
	ss_action_record8(Record, Acc1);
ss_action_record7(Record, Acc) ->
	ss_action_record8(Record, Acc).
%% @hidden
ss_action_record8(#{ssAction := ActionType} = Record, Acc) ->
	Acc1 = Acc#{<<"ssAction">> => ActionType},
	ss_action_record8(Record, Acc1);
ss_action_record8(Record, Acc) ->
	ss_action_record9(Record, Acc).
%% @hidden
ss_action_record9(#{ssActionTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"ssActionTime">> => cgf_lib:bcd_date_time(Time)},
	ss_action_record10(Record, Acc1);
ss_action_record9(Record, Acc) ->
	ss_action_record10(Record, Acc).
%% @hidden
ss_action_record10(#{ssParameters := Parameters} = Record, Acc) ->
	Acc1 = Acc#{<<"ssParameters">> => ss_parameters(Parameters)},
	ss_action_record11(Record, Acc1);
ss_action_record10(Record, Acc) ->
	ss_action_record11(Record, Acc).
%% @hidden
ss_action_record11(#{ssActionResult := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"ssActionResult">> => cgf_lib:diagnostics(Diagnostics)},
	ss_action_record12(Record, Acc1);
ss_action_record11(Record, Acc) ->
	ss_action_record12(Record, Acc).
%% @hidden
ss_action_record12(#{callReference := CallReference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(CallReference)},
	ss_action_record13(Record, Acc1);
ss_action_record12(Record, Acc) ->
	ss_action_record13(Record, Acc).
%% @hidden
ss_action_record13(#{systemType := SystemType}, Acc) ->
	Acc#{<<"systemType">> => atom_to_binary(SystemType)};
ss_action_record13(_Record, Acc) ->
	Acc.

%% @hidden
ss_parameters({forwardedToNumber, Number}) ->
	#{<<"forwardedToNumber">> => cgf_lib:bcd_dn(Number)};
ss_parameters({unstructuredData, Data}) ->
	#{<<"unstructuredData">> => cgf_lib:octet_string(Data)}.

%% @hidden
location_area_and_cell(#{locationAreaCode
		:= <<LAC:16>>} = LocationAreaAndCell) ->
	Acc = #{<<"locationAreaCode">> => LAC},
	location_area_and_cell1(LocationAreaAndCell, Acc);
location_area_and_cell(LocationAreaAndCell) ->
	location_area_and_cell1(LocationAreaAndCell, #{}).
%% @hidden
location_area_and_cell1(#{cellId
		:= <<CID:16>>} = LocationAreaAndCell, Acc) ->
	Acc1 = Acc#{<<"cellId">> => CID},
	location_area_and_cell2(LocationAreaAndCell, Acc1);
location_area_and_cell1(LocationAreaAndCell, Acc) ->
	location_area_and_cell2(LocationAreaAndCell, Acc).
%% @hidden
location_area_and_cell2(#{'mCC-MNC' := MccMnc}, Acc) ->
	Acc#{<<"cellId">> => cgf_lib:octet_string(MccMnc)};
location_area_and_cell2(_LocationAreaAndCell, Acc) ->
	Acc.

%% @hidden
inc_gateway_record(#{callingNumber := Number} = Record) ->
	Acc = #{<<"callingNumber">> => cgf_lib:bcd_dn(Number)},
	inc_gateway_record1(Record, Acc);
inc_gateway_record(Record) ->
	inc_gateway_record1(Record, #{}).
%% @hidden
inc_gateway_record1(#{calledNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"calledNumber">> => cgf_lib:bcd_dn(Number)},
	inc_gateway_record2(Record, Acc1);
inc_gateway_record1(Record, Acc) ->
	inc_gateway_record2(Record, Acc).
%% @hidden
inc_gateway_record2(#{recordingEntity := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(Number)},
	inc_gateway_record3(Record, Acc1);
inc_gateway_record2(Record, Acc) ->
	inc_gateway_record3(Record, Acc).
%% @hidden
inc_gateway_record3(#{mscIncomingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	inc_gateway_record4(Record, Acc1);
inc_gateway_record3(Record, Acc) ->
	inc_gateway_record4(Record, Acc).
%% @hidden
inc_gateway_record4(#{mscOutgoingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	inc_gateway_record5(Record, Acc1);
inc_gateway_record4(Record, Acc) ->
	inc_gateway_record5(Record, Acc).
%% @hidden
inc_gateway_record5(#{seizureTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:bcd_date_time(Time)},
	inc_gateway_record6(Record, Acc1);
inc_gateway_record5(Record, Acc) ->
	inc_gateway_record6(Record, Acc).
%% @hidden
inc_gateway_record6(#{answerTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:bcd_date_time(Time)},
	inc_gateway_record7(Record, Acc1);
inc_gateway_record6(Record, Acc) ->
	inc_gateway_record7(Record, Acc).
%% @hidden
inc_gateway_record7(#{releaseTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:bcd_date_time(Time)},
	inc_gateway_record8(Record, Acc1);
inc_gateway_record7(Record, Acc) ->
	inc_gateway_record8(Record, Acc).
%% @hidden
inc_gateway_record8(#{callDuration := Duration} = Record, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
	inc_gateway_record9(Record, Acc1);
inc_gateway_record8(Record, Acc) ->
	inc_gateway_record9(Record, Acc).
%% @hidden
inc_gateway_record9(#{dataVolume := Volume} = Record, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => Volume},
	inc_gateway_record10(Record, Acc1);
inc_gateway_record9(Record, Acc) ->
	inc_gateway_record10(Record, Acc).
%% @hidden
inc_gateway_record10(#{causeForTerm := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	inc_gateway_record11(Record, Acc1);
inc_gateway_record10(Record, Acc) ->
	inc_gateway_record11(Record, Acc).
%% @hidden
inc_gateway_record11(#{diagnostics := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	inc_gateway_record12(Record, Acc1);
inc_gateway_record11(Record, Acc) ->
	inc_gateway_record12(Record, Acc).
%% @hidden
inc_gateway_record12(#{callReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(Reference)},
	inc_gateway_record13(Record, Acc1);
inc_gateway_record12(Record, Acc) ->
	inc_gateway_record13(Record, Acc).
%% @hidden
inc_gateway_record13(#{sequenceNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => Number},
	inc_gateway_record14(Record, Acc1);
inc_gateway_record13(Record, Acc) ->
	inc_gateway_record14(Record, Acc).
%% @hidden
inc_gateway_record14(#{locationRoutNum := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(Number)},
	inc_gateway_record15(Record, Acc1);
inc_gateway_record14(Record, Acc) ->
	inc_gateway_record15(Record, Acc).
%% @hidden
inc_gateway_record15(#{lrnSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => Indicator},
	inc_gateway_record16(Record, Acc1);
inc_gateway_record15(Record, Acc) ->
	inc_gateway_record16(Record, Acc).
%% @hidden
inc_gateway_record16(#{lrnQuryStatus := Status} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => Status},
	inc_gateway_record17(Record, Acc1);
inc_gateway_record16(Record, Acc) ->
	inc_gateway_record17(Record, Acc).
%% @hidden
inc_gateway_record17(#{jIPPara := Parameter} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => Parameter},
	inc_gateway_record18(Record, Acc1);
inc_gateway_record17(Record, Acc) ->
	inc_gateway_record18(Record, Acc).
%% @hidden
inc_gateway_record18(#{jIPSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => Indicator},
	inc_gateway_record19(Record, Acc1);
inc_gateway_record18(Record, Acc) ->
	inc_gateway_record19(Record, Acc).
%% @hidden
inc_gateway_record19(#{jIPQuryStatus := Status} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => Status},
	inc_gateway_record20(Record, Acc1);
inc_gateway_record19(Record, Acc) ->
	inc_gateway_record20(Record, Acc).
%% @hidden
inc_gateway_record20(#{reasonForServiceChange := Reason} = Record, Acc) ->
	Acc1 = Acc#{<<"reasonForServiceChange">> => Reason},
	inc_gateway_record21(Record, Acc1);
inc_gateway_record20(Record, Acc) ->
	inc_gateway_record21(Record, Acc).
%% @hidden
inc_gateway_record21(#{serviceChangeInitiator := Boolean}, Acc) ->
	Acc#{<<"reasonForServiceChange">> => Boolean};
inc_gateway_record21(_Record, Acc) ->
	Acc.

%% @hidden
out_gateway_record(#{callingNumber := Number} = Record) ->
	Acc = #{<<"callingNumber">> => cgf_lib:bcd_dn(Number)},
	out_gateway_record1(Record, Acc);
out_gateway_record(Record) ->
	out_gateway_record1(Record, #{}).
%% @hidden
out_gateway_record1(#{calledNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"calledNumber">> => cgf_lib:bcd_dn(Number)},
	out_gateway_record2(Record, Acc1);
out_gateway_record1(Record, Acc) ->
	out_gateway_record2(Record, Acc).
%% @hidden
out_gateway_record2(#{recordingEntity := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(Number)},
	out_gateway_record3(Record, Acc1);
out_gateway_record2(Record, Acc) ->
	out_gateway_record3(Record, Acc).
%% @hidden
out_gateway_record3(#{mscIncomingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	out_gateway_record4(Record, Acc1);
out_gateway_record3(Record, Acc) ->
	out_gateway_record4(Record, Acc).
%% @hidden
out_gateway_record4(#{mscOutgoingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	out_gateway_record5(Record, Acc1);
out_gateway_record4(Record, Acc) ->
	out_gateway_record5(Record, Acc).
%% @hidden
out_gateway_record5(#{seizureTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:bcd_date_time(Time)},
	out_gateway_record6(Record, Acc1);
out_gateway_record5(Record, Acc) ->
	out_gateway_record6(Record, Acc).
%% @hidden
out_gateway_record6(#{answerTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:bcd_date_time(Time)},
	out_gateway_record7(Record, Acc1);
out_gateway_record6(Record, Acc) ->
	out_gateway_record7(Record, Acc).
%% @hidden
out_gateway_record7(#{releaseTime := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:bcd_date_time(Time)},
	out_gateway_record8(Record, Acc1);
out_gateway_record7(Record, Acc) ->
	out_gateway_record8(Record, Acc).
%% @hidden
out_gateway_record8(#{callDuration := Duration} = Record, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
	out_gateway_record9(Record, Acc1);
out_gateway_record8(Record, Acc) ->
	out_gateway_record9(Record, Acc).
%% @hidden
out_gateway_record9(#{dataVolume := Volume} = Record, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => Volume},
	out_gateway_record10(Record, Acc1);
out_gateway_record9(Record, Acc) ->
	out_gateway_record10(Record, Acc).
%% @hidden
out_gateway_record10(#{causeForTerm := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	out_gateway_record11(Record, Acc1);
out_gateway_record10(Record, Acc) ->
	out_gateway_record11(Record, Acc).
%% @hidden
out_gateway_record11(#{diagnostics := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	out_gateway_record12(Record, Acc1);
out_gateway_record11(Record, Acc) ->
	out_gateway_record12(Record, Acc).
%% @hidden
out_gateway_record12(#{callReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(Reference)},
	out_gateway_record13(Record, Acc1);
out_gateway_record12(Record, Acc) ->
	out_gateway_record13(Record, Acc).
%% @hidden
out_gateway_record13(#{sequenceNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => Number},
	out_gateway_record14(Record, Acc1);
out_gateway_record13(Record, Acc) ->
	out_gateway_record14(Record, Acc).
%% @hidden
out_gateway_record14(#{locationRoutNum := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(Number)},
	out_gateway_record15(Record, Acc1);
out_gateway_record14(Record, Acc) ->
	out_gateway_record15(Record, Acc).
%% @hidden
out_gateway_record15(#{lrnSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => Indicator},
	out_gateway_record16(Record, Acc1);
out_gateway_record15(Record, Acc) ->
	out_gateway_record16(Record, Acc).
%% @hidden
out_gateway_record16(#{lrnQuryStatus := Status} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => Status},
	out_gateway_record17(Record, Acc1);
out_gateway_record16(Record, Acc) ->
	out_gateway_record17(Record, Acc).
%% @hidden
out_gateway_record17(#{jIPPara := Parameter} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => Parameter},
	out_gateway_record18(Record, Acc1);
out_gateway_record17(Record, Acc) ->
	out_gateway_record18(Record, Acc).
%% @hidden
out_gateway_record18(#{jIPSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => Indicator},
	out_gateway_record19(Record, Acc1);
out_gateway_record18(Record, Acc) ->
	out_gateway_record19(Record, Acc).
%% @hidden
out_gateway_record19(#{jIPQuryStatus := Status} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => Status},
	out_gateway_record20(Record, Acc1);
out_gateway_record19(Record, Acc) ->
	out_gateway_record20(Record, Acc).
%% @hidden
out_gateway_record20(#{reasonForServiceChange := Reason} = Record, Acc) ->
	Acc1 = Acc#{<<"reasonForServiceChange">> => Reason},
	out_gateway_record21(Record, Acc1);
out_gateway_record20(Record, Acc) ->
	out_gateway_record21(Record, Acc).
%% @hidden
out_gateway_record21(#{serviceChangeInitiator := Boolean}, Acc) ->
	Acc#{<<"reasonForServiceChange">> => Boolean};
out_gateway_record21(_Record, Acc) ->
	Acc.

%% @hidden
transit_call_record(#{recordingEntity := Number} = Record) ->
	Acc = #{<<"recordingEntity">> => cgf_lib:bcd_dn(Number)},
	transit_call_record1(Record, Acc);
transit_call_record(Record) ->
	transit_call_record1(Record, #{}).
%% @hidden
transit_call_record1(#{mscIncomingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	transit_call_record2(Record, Acc1);
transit_call_record1(Record, Acc) ->
	transit_call_record2(Record, Acc).
%% @hidden
transit_call_record2(#{mscOutgoingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	transit_call_record3(Record, Acc1);
transit_call_record2(Record, Acc) ->
	transit_call_record3(Record, Acc).
%% @hidden
transit_call_record3(#{callingNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"callingNumber">> => cgf_lib:bcd_dn(Number)},
	transit_call_record4(Record, Acc1);
transit_call_record3(Record, Acc) ->
	transit_call_record4(Record, Acc).
%% @hidden
transit_call_record4(#{calledNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"calledNumber">> => cgf_lib:bcd_dn(Number)},
	transit_call_record5(Record, Acc1);
transit_call_record4(Record, Acc) ->
	transit_call_record5(Record, Acc).
%% @hidden
transit_call_record5(#{isdnBasicService := BasicService} = Record, Acc) ->
	Acc1 = Acc#{<<"isdnBasicServic">> => BasicService},
	transit_call_record6(Record, Acc1);
transit_call_record5(Record, Acc) ->
	transit_call_record6(Record, Acc).
%% @hidden
transit_call_record6(#{seizureTimestamp := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"seizureTimestamp">> => cgf_lib:bcd_date_time(Time)},
	transit_call_record7(Record, Acc1);
transit_call_record6(Record, Acc) ->
	transit_call_record7(Record, Acc).
%% @hidden
transit_call_record7(#{answerTimestamp := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"answerTimestamp">> => cgf_lib:bcd_date_time(Time)},
	transit_call_record8(Record, Acc1);
transit_call_record7(Record, Acc) ->
	transit_call_record8(Record, Acc).
%% @hidden
transit_call_record8(#{ releaseTimestamp := Time} = Record, Acc) ->
	Acc1 = Acc#{<<"releaseTimestamp">> => cgf_lib:bcd_date_time(Time)},
	transit_call_record9(Record, Acc1);
transit_call_record8(Record, Acc) ->
	transit_call_record9(Record, Acc).
%% @hidden
transit_call_record9(#{callDuration := Duration} = Record, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
	transit_call_record10(Record, Acc1);
transit_call_record9(Record, Acc) ->
	transit_call_record10(Record, Acc).
%% @hidden
transit_call_record10(#{dataVolume := Volume} = Record, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => Volume},
	transit_call_record11(Record, Acc1);
transit_call_record10(Record, Acc) ->
	transit_call_record11(Record, Acc).
%% @hidden
transit_call_record11(#{causeForTerm := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	transit_call_record12(Record, Acc1);
transit_call_record11(Record, Acc) ->
	transit_call_record12(Record, Acc).
%% @hidden
transit_call_record12(#{diagnostics := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	transit_call_record13(Record, Acc1);
transit_call_record12(Record, Acc) ->
	transit_call_record13(Record, Acc).
%% @hidden
transit_call_record13(#{callReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(Reference)},
	transit_call_record14(Record, Acc1);
transit_call_record13(Record, Acc) ->
	transit_call_record14(Record, Acc).
%% @hidden
transit_call_record14(#{sequenceNumber := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => Number},
	transit_call_record15(Record, Acc1);
transit_call_record14(Record, Acc) ->
	transit_call_record15(Record, Acc).
%% @hidden
transit_call_record15(#{locationRoutNum := Number} = Record, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(Number)},
	transit_call_record16(Record, Acc1);
transit_call_record15(Record, Acc) ->
	transit_call_record16(Record, Acc).
%% @hidden
transit_call_record16(#{lrnSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => Indicator},
	transit_call_record17(Record, Acc1);
transit_call_record16(Record, Acc) ->
	transit_call_record17(Record, Acc).
%% @hidden
transit_call_record17(#{lrnQuryStatus := Status} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => Status},
	transit_call_record18(Record, Acc1);
transit_call_record17(Record, Acc) ->
	transit_call_record18(Record, Acc).
%% @hidden
transit_call_record18(#{jIPPara := Parameter} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => Parameter},
	transit_call_record19(Record, Acc1);
transit_call_record18(Record, Acc) ->
	transit_call_record19(Record, Acc).
%% @hidden
transit_call_record19(#{jIPSoInd := Indicator} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => Indicator},
	transit_call_record20(Record, Acc1);
transit_call_record19(Record, Acc) ->
	transit_call_record20(Record, Acc).
%% @hidden
transit_call_record20(#{jIPQuryStatus := Status} = _Record, Acc) ->
	Acc#{<<"jIPQuryStatus">> => Status};
transit_call_record20(_Record, Acc) ->
	Acc.

%% @hidden
roaming_record(#{servedIMSI := ServedIMSI} = Record) ->
	Acc = #{<<"servedIMSI">> => cgf_lib:tbcd(ServedIMSI)},
	roaming_record1(Record, Acc);
roaming_record(Record) ->
	roaming_record1(Record, #{}).
%% @hidden
roaming_record1(#{servedMSISDN := ServedMSISDN} = Record, Acc) ->
	#{<<"address">> := MSISDN} = cgf_lib:bcd_dn(ServedMSISDN),
	Acc1 = Acc#{<<"servedMSISDN">> => MSISDN},
	roaming_record2(Record, Acc1);
roaming_record1(Record, Acc) ->
	roaming_record2(Record, Acc).
%% @hidden
roaming_record2(#{callingNumber := CallingNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"callingNumber">> => cgf_lib:bcd_dn(CallingNumber)},
	roaming_record3(Record, Acc1);
roaming_record2(Record, Acc) ->
	roaming_record3(Record, Acc).
%% @hidden
roaming_record3(#{roamingNumber := RoamingNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"roamingNumber">> => cgf_lib:bcd_dn(RoamingNumber)},
	roaming_record4(Record, Acc1);
roaming_record3(Record, Acc) ->
	roaming_record4(Record, Acc).
%% @hidden
roaming_record4(#{recordingEntity := RecordingEntity} = Record, Acc) ->
	Acc1 = Acc#{<<"recordingEntity">> => cgf_lib:bcd_dn(RecordingEntity)},
	roaming_record5(Record, Acc1);
roaming_record4(Record, Acc) ->
	roaming_record5(Record, Acc).
%% @hidden
roaming_record5(#{mscIncomingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscIncomingTKGP">> => trunk_group(TrunkGroup)},
	roaming_record6(Record, Acc1);
roaming_record5(Record, Acc) ->
	roaming_record6(Record, Acc).
%% @hidden
roaming_record6(#{mscOutgoingTKGP := TrunkGroup} = Record, Acc) ->
	Acc1 = Acc#{<<"mscOutgoingTKGP">> => trunk_group(TrunkGroup)},
	roaming_record7(Record, Acc1);
roaming_record6(Record, Acc) ->
	roaming_record7(Record, Acc).
%% @hidden
roaming_record7(#{basicService := BasicService} = Record, Acc) ->
	Acc1 = Acc#{<<"basicService">> => basic_service_code(BasicService)},
	roaming_record8(Record, Acc1);
roaming_record7(Record, Acc) ->
	roaming_record8(Record, Acc).
%% @hidden
roaming_record8(#{transparencyIndicator := TransparencyIndicator} = Record, Acc) ->
	Acc1 = Acc#{<<"transparencyIndicator">> => TransparencyIndicator},
	roaming_record9(Record, Acc1);
roaming_record8(Record, Acc) ->
	roaming_record9(Record, Acc).
%% @hidden
roaming_record9(#{changeOfService := ChangeOfService} = Record, Acc) ->
	Acc1 = Acc#{<<"changeOfService">> => change_of_service(ChangeOfService)},
	roaming_record10(Record, Acc1);
roaming_record9(Record, Acc) ->
	roaming_record10(Record, Acc).
%% @hidden
roaming_record10(#{supplServicesUsed := SupplServicesUsed} = Record, Acc) ->
	SSU = [suppl_service_used(Service) || Service <- SupplServicesUsed],
	Acc1 = Acc#{<<"supplServicesUsed">> => SSU},
	roaming_record11(Record, Acc1);
roaming_record10(Record, Acc) ->
	roaming_record11(Record, Acc).
%% @hidden
roaming_record11(#{seizureTime := SeizureTime} = Record, Acc) ->
	Acc1 = Acc#{<<"seizureTime">> => cgf_lib:bcd_date_time(SeizureTime)},
	roaming_record12(Record, Acc1);
roaming_record11(Record, Acc) ->
	roaming_record12(Record, Acc).
%% @hidden
roaming_record12(#{answerTime := AnswerTime} = Record, Acc) ->
	Acc1 = Acc#{<<"answerTime">> => cgf_lib:bcd_date_time(AnswerTime)},
	roaming_record13(Record, Acc1);
roaming_record12(Record, Acc) ->
	roaming_record13(Record, Acc).
%% @hidden
roaming_record13(#{releaseTime := ReleaseTime} = Record, Acc) ->
	Acc1 = Acc#{<<"releaseTime">> => cgf_lib:bcd_date_time(ReleaseTime)},
	roaming_record14(Record, Acc1);
roaming_record13(Record, Acc) ->
	roaming_record14(Record, Acc).
%% @hidden
roaming_record14(#{callDuration := Duration} = Record, Acc) ->
	Acc1 = Acc#{<<"callDuration">> => Duration},
	roaming_record15(Record, Acc1);
roaming_record14(Record, Acc) ->
	roaming_record15(Record, Acc).
%% @hidden
roaming_record15(#{dataVolume := DataVolume} = Record, Acc) ->
	Acc1 = Acc#{<<"dataVolume">> => DataVolume},
	roaming_record16(Record, Acc1);
roaming_record15(Record, Acc) ->
	roaming_record16(Record, Acc).
%% @hidden
roaming_record16(#{causeForTerm := Cause} = Record, Acc) ->
	Acc1 = Acc#{<<"causeForTerm">> => atom_to_binary(Cause)},
	roaming_record17(Record, Acc1);
roaming_record16(Record, Acc) ->
	roaming_record17(Record, Acc).
%% @hidden
roaming_record17(#{diagnostics := Diagnostics} = Record, Acc) ->
	Acc1 = Acc#{<<"diagnostics">> => cgf_lib:diagnostics(Diagnostics)},
	roaming_record18(Record, Acc1);
roaming_record17(Record, Acc) ->
	roaming_record18(Record, Acc).
%% @hidden
roaming_record18(#{callReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"callReference">> => cgf_lib:octet_string(Reference)},
	roaming_record19(Record, Acc1);
roaming_record18(Record, Acc) ->
	roaming_record19(Record, Acc).
%% @hidden
roaming_record19(#{sequenceNumber := SequenceNumber} = Record, Acc) ->
	Acc1 = Acc#{<<"sequenceNumber">> => SequenceNumber},
	roaming_record20(Record, Acc1);
roaming_record19(Record, Acc) ->
	roaming_record20(Record, Acc).
%% @hidden
roaming_record20(#{networkCallReference := Reference} = Record, Acc) ->
	Acc1 = Acc#{<<"networkCallReference">> => cgf_lib:octet_string(Reference)},
	roaming_record21(Record, Acc1);
roaming_record20(Record, Acc) ->
	roaming_record21(Record, Acc).
%% @hidden
roaming_record21(#{mSCAddress := MSCAddress} = Record, Acc) ->
	Acc1 = Acc#{<<"mSCAddress">> => cgf_lib:bcd_dn(MSCAddress)},
	roaming_record22(Record, Acc1);
roaming_record21(Record, Acc) ->
	roaming_record22(Record, Acc).
%% @hidden
roaming_record22(#{locationRoutNum := LocationRoutNum} = Record, Acc) ->
	Acc1 = Acc#{<<"locationRoutNum">> => cgf_lib:tbcd(LocationRoutNum)},
	roaming_record23(Record, Acc1);
roaming_record22(Record, Acc) ->
	roaming_record23(Record, Acc).
%% @hidden
roaming_record23(#{lrnSoInd := LrnSoInd} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnSoInd">> => LrnSoInd},
	roaming_record24(Record, Acc1);
roaming_record23(Record, Acc) ->
	roaming_record24(Record, Acc).
%% @hidden
roaming_record24(#{lrnQuryStatus := LrnQuryStatus} = Record, Acc) ->
	Acc1 = Acc#{<<"lrnQuryStatus">> => LrnQuryStatus},
	roaming_record25(Record, Acc1);
roaming_record24(Record, Acc) ->
	roaming_record25(Record, Acc).
%% @hidden
roaming_record25(#{jIPPara := JIPPara} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPPara">> => JIPPara},
	roaming_record26(Record, Acc1);
roaming_record25(Record, Acc) ->
	roaming_record26(Record, Acc).
%% @hidden
roaming_record26(#{jIPSoInd := JIPSoInd} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPSoInd">> => JIPSoInd},
	roaming_record27(Record, Acc1);
roaming_record26(Record, Acc) ->
	roaming_record27(Record, Acc).
%% @hidden
roaming_record27(#{jIPQuryStatus := JIPQuryStatus} = Record, Acc) ->
	Acc1 = Acc#{<<"jIPQuryStatus">> => JIPQuryStatus},
	roaming_record28(Record, Acc1);
roaming_record27(Record, Acc) ->
	roaming_record28(Record, Acc).
%% @hidden
roaming_record28(#{partialRecordType := PartialRecordType}, Acc) ->
	Acc#{<<"partialRecordType">> => PartialRecordType};
roaming_record28(_Record, Acc) ->
	Acc.

