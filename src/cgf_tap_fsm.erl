%%% cgf_tap_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
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
%%% @doc This {@link //cgf/cgf_import_fsm. cgf_import_fsm} callback module
%%% 	implements a GSMA TAP3 CDR import FSM within
%%% 	the {@link //cgf. cgf} application.
%%%
%%% 	This call back module handles reading a GSMA TAP3 format file of
%%% 	ASN.1 BER encoded CDR, parsing the charging data records
%%% 	and logging to a JSON Bx interface log.
%%%
%%% 	== State Transitions ==
%%% 	The following diagram depicts the states, and events which drive
%%% 	state transitions, in the finite state machine (FSM):
%%%
%%% 	<img alt="state machine" src="import-fsm.svg" />
%%%
-module(cgf_tap_fsm).
-copyright('Copyright (c) 2025 SigScale Global Inc.').

%% export the cgf_import_fsm callback API
-export([init/1, open/2, read/2, parse/3, close/2]).

-behaviour(cgf_import_fsm).

-type statedata() ::
		#{filename := file:filename() | binary(),
		log := disk_log:log(),
		metadata => [{AttributeName :: string(),
				AttributeValue :: term()}],
		extra_args => [term()],
		tap_state => cgf_tap:state()}.

%%----------------------------------------------------------------------
%%  The cgf_import_fsm callback API
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateData} | {error, Reason},
		StateData :: statedata(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
init([Filename, Log] = _Args) ->
	init([Filename, Log, #{}]);
init([Filename, Log, Metadata | ExtraArgs] = _Args) ->
	NewMetadata = metadata(Filename, Metadata),
	StateData = #{filename => Filename, log => Log,
			metadata => maps:to_list(NewMetadata),
			extra_args => ExtraArgs},
	{ok, StateData}.

-spec open(Filename, StateData) -> Result
	when
		Filename :: file:filename() | binary(),
		StateData :: statedata(),
		Result :: {continue, Cont, StateData}
				| {error, Reason},
		Cont :: binary(),
		Reason :: term().
%% @doc Handles events received in the <em>open</em> state.
%%
%% 	Open the `Filename' for subsequent reading.
%% @todo Read file in chunks.
open(Filename, StateData) ->
	case file:read_file(Filename) of
		{ok, Cont} ->
			{continue, Cont, StateData};
		{error, Reason} ->
			{error, Reason}
	end.

-spec read(Cont, StateData) -> Result
	when
		Cont :: binary() | map(),
		StateData :: statedata(),
		Result :: {continue, CDR, Cont, StateData}
				| {error, Reason, Cont, StateData}
				| {close, Reason, Cont, StateData},
		CDR :: tuple(),
		Reason :: asn1_decode | normal | shutdown | term().
%% @doc Handles events received in the <em>read</em> state.
%%
%% 	Read charging data.
%%
read(Cont, StateData)
		when is_binary(Cont) ->
	case decode_tap(Cont, StateData) of
		{ok, TransferBatch, NewStateData} ->
			read(TransferBatch, NewStateData);
		{error, {asn1, _Description}} ->
			{close, asn1_decode, <<>>, StateData}
	end;
read(#{callEventDetails := []} = TransferBatch, StateData) ->
	{close, normal, TransferBatch, StateData};
read(#{callEventDetails := [H | T]} = TransferBatch, StateData) ->
	Cont1 = TransferBatch#{callEventDetails => T},
	{continue, H, Cont1, StateData}.

-spec parse(CDR, Log, StateData) -> Result
	when
		CDR :: tuple(),
		Log :: disk_log:log(),
		StateData :: statedata(),
		Result :: {continue, StateData}
				| {error, Reason, StateData}
				| {close, Reason, StateData}
				| {stop, Reason, StateData},
		Reason :: normal | shutdown | term().
%% @doc Handles events received in the <em>parse</em> state.
%%
%% 	Parse and log a charging data record (CDR).
%%
parse(CDR, Log,
		#{metadata := Metadata, tap_state := TapState} = StateData) ->
	case cgf_tap:parse(Log, Metadata, TapState, CDR) of
		ok ->
			{continue, StateData};
		{error, Reason} ->
			{error, Reason, StateData}
	end.

-spec close(Cont, StateData) -> Result
	when
		Cont :: binary() | map(),
		StateData :: statedata(),
		Result :: {stop, StateData}
				| {error, Reason, StateData},
		Reason :: normal | shutdown | term().
%% @doc Handles events received in the <em>close</em> state.
close(_Cont, StateData) ->
	{stop, StateData}.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
metadata(Filename, Metadata) ->
	MimeType = "application/octet-stream",
	FileMap = #{"name" => filename:basename(Filename),
			"mime_type" => MimeType},
	metadata1(FileMap, Metadata).
%% @hidden
metadata1(FileMap, #{"log" := MetaLog} = Metadata) ->
	F = fun("file", MetaValue, FileValue) ->
				maps:merge(MetaValue, FileValue);
			(_, MetaValue, _) ->
				MetaValue
	end,
	MetaLog1 = maps:merge_with(F, MetaLog, #{"file" => FileMap}),
	Metadata#{"log" => MetaLog1};
metadata1(FileMap, Metadata) ->
	Metadata#{"log" => #{"file" => FileMap}}.

%% @hidden
decode_tap(Bin, StateData) ->
	case 'TAP-0312':decode('DataInterChange', Bin) of
		{ok, {transferBatch, TransferBatch}, <<>>} ->
			decode_tap1(TransferBatch, StateData);
		{ok, {notification, _Notification}, _Rest} ->
			{error, not_implemented};
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
decode_tap1(#{accountingInfo := AccountingInfo} = TransferBatch,
		#{metadata := Metadata} = StateData) ->
	Metadata1 = case cgf_tap:parse_accounting(AccountingInfo) of
		AccountingInfo1 when map_size(AccountingInfo1) > 0 ->
			[{roam_accountingInfo, AccountingInfo1} | Metadata];
		_ ->
			Metadata
	end,
	NewStateData = StateData#{metadata => Metadata1},
	decode_tap2(TransferBatch, NewStateData);
decode_tap1(TransferBatch, StateData) ->
	decode_tap2(TransferBatch, StateData).
%% @hidden
decode_tap2(#{batchControlInfo := BatchControlInfo} = TransferBatch,
		#{metadata := Metadata} = StateData)
		when map_size(BatchControlInfo) > 0 ->
	Metadata1 = case cgf_tap:parse_batchcontrol(BatchControlInfo) of
		BatchControlInfo1 when map_size(BatchControlInfo1) > 0 ->
			[{roam_batchControlInfo, BatchControlInfo1} | Metadata];
		_ ->
			Metadata
	end,
	NewStateData = StateData#{metadata => Metadata1},
	decode_tap3(TransferBatch, NewStateData);
decode_tap2(TransferBatch, StateData) ->
	decode_tap3(TransferBatch, StateData).
%% @hidden
decode_tap3(#{networkInfo := NetworkInfo} = TransferBatch,
		StateData) when map_size(NetworkInfo) > 0 ->
	TapState = cgf_tap:parse_networkinfo(NetworkInfo),
	NewStateData = StateData#{tap_state => TapState},
	{ok, TransferBatch, NewStateData};
decode_tap3(TransferBatch, StateData) ->
	{ok, TransferBatch, StateData}.

