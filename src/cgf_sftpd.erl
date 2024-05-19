%%% cgf_sftpd.erl
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
%%% @doc This {@link //ssh/ssh_server_channel. ssh_server_channel} callback
%%% 	module implements a `sftpd' subsystem in
%%% 	the {@link //cgf. cgf} application.
%%%
%%% 	The SSH `sftpd' subsystem implemented in this module is a wrapper
%%% 	around the {@link //ssh/ssh_sftpd. ssh_sftpd} callback module with
%%% 	the additional procedure of changing the `root' directory to a
%%% 	subdirectory with the name of the username for the session
%%% 	(`root/username'). This	sandboxes a session so they cannot see files
%%% 	belonging to other users.
%%%
-module(cgf_sftpd).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-behaviour(ssh_server_channel).

% export the public api
-export([subsystem_spec/1]).

% export the ssh_server_channel callbacks
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-include("ssh_sftpd.hrl").
-include_lib("ssh/src/ssh.hrl").
-include_lib("ssh/src/ssh_xfer.hrl").

-export_type([option/0, options/0]).
-type option() :: {cwd, string()}
		| {file_handler, CbMod :: atom() | {CbMod :: atom(), FileState :: term()}}
		| {max_files, integer()}
		| {root, string()}
		| {sftpd_vsn, integer()}.
-type options() :: [option()].
-type cgf_state() :: #{
		user => Username :: string(),
		write_handles := ordsets:set(Handle :: non_neg_integer())}.
-type sftpd_state() :: #state{}.

%%----------------------------------------------------------------------
%%  The cgf_sftpd public API
%%----------------------------------------------------------------------

-spec subsystem_spec(Options) -> Spec
	when
		Options :: options(),
		CbMod :: ?MODULE,
		Spec :: {Name, {CbMod, Options}},
		Name :: string().
%% @doc Returns an `ssh' subsystem specification for `sftpd'.
subsystem_spec(Options) ->
	case proplists:lookup(root, Options) of
		{root, Root} when is_list(Root), length(Root) > 0 ->
			subsystem_spec1(ssh_sftpd:subsystem_spec(Options));
		_ ->
			{stop, bad_root}
	end.
%% @hidden
subsystem_spec1({Name, {ssh_sftpd, Options}}) ->
	{Name, {?MODULE, Options}}.

%%----------------------------------------------------------------------
%%  ssh_server_channel callbacks
%%----------------------------------------------------------------------

-spec init(Options) -> Result
	when
		Options :: options(),
		Result :: {ok, State} | {ok, State, timeout()} | {stop, Reason},
		State :: {CgfState, SftpdState},
		CgfState :: cgf_state(),
		SftpdState :: sftpd_state(),
		Reason :: term().
%% Initialize an `sftpd' server channel.
%% @private
init(Options) ->
	CgfState = #{write_handles => ordsets:new()},
	case ssh_sftpd:init(Options) of
		{ok, SftpdState} ->
			{ok, {CgfState, SftpdState}};
		{ok, SftpdState, Timeout} ->
			{pk, {CgfState, SftpdState}, Timeout};
		{stop, Reason} ->
			{stop, Reason}
	end.

-spec handle_msg(Msg, State) -> Result
	when
		Msg :: timeout | term(),
		Result :: {ok, State} | {stop, ChannelId, State},
		ChannelId :: ssh:channel_id(),
		State :: {CgfState, SftpdState},
		CgfState :: cgf_state(),
		SftpdState :: sftpd_state().
%% @doc Handle messages other than SSH Connection Protocol,
%% 	call, or cast messages sent to the channel.
%% @private
handle_msg({ssh_channel_up, _ChannelId, ConnectionRef} = Msg,
		{CgfState, #state{root = Root} = SftpdState} = _State) ->
	{user, Username} = ssh:connection_info(ConnectionRef, user),
	CgfState1 = CgfState#{user => Username},
	UserRoot = filename:join(Root, Username),
	SftpdState1 = SftpdState#state{root = UserRoot},
	case ssh_sftpd:handle_msg(Msg, SftpdState1) of
		{ok, SftpdState2} ->
			{ok, {CgfState1, SftpdState2}};
		{stop, ChannelId, SftpdState2} ->
			{stop, ChannelId, {CgfState1, SftpdState2}}
	end;
handle_msg(Msg, {CgfState, SftpdState} = _State) ->
	case ssh_sftpd:handle_msg(Msg, SftpdState) of
		{ok, SftpdState1} ->
			{ok, {CgfState, SftpdState1}};
		{stop, ChannelId, SftpdState1} ->
			{stop, ChannelId, {CgfState, SftpdState1}}
	end.

-spec handle_ssh_msg(Event, State) -> Result
	when
		Event :: timeout | ssh_connection:event(),
		Result :: {ok, State} | {stop, ChannelId, State},
		ChannelId :: ssh:channel_id(),
		State :: {CgfState, SftpdState},
		CgfState :: cgf_state(),
		SftpdState :: sftpd_state().
%% @doc Handles SSH Connection Protocol messages
%% 	that may need service-specific attention.
%% @private
handle_ssh_msg({ssh_cm, _ConnectionRef,
		{data, _ChannelId, Type, Data}} = Event, State) ->
	case handle_data(Type, Data, State) of
		{ok, {CgfState, SftpdState} = _State1} ->
			case ssh_sftpd:handle_ssh_msg(Event, SftpdState) of
				{ok, SftpdState1} ->
					{ok, {CgfState, SftpdState1}};
				{stop, ChannelId, SftpdState1} ->
					{stop, ChannelId, {CgfState, SftpdState1}}
			end;
		{prohibit, State1} ->
			{ok, State1}
	end;
handle_ssh_msg(Event,
		{CgfState, SftpdState} = _State) ->
	case ssh_sftpd:handle_ssh_msg(Event, SftpdState) of
		{ok, SftpdState1} ->
			{ok, {CgfState, SftpdState1}};
		{stop, ChannelId, SftpdState1} ->
			{stop, ChannelId, {CgfState, SftpdState1}}
	end.

-spec terminate(Reason, State) -> any()
	when
		Reason :: term(),
		State :: {CgfState, SftpdState},
		CgfState :: cgf_state(),
		SftpdState :: sftpd_state().
%% @doc Called by a channel process when it is about to terminate.
terminate(Reason, {_CgfState, SftpdState} = _State) ->
	ssh_sftpd:terminate(Reason, SftpdState).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
handle_data(0, <<?UINT32(Len), Msg:Len/binary, _/binary>>,
		{_CgfState, #state{pending = <<>>} = _SftpdState} = State) ->
	<<Op, ?UINT32(ReqId), Data/binary>> = Msg,
	handle_op(Op, ReqId, Data, State);
handle_data(Type, Data,
		{CgfState, #state{pending = Pending} = SftpdState} = _State) ->
	SftpdState1 = SftpdState#state{pending = <<>>},
	State1 = {CgfState, SftpdState1},
	handle_data(Type, <<Pending/binary, Data/binary>>, State1);
handle_data(_Type, _Data, State) ->
	{ok, State}.

%% @hidden
handle_op(?SSH_FXP_INIT, _Version, _Data, State) ->
	{ok, State};
handle_op(?SSH_FXP_REALPATH, _ReqId, _Data, State) ->
	{ok, State};
handle_op(?SSH_FXP_OPENDIR, _ReqId, _Data, State) ->
	{ok, State};
handle_op(?SSH_FXP_READDIR, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_CLOSE, _ReqId,
		<<?UINT32(HLen), BinHandle:HLen/binary>> = _Data,
		{#{write_handles := CgfHandles, user := Username} = CgfState,
		#state{root = Root, handles = SftpdHandles} = SftpdState} = State) ->
	Handle = binary_to_integer(BinHandle),
	case lists:keysearch(Handle, 1, SftpdHandles) of
		{value, {Handle, file, {Path, _IoDevice}}} ->
			case ordsets:is_element(Handle, CgfHandles) of
				true ->
					RealPath = Root ++ Path,
					EventPayload = #{user => Username, path => RealPath},
					cgf_event:notify(file_close, EventPayload),
					CgfHandles1 = ordsets:del_element(Handle, CgfHandles),
					CgfState1 = CgfState#{write_handles => CgfHandles1},
					State1 = {CgfState1, SftpdState},
					{ok, State1};
				false ->
					{ok, State}
			end;
		_ ->
			{ok, State}
	end;
handle_op(?SSH_FXP_LSTAT, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_STAT, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_FSTAT, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_OPEN, _ReqId, _Data, State) ->
	{ok, State};
handle_op(?SSH_FXP_READ, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_WRITE, _ReqId,
		<<?UINT32(HLen), BinHandle:HLen/binary, _/binary>> = _Data,
		{#{write_handles := Handles} = CgfState, SftpdState} = _State) ->
	Handle = binary_to_integer(BinHandle),
	Handles1 = ordsets:add_element(Handle, Handles),
	CgfState1 = CgfState#{write_handles => Handles1},
	State1 = {CgfState1, SftpdState},
	{ok, State1};
handle_op(?SSH_FXP_READLINK, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_SETSTAT, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_MKDIR, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_FSETSTAT, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_REMOVE, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_RMDIR, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_RENAME, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State};
handle_op(?SSH_FXP_SYMLINK, ReqId, _Data,
		{_CgfState, #state{xf = XF} = _SftpdState} = State) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	{prohibit, State}.

