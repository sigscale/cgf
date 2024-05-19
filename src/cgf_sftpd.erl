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
-type cgf_state() :: #{user => Username :: string()}.
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
	case ssh_sftpd:init(Options) of
		{ok, SftpdState} ->
			{ok, {#{}, SftpdState}};
		{ok, SftpdState, Timeout} ->
			{pk, {#{}, SftpdState}, Timeout};
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
		{data, _ChannelId, Type, Data}} = Event,
		{CgfState, SftpdState} = State) ->
	case handle_data(Type, Data, SftpdState) of
		true ->
			case ssh_sftpd:handle_ssh_msg(Event, SftpdState) of
				{ok, SftpdState1} ->
					{ok, {CgfState, SftpdState1}};
				{stop, ChannelId, SftpdState1} ->
					{stop, ChannelId, {CgfState, SftpdState1}}
			end;
		false ->
			{ok, State}
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
		#state{pending = <<>>} = SftpdState) ->
	<<Op, ?UINT32(ReqId), Data/binary>> = Msg,
	handle_op(Op, ReqId, Data, SftpdState);
handle_data(Type, Data, #state{pending = Pending} = SftpdState) ->
	handle_data(Type, <<Pending/binary, Data/binary>>, SftpdState);
handle_data(_Type, _Data, _SftpdState) ->
	true.

%% @hidden
handle_op(?SSH_FXP_INIT, Version, _, _SftpdState) ->
	true;
handle_op(?SSH_FXP_REALPATH, ReqId, <<?UINT32(Rlen), RPath:Rlen/binary>>, _SftpdState) ->
	true;
handle_op(?SSH_FXP_OPENDIR, ReqId, _Data, _SftpdState) ->
	true;
handle_op(?SSH_FXP_READDIR, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_CLOSE, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary>>, _SftpdState) ->
	true;
handle_op(?SSH_FXP_LSTAT, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_STAT, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_FSTAT, ReqId, _Data, _SftpdState) ->
	true;
handle_op(?SSH_FXP_OPEN, ReqId, _Data, _SftpdState) ->
	true;
handle_op(?SSH_FXP_READ, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_WRITE, ReqId, <<?UINT32(HLen), BinHandle:HLen/binary, _/binary>>, _SftpdState) ->
	true;
handle_op(?SSH_FXP_READLINK, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_SETSTAT, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_MKDIR, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_FSETSTAT, ReqId, _Data, _SftpdState) ->
	true;
handle_op(?SSH_FXP_REMOVE, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_RMDIR, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_RENAME, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false;
handle_op(?SSH_FXP_SYMLINK, ReqId, _Data, #state{xf = XF} = _SftpdState) ->
	ssh_xfer:xf_send_status(XF, ReqId, ?SSH_FX_PERMISSION_DENIED, "Prohibited."),
	false.

