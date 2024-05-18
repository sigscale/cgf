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

-export_type([option/0, options/0]).
-type option() :: {cwd, string()}
		| {file_handler, CbMod :: atom() | {CbMod :: atom(), FileState :: term()}}
		| {max_files, integer()}
		| {root, string()}
		| {sftpd_vsn, integer()}.
-type options() :: [option()].

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
		State :: term(),
		Reason :: term().
%% Initialize an `sftpd' server channel.
%% @private
init(Options) ->
	ssh_sftpd:init(Options).

-spec handle_msg(Msg, State) -> Result
	when
		Msg :: timeout | term(),
		Result :: {ok, State} | {stop, ChannelId, State},
		ChannelId :: ssh:channel_id(),
		State :: term().
%% @doc Handle messages other than SSH Connection Protocol,
%% 	call, or cast messages sent to the channel.
%% @private
handle_msg({ssh_channel_up, _ChannelId, ConnectionRef} = Msg, State) ->
	{user, Username} = ssh:connection_info(ConnectionRef, user),
	State1 = update_state(Username, State),
	ssh_sftpd:handle_msg(Msg, State1);
handle_msg(Msg, State) ->
	ssh_sftpd:handle_msg(Msg, State).

-spec handle_ssh_msg(Msg, State) -> Result
	when
		Msg :: timeout | term(),
		Result :: {ok, State} | {stop, ChannelId, State},
		ChannelId :: ssh:channel_id(),
		State :: term().
%% @doc Handles SSH Connection Protocol messages
%% 	that may need service-specific attention.
%% @private
handle_ssh_msg(Msg, State) ->
	ssh_sftpd:handle_ssh_msg(Msg, State).

-spec terminate(Reason, State) -> any()
	when
		Reason :: term(),
		State :: term().
%% @doc Called by a channel process when it is about to terminate.
terminate(Reason, State) ->
	ssh_sftpd:terminate(Reason, State).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @doc Update the `ssh_sftpd' state record.
%% 	Hopefully `(#state.root == 4)' is true forever!
%% @hidden
update_state(Username, State) ->
	RootPath = filename:join(element(4, State), Username),
	setelement(4, State, RootPath).

