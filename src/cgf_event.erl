%%% cgf_event.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 - 2025 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_event. gen_event} behaviour callback
%%% 	module implements an event handler of the
%%% 	{@link //cgf. cgf} application.
%%%
-module(cgf_event).
-copyright('Copyright (c) 2024 - 2025 SigScale Global Inc.').

-behaviour(gen_event).

%% export the cgf_event API
-export([add_handler/2, add_sup_handler/2]).
-export([notify/2]).

%% export the callbacks needed for gen_event behaviour
-export([init/1, handle_call/2, handle_event/2, handle_info/2,
			terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-export_type([file_close/0]).

-type stack() :: [{Event :: cgf:event(),
		{Match :: cgf:match(), Action :: cgf:action()}}].
-type file_close() ::
		#{module := Module :: atom(),
		user := User :: binary(),
		root := Root :: binary(),
		path := Path :: binary(),
		stack := Stack :: stack()}.

-record(state,
		{max_stack :: pos_integer()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The cgf_event API
%%----------------------------------------------------------------------

-spec add_handler(Handler, Args) -> Result
	when
		Handler :: gen_event:handler(),
		Args :: list(),
		Result :: gen_event:add_handler_ret().
%% @doc Add a new event handler.
add_handler(Handler, Args) ->
	gen_event:add_handler(?MODULE, Handler, Args).

-spec add_sup_handler(Handler, Args) -> Result
	when
		Handler :: gen_event:handler(),
		Args :: list(),
		Result :: gen_event:add_handler_ret().
%% @doc Add a new supervised event handler.
add_sup_handler(Handler, Args) ->
	gen_event:add_sup_handler(?MODULE, Handler, Args).

-spec notify(EventType, EventPayLoad) -> ok
	when
		EventType :: file_close,
		EventPayLoad :: term().
%% @doc Send a notification event.
notify(EventType, EventPayLoad) ->
	catch gen_event:notify(?MODULE, {EventType, EventPayLoad}),
	ok.

%%----------------------------------------------------------------------
%%  The cgf_event gen_event callbacks
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, State} | {ok, State, hibernate} | {error, State},
		State :: state().
%% @doc Initialize the {@module} server.
%% @see //stdlib/gen_event:init/1
%% @private
%%
init([] = _Args) ->
	{ok, MaxStack} = application:get_env(max_action),
	{ok, #state{max_stack = MaxStack}}.

-spec handle_event(Event, State) -> Result
	when
		Event :: term(),
		State :: state(),
		Result :: {ok, NewState}
				| {ok, NewState, hibernate}
				| {swap_handler, Args1, NewState, Handler2, Args2}
				| remove_handler,
		NewState :: state(),
		Args1 :: term(),
		Args2 :: term(),
		Handler2 :: Module2 | {Module2, Id},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_event:handle_event/2.
%% 	gen_event:notify/2, gen_event:sync_notify/2}.
%% @private
%%
handle_event({_, #{stack := Stack}} = Event,
		#state{max_stack = MaxStack} = State)
		when length(Stack) >= MaxStack ->
	?LOG_ERROR([{?MODULE, max_action}, {event, Event}]),
	{ok, State};
handle_event({file_close, #{}} = Event, State) ->
	gen_server:call(cgf_event_server, Event),
	{ok, State};
handle_event(Event, State) ->
	?LOG_WARNING([{?MODULE, unknown_event}, {event, Event}]),
	{ok, State}.

-spec handle_call(Request, State) -> Result
	when
		Request :: term(),
		State :: state(),
		Result :: {ok, Reply :: term(), State :: state()}
			| {ok, Reply :: term(), State :: state(), hibernate}
			| {swap_handler, Reply :: term(), Args1 :: term(), State :: state(),
				Handler2 :: Module2 | {Module2, Id}, Args2 :: term()}
			| {remove_handler, Reply :: term()},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a request sent using {@link //stdlib/gen_event:call/3.
%% 	gen_event:call/3,4}.
%% @see //stdlib/gen_event:handle_call/3
%% @private
%%
handle_call(_Request, State) ->
	{ok, not_implemented, State}.

-spec handle_info(Info, State) -> Result
	when
		Info :: term(),
		State :: state(),
		Result :: {ok, NewState :: term()}
			| {ok, NewState :: term(), hibernate}
			| {swap_handler, Args1 :: term(), NewState :: term(),
			Handler2, Args2 :: term()} | remove_handler,
		Handler2 :: Module2 | {Module2, Id},
		Module2 :: atom(),
		Id :: term().
%% @doc Handle a received message.
%% @see //stdlib/gen_event:handle_info/2
%% @private
%%
handle_info(_Info, State) ->
	{ok, State}.

-spec terminate(Arg, State) -> term()
	when
		Arg :: Args :: term() | {stop, Reson :: term()} | {error, term()}
				| stop | remove_handler | {error,{'EXIT', Reason :: term()}},
		State :: state().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_event:terminate/3
%% @private
%%
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn, State, Extra) -> Result
	when
		OldVsn :: term() | {down, term()},
		State :: term(),
		Extra :: term(),
		Result :: {ok, NewState :: term()}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_event:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

