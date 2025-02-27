%%% cgf_test_event.erl
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
%%% @doc This {@link //stdlib/gen_event. gen_event} behaviour callback
%%% 	module implements an event handler in test suites of the
%%% 	{@link //cgf. cgf} application.
%%%
-module(cgf_test_event).
-copyright('Copyright (c) 2025 SigScale Global Inc.').

-behaviour(gen_event).

%% export the cgf_test_event API
-export([add_handler/1, get_event/0, get_event/1]).

%% export the callbacks needed for gen_event behaviour
-export([init/1, handle_call/2, handle_event/2, handle_info/2,
			terminate/2]).

-record(state, {test_case :: pid()}).
-type state() :: #state{}.

%%----------------------------------------------------------------------
%%  The cgf_test_event API
%%----------------------------------------------------------------------

-spec add_handler(TestCase) -> Result
	when
		TestCase :: pid(),
		Result :: gen_event:add_handler_ret().
%% @doc Add an event handler for a test case.
add_handler(TestCase) when is_pid(TestCase) ->
	gen_event:add_handler(cgf_event, {?MODULE, TestCase}, [TestCase]).

-spec get_event() -> Event
	when
		Event :: cgf_event:file_close().
%% @doc Receive an event.
get_event() ->
	get_event(5000).

-spec get_event(Timeout) -> Event
	when
		Timeout :: timeout(),
		Event :: cgf_event:file_close().
%% @doc Receive an event.
get_event(Timeout) ->
	receive
		{?MODULE, Event} ->
			Event
	after
		Timeout ->
			ct:fail(no_event)
	end.

%%----------------------------------------------------------------------
%%  The cgf_test_event gen_event callbacks
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
init([TestCase] = _Args) when is_pid(TestCase) ->
	{ok, #state{test_case = TestCase}}.

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
handle_event(Event, #state{test_case = TestCase} = State) ->
	TestCase ! {?MODULE, Event},
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

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

