%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Mock RTC implementation for use in tests.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @copyright 2010 Geoffrey Biggs
%% RT-Synthesis Research Group
%% Intelligent Systems Research Institute,
%% National Institute of Advanced Industrial Science and Technology (AIST),
%% Japan
%% All rights reserved.
%% Licensed under the Eclipse Public License -v 1.0 (EPL)
%% http://www.opensource.org/licenses/eclipse-1.0.txt
%% @version {@version}
%% @end
%%-----------------------------------------------------------------------------
-module(mock_rtc).
-behaviour(gen_fsm).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include_lib("orber/src/orber_iiop.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link_fsm/1, start_fsm/1, stop_fsm/1]).
% Other APIs
-export([next_event_to_error/1, cbs/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
% States
-export([normal/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the state machine.
%% @spec start_link_fsm(Type) -> {ok, pid()} | ignore | {error, Error}
%% where
%%      Type = atom()
%%          Type of RTC to emulate.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link_fsm(Type::atom()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link_fsm(Type) ->
    gen_fsm:start_link(mock_rtc, Type, []).


%%-----------------------------------------------------------------------------
%% @doc Start a state machine without linking.
%% @spec start_fsm(Type) -> {ok, pid()} | ignore | {error, Error}
%% where
%%      Type = atom()
%%          Type of RTC to emulate.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_fsm(Type::atom()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_fsm(Type) ->
    gen_fsm:start_link(mock_rtc, Type, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the state machine.
%% @spec stop_fsm(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(stop_fsm(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_fsm(FSM) ->
    gen_fsm:send_all_state_event(FSM, stop).


%%-----------------------------------------------------------------------------
%% @doc Make the next event return an error.
%% @spec next_event_to_error(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(next_event_to_error(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
next_event_to_error(FSM) ->
    gen_fsm:send_all_state_event(FSM, to_error).


%%-----------------------------------------------------------------------------
%% @doc Get and reset the current callback history.
%% @spec cbs(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(cbs(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
cbs(FSM) ->
    gen_fsm:sync_send_all_state_event(FSM, cbs).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise this state machine.
%% @spec init(Type) -> {ok, StateName, State}
%% where
%%      Type = atom()
%%          Type of RTC to emulate.
%%      StateName = atom()
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%          Whether the next event should transition to error.
%%      CBHistory = [atom()]
%%          A list of the callbacks recently "executed."
%% @end
-spec(init(Type::atom()) -> {ok, normal, {false, []}}).
%%-----------------------------------------------------------------------------
init(Type) ->
    ?LOG(rtl_info, "Mock RTC ~p initialised.", [self()]),
    {ok, normal, {false, [], Type}}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, StateName, State) -> Result
%% where
%%      Event = to_error | stop
%%      StateName = normal
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%%      Result = {next_state, NextStateName, NewState} |
%%          {stop, normal, State}
%%      NextStateName = normal
%%      NewState = {NewToError, CBHistory, Type}
%%      NewToError = boolean()
%% @end
-spec(handle_event(Event::to_error | stop, StateName::normal,
        State::{ToError::boolean(), CBHistory::[atom()]}) ->
    {next_state, NextStateName::normal,
        NewState::{NewToError::boolean(), CBHistory::[atom()]}} |
    {stop, normal, State::{ToError::boolean(), CBHistory::[atom()]}}).
%%-----------------------------------------------------------------------------
handle_event(to_error, normal, {_ToError, CBHistory, Type}) ->
    {next_state, normal, {true, CBHistory, Type}};
handle_event(stop, normal, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle a synchronous event.
%% @spec handle_sync_event(Event, From, StateName, State -> Result
%% where
%%      Event = cbs | get_type
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateName = normal
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%%      Result = {reply, Replay, NextStateName, NewState} |
%%      Reply = CBHistory | Type
%%      NextStateName = normal
%%      NewState = {ToError, [], Type} | {ToError, CBHistory, Type}
%% @end
-spec(handle_sync_event(Event::cbs, From::{pid(), Tag::any()},
        StateName::normal,
        State::{ToError::boolean(), CBHistory::[atom()], Type::atom()}) ->
    {reply, Reply::[atom()], NextStateName::normal,
        NewState::{ToError::boolean(), CBHistory::[atom()],
            Type::atom()}}).
%%-----------------------------------------------------------------------------
handle_sync_event(cbs, _From, normal, {ToError, CBHistory, Type}) ->
    {reply, CBHistory, normal, {ToError, [], Type}};
handle_sync_event(get_type, _From, normal, {ToError, CBHistory, Type}) ->
    {reply, Type, normal, {ToError, CBHistory, Type}};
handle_sync_event({attach_ec, EC}, _From, normal,
        {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling attach_context(~p).",
        [self(), EC]),
    {reply, make_ref(), normal, {ToError, [attach_context | CBHistory], Type}};
handle_sync_event({detach_ec, EC}, _From, normal,
        {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling detach_context(~p).",
        [self(), EC]),
    {reply, make_ref(), normal, {ToError, [detach_context | CBHistory], Type}};
handle_sync_event(get_corba_obj, _From, normal, State) ->
    {reply, ?ORBER_NIL_OBJREF, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle an info message.
%% @spec handle_info(Info, StateName, State) -> Result
%% where
%%      Info = any()
%%      StateName = atom()
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%%      Result = {next_state, NextStateName, NewState}
%%      Reply = any()
%%      NextStateName = atom()
%%      NewState = {ToError, CBHistory, Type}
%% @end
-spec(handle_info(Info::any(), StateName::atom(), State::any()) ->
    {next_state, NextStateName::atom(), NewState::any()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, _StateName, State) ->
    {next_state, none, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, StateName, State) -> any()
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      StateName = atom()
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        StateName::atom(), State::any()) -> any()).
%%-----------------------------------------------------------------------------
terminate(normal, _StateName, _State) ->
    ?LOG(rtl_info, "Mock RTC ~p shutting down normally.", [self()]);
terminate(shutdown, _StateName, _State) ->
    ?LOG(rtl_info, "Mock RTC ~p shut down by supervisor.", [self()]);
terminate({shutdown, Reason}, _StateName, _State) ->
    ?LOG(rtl_info, "Mock RTC ~p shutting down: ~p", [self(), Reason]);
terminate(Reason, _StateName, _State) ->
    ?LOG(rtl_info, "Mock RTC ~p unusual shutdown: ~p", [self(), Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%      {ok, NextStateName, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      StateName = atom()
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%%      Extra = any()
%%      NextStateName = atom()
%%      NewState = {ToError, CBHistory, Type}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, StateName::atom(),
        State::any(), Extra::any()) ->
    {ok, NextStateName::atom(), NewState::any()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Normal state synchronous event handler.
%% @spec normal(Event, From, State) -> Result
%% where
%%      Event = activate | deactivate | execute | state_update | aborting |
%%          error | reset
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {ToError, CBHistory, Type}
%%      ToError = boolean()
%%      CBHistory = [atom()]
%%      Type = atom()
%%      Result = {reply, ok | error, NextStateName, NewState}
%%      NextStateName = normal
%%      NewState = {ToError, CBHistory, Type}
%% @end
-spec(normal(Event::activate | deactivate | execute | state_update | aborting |
            error | reset,
        From::{pid(), Tag::any()},
        State::{ToError::boolean(), CBHistory::[atom()]}) ->
    {next_state, NextStateName::normal,
        NewState::{ToError::boolean(), CBHistory::atom()}}).
%%-----------------------------------------------------------------------------
normal({on_startup, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling startup in EC ~p.", [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_startup | CBHistory], Type}};
normal({on_shutdown, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling shutdown in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_shutdown | CBHistory], Type}};
normal({on_execute, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling on_execute in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_execute | CBHistory], Type}};
normal({on_state_update, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling on_state_update in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_state_update | CBHistory], Type}};
normal({on_rate_changed, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling on_rate_changed in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_rate_changed | CBHistory], Type}};
normal({on_activated, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling activate in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_activated | CBHistory], Type}};
normal({on_deactivated, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling deactivate in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_deactivated | CBHistory], Type}};
normal({on_aborting, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling aborting in EC ~p.",
        [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_aborting | CBHistory], Type}};
normal({on_error, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling error in EC ~p.", [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_error | CBHistory], Type}};
normal({on_reset, EC}, _From, {ToError, CBHistory, Type}) ->
    ?LOG(rtl_paranoid, "Mock RTC ~p handling reset in EC ~p.", [self(), EC]),
    {reply, return_code(ToError), normal,
        {false, [on_reset | CBHistory], Type}}.


%%-----------------------------------------------------------------------------
%% @doc Produce a return code based on a boolean value. True for error.
%% @spec return_code(boolean()) -> ok | error
%% @end
-spec(return_code(boolean()) -> ok | errror).
%%-----------------------------------------------------------------------------
return_code(false) ->
    ok;
return_code(true) ->
    error.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test recording some callbacks.
%% @spec cb_recording_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(cb_recording_test_() ->
        {setup, fun(() -> {any(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
cb_recording_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_fsm(dataflow),
            Pid
        end,
        fun(Pid) ->
            ok = stop_fsm(Pid)
        end,
        fun(Pid) ->
            [?_assertMatch(ok, gen_fsm:sync_send_event(Pid, activate)),
                ?_assertMatch(ok, gen_fsm:sync_send_event(Pid, execute)),
                ?_assertMatch(ok, gen_fsm:sync_send_event(Pid, state_update)),
                ?_assertMatch(ok, next_event_to_error(Pid)),
                ?_assertMatch(error, gen_fsm:sync_send_event(Pid, deactivate)),
                ?_assertMatch(ok, gen_fsm:sync_send_event(Pid, aborting)),
                ?_assertMatch(ok, gen_fsm:sync_send_event(Pid, error)),
                ?_assertMatch(ok, gen_fsm:sync_send_event(Pid, reset)),
                ?_assertMatch([on_reset, on_error, on_aborting, on_deactivated,
                        on_state_update, on_execute, on_activated], cbs(Pid))]
        end
    }.

-endif. % TEST

