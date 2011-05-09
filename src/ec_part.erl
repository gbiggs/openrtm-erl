%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc EC participant state machine.
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
-module(ec_part).
-behaviour(gen_fsm).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").


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
-export([start_link_fsm/2, start_fsm/2, stop_fsm/1]).
% Operation events
-export([activate/1, deactivate/1, reset/1, tick/1, end_of_tick/1,
        go_to_error/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
% Introspection for tests
-export([state/1, sync/1]).
% States
-export([inactive/3, active/3, error/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the state machine.
%% @spec start_link_fsm(RTC, Handle) -> {ok, pid()} | ignore | {error, Error}
%% where
%%      RTC = pid() | object_ref()
%%          The RTC this participant FSM is managing.
%%      Handle = ec_handle()
%%          The handle of this EC in the RTC.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link_fsm(RTC::pid() | object_ref(), Handle::ec_handle()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link_fsm(RTC, Handle) ->
    gen_fsm:start_link(ec_part, {RTC, Handle}, []).


%%-----------------------------------------------------------------------------
%% @doc Start a state machine without linking.
%% @spec start_fsm(RTC, Handle) -> {ok, pid()} | ignore | {error, Error}
%% where
%%      RTC = pid() | object_ref()
%%          The RTC this participant FSM is managing.
%%      Handle = ec_handle()
%%          The handle of this EC in the RTC.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_fsm(RTC::pid() | object_ref(), Handle::ec_handle()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_fsm(RTC, Handle) ->
    gen_fsm:start(ec_part, {RTC, Handle}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the state machine.
%% @spec stop_fsm(ECPart) -> ok
%%      ECPart = pid()
%% @end
-spec(stop_fsm(ECPart::pid() | object_ref()) -> ok).
%%-----------------------------------------------------------------------------
stop_fsm(ECPart) ->
    gen_fsm:send_all_state_event(ECPart, stop).


%%-----------------------------------------------------------------------------
%% @doc Send an activate event to shift the lifecycle of the EC participant to
%% active.
%% @spec activate(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(activate(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
activate(ECPart) ->
    gen_fsm:sync_send_event(ECPart, activate).


%%-----------------------------------------------------------------------------
%% @doc Send a deactivate event to shift the lifecycle of the EC participant to
%% inactive.
%% @spec deactivate(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(deactivate(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
deactivate(ECPart) ->
    gen_fsm:sync_send_event(ECPart, deactivate).


%%-----------------------------------------------------------------------------
%% @doc Send a reset event to shift the lifecycle of the EC participant to
%% inactive from error.
%% @spec reset(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(reset(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
reset(ECPart) ->
    gen_fsm:sync_send_event(ECPart, reset).


%%-----------------------------------------------------------------------------
%% @doc Send a tick event to trigger the execution stage of the EC participant.
%% @spec tick(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(tick(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
tick(ECPart) ->
    gen_fsm:sync_send_event(ECPart, tick).


%%-----------------------------------------------------------------------------
%% @doc Send an end-of-tick event to trigger the execution stage of the EC
%% participant.
%% @spec end_of_tick(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(end_of_tick(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
end_of_tick(ECPart) ->
    gen_fsm:sync_send_event(ECPart, end_of_tick).


%%-----------------------------------------------------------------------------
%% @doc Immediately shift the EC participant to the error state. For internal
%% use only.
%% @spec go_to_error(ECPart) -> ok
%% where
%%      ECPart = pid()
%% @end
-spec(go_to_error(ECPart::pid()) -> ok).
%%-----------------------------------------------------------------------------
go_to_error(ECPart) ->
    gen_fsm:sync_send_event(ECPart, go_to_error).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise this state machine.
%% @spec init({RTC, Handle}) -> {ok, StateName, State}
%% where
%%      RTC = pid() | object_ref()
%%          The PID or reference of the RTC being managed.
%%      Handle = ec_handle()
%%          The handle of this EC in the RTC.
%%      StateName = inactive
%%      State = {pid(), ec_handle()}
%% @end
-spec(init(RTC::pid() | object_ref()) ->
    {ok, StateName::inactive, State::{pid(), ec_handle()}}).
%%-----------------------------------------------------------------------------
init({RTC, Handle}) ->
    {ok, inactive, {RTC, Handle}}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, StateName, State) -> Result
%% where
%%      Event = stop
%%      StateName = atom()
%%      State = {pid() | object_ref(), ec_handle()}
%%      Result = {stop, normal, State}
%% @end
-spec(handle_event(Event::stop, StateName::atom(), State::pid()) ->
    {stop, normal, State::{pid() | object_ref(), ec_handle()}}).
%%-----------------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    ?LOG(rtl_info, "EC participant for RTC ~p stopping.", [State]),
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle a synchronous event.
%% @spec handle_sync_event(Event, From, StateName, State -> Result
%% where
%%      Event = sync
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateName = atom()
%%      State = {pid(), ec_handle()}
%%      Result = {reply, ok, StateName, State}
%%      Reply = ok
%% @end
-spec(handle_sync_event(Event::sync, From::{pid(), Tag::any()},
        StateName::atom(), State::{pid(), ec_handle()}) ->
    {reply, Reply::ok, StateName::atom(), State::pid()}).
%%-----------------------------------------------------------------------------
handle_sync_event(sync, _From, StateName, State) ->
    {reply, ok, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle an info message.
%% @spec handle_info(Info, StateName, State) -> Result
%% where
%%      Info = any()
%%      StateName = atom()
%%      State = {pid(), ec_handle()}
%%      Result = {next_state, NextStateName, NewState}
%%      Reply = any()
%%      NextStateName = atom()
%%      NewState = any()
%% @end
-spec(handle_info(Info::any(), StateName::atom(),
        State::{pid(), ec_handle()}) ->
    {next_state, NextStateName::atom(), NewState::{pid(), ec_handle()}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, _StateName, State) ->
    {next_state, none, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, StateName, State) -> any()
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      StateName = atom()
%%      State = {pid(), ec_handle()}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        StateName::atom(), State::{pid(), ec_handle()}) -> any()).
%%-----------------------------------------------------------------------------
terminate(normal, _StateName, State) ->
    ?LOG(rtl_info, "EC participant for RTC ~p shutting down normally.",
        [State]);
terminate(shutdown, _StateName, State) ->
    ?LOG(rtl_info, "EC participant for RTC ~p shut down by supervisor.",
        [State]);
terminate({shutdown, Reason}, _StateName, State) ->
    ?LOG(rtl_info, "EC participant for RTC ~p shutting down: ~p",
        [State, Reason]);
terminate(Reason, _StateName, State) ->
    ?LOG(rtl_info, "EC participant for RTC ~p unusual shutdown: ~p",
        [State, Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%      {ok, NextStateName, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      StateName = atom()
%%      State = {pid() | object_ref(), ec_handle()}
%%      Extra = any()
%%      NextStateName = atom()
%%      NewState = {pid() | object_ref(), ec_handle()}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, StateName::atom(),
        State::{pid() | object_ref(), ec_handle()}, Extra::any()) ->
    {ok, NextStateName::atom(), NewState::{pid() | object_ref(), ec_handle()}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Inactive state synchronous event handler.
%% @spec inactive(Event, From, State) -> Result
%% where
%%      Event = activate | tick | end_of_tick | go_to_error | state
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {pid(), ec_handle()}
%%      Result = {reply, Reply, active | error | inactive, RTC}
%%      Reply = ok | error | inactive
%%      RTC = pid() | object_ref()
%% @end
-spec(inactive(Event::activate | tick | end_of_tick | go_to_error | state,
        From::{pid(), Tag::any()}, State::{pid(), ec_handle()}) ->
    {reply, ok | error | inactive,
        NextStateName::active | error | inactive, RTC::pid() | object_ref()}).
%%-----------------------------------------------------------------------------
inactive(activate, _From, {RTC, Handle}=State) ->
    case call_on_activated(RTC, Handle)
        of ok ->
            {reply, ok, active, State}
         ; error ->
            call_on_aborting(RTC, Handle),
            {reply, error, error, State}
    end;
inactive(tick, _From, State) ->
    {reply, ok, inactive, State};
inactive(end_of_tick, _From, State) ->
    {reply, ok, inactive, State};
inactive(go_to_error, _From, State) ->
    {reply, ok, error, State};
inactive(state, _From, State) ->
    {reply, inactive, inactive, State}.


%%-----------------------------------------------------------------------------
%% @doc Active state synchronous event handler.
%% @spec active(Event, From, State) -> Result
%% where
%%      Event = deactivate | tick | end_of_tick | go_to_error | state
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {pid(), ec_handle()}
%%      Result = {reply, Reply, inactive | active | error, RTC}
%%      Reply = ok | error | active
%%      RTC = pid() | object_ref()
%% @end
-spec(active(Event::deactivate | tick | end_of_tick | go_to_error | state,
        From::{pid(), Tag::any()}, State::{pid(), ec_handle()}) ->
    {reply, ok | error | active,
        NextStateName::inactive | active | error, RTC::pid() | object_ref()}).
%%-----------------------------------------------------------------------------
active(deactivate, _From, {RTC, Handle}=State) ->
    case call_on_deactivated(RTC, Handle)
        of ok ->
            {reply, ok, inactive, State}
         ; error ->
            call_on_aborting(RTC, Handle),
            {reply, error, error, State}
    end;
active(tick, _From, {RTC, Handle}=State) ->
    case call_on_execute(RTC, Handle)
        of ok ->
            {reply, ok, active, State}
         ; error ->
            call_on_aborting(RTC, Handle),
            {reply, error, error, State}
    end;
active(end_of_tick, _From, {RTC, Handle}=State) ->
    case call_on_state_update(RTC, Handle)
        of ok ->
            {reply, ok, active, State}
         ; error ->
            call_on_aborting(RTC, Handle),
            {reply, error, error, State}
    end;
active(go_to_error, _From, State) ->
    {reply, ok, error, State};
active(state, _From, State) ->
    {reply, active, active, State}.


%%-----------------------------------------------------------------------------
%% @doc Error state synchronous event handler.
%% @spec error(Event, From, State) -> Result
%% where
%%      Event = reset | tick | end_of_tick | go_to_error | state
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {pid(), ec_handle()}
%%      Result = {reply, error, error, RTC}
%%      Reply = ok | error
%%      RTC = pid() | object_ref()
%% @end
-spec(error(Event::reset | tick | end_of_tick | go_to_error | state,
        From::{pid(), Tag::any()}, State::{pid(), ec_handle()}) ->
    {reply, ok | error, NextStateName::inactive | error,
        RTC::pid() | object_ref()}).
%%-----------------------------------------------------------------------------
error(reset, _From, {RTC, Handle}=State) ->
    case call_on_reset(RTC, Handle)
        of ok ->
            {reply, ok, inactive, State}
         ; error ->
            {reply, error, error, State}
    end;
error(tick, _From, {RTC, Handle}=State) ->
    call_on_error(RTC, Handle),
    {reply, ok, error, State};
error(end_of_tick, _From, State) ->
    {reply, ok, error, State};
error(go_to_error, _From, State) ->
    {reply, ok, error, State};
error(state, _From, State) ->
    {reply, error, error, State}.


%%-----------------------------------------------------------------------------
%% @doc Call on_activated on a local or CORBA reference.
%% @spec call_on_activated(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_activated(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_activated(RTC, Handle) when is_pid(RTC) ->
    rtc:on_activated(RTC, Handle);
call_on_activated({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_activated(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_deactivated on a local or CORBA reference.
%% @spec call_on_deactivated(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_deactivated(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_deactivated(RTC, Handle) when is_pid(RTC) ->
    ?LOG(rtl_paranoid, "Calling on_deactivated on PID ~p.", [RTC]),
    rtc:on_deactivated(RTC, Handle);
call_on_deactivated({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC,
        Handle) ->
    ?LOG(rtl_paranoid, "Calling on_deactivated on CORBA object."),
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_deactivated(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_execute on a local or CORBA reference.
%% @spec call_on_execute(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_execute(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_execute(RTC, Handle) when is_pid(RTC) ->
    rtc:on_execute(RTC, Handle);
call_on_execute({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_execute(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_state_update on a local or CORBA reference.
%% @spec call_on_state_update(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_state_update(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_state_update(RTC, Handle) when is_pid(RTC) ->
    rtc:on_state_update(RTC, Handle);
call_on_state_update({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC,
        Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_state_update(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_aborting on a local or CORBA reference.
%% @spec call_on_aborting(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_aborting(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_aborting(RTC, Handle) when is_pid(RTC) ->
    rtc:on_aborting(RTC, Handle);
call_on_aborting({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_aborting(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_error on a local or CORBA reference.
%% @spec call_on_error(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_error(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_error(RTC, Handle) when is_pid(RTC) ->
    rtc:on_error(RTC, Handle);
call_on_error({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_error(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Call on_reset on a local or CORBA reference.
%% @spec call_on_reset(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_reset(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_reset(RTC, Handle) when is_pid(RTC) ->
    rtc:on_reset(RTC, Handle);
call_on_reset({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_reset(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Get the current state.
%% @spec state(FSM) -> atom()
%% where
%%      FSM = pid()
%% @end
-spec(state(FSM::pid()) -> atom()).
%%-----------------------------------------------------------------------------
state(FSM) ->
    gen_fsm:sync_send_event(FSM, state).


%%-----------------------------------------------------------------------------
%% @doc Used during testing to synchronise the test process with the FSM.
%% @spec sync(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(sync(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
sync(FSM) ->
    gen_fsm:sync_send_all_state_event(FSM, sync).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test starting and stopping an EC participant.
%% @spec start_and_stop_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(start_and_stop_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
start_and_stop_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            RTC
        end,
        fun(RTC) ->
            mock_rtc:stop_fsm(RTC)
        end,
        fun(RTC) ->
            ?LET({ok, Pid}, start_fsm(RTC, none),
                    [?_assertMatch(true, is_process_alive(Pid)),
                        ?_assertMatch(ok, stop_fsm(Pid)),
                        ?_test(timer:sleep(100)),
                        ?_assertMatch(false, is_process_alive(Pid))])
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test shifting a participant from inactive to active.
%% @spec inactive_to_active_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(inactive_to_active_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
inactive_to_active_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(inactive, state(Pid)),
                ?_assertMatch(ok, activate(Pid)),
                ?_assertMatch(active, state(Pid)),
                ?_assertMatch([on_activated], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test shifting a participant from active to inactive.
%% @spec active_to_inactive_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(active_to_inactive_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
active_to_inactive_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            ok = activate(Pid),
            sync(Pid), % Ensure the activation event gets through
            mock_rtc:cbs(RTC), % Clear the history
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(active, state(Pid)),
                ?_assertMatch(ok, deactivate(Pid)),
                ?_assertMatch(inactive, state(Pid)),
                ?_assertMatch([on_deactivated], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test an RTC failing to activate.
%% @spec activate_fail_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(activate_fail_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
activate_fail_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            mock_rtc:next_event_to_error(RTC),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(inactive, state(Pid)),
                ?_assertMatch(error, activate(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_activated], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test shifting a participant from error to inactive.
%% @spec error_to_inactive_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(error_to_inactive_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
error_to_inactive_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            mock_rtc:next_event_to_error(RTC),
            error = activate(Pid),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(error, state(Pid)),
                ?_assertMatch(ok, reset(Pid)),
                ?_assertMatch(inactive, state(Pid)),
                ?_assertMatch([on_reset, on_aborting, on_activated],
                    mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test an RTC failing to deactivate.
%% @spec deactivate_fail_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(deactivate_fail_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
deactivate_fail_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            ok = activate(Pid),
            sync(Pid), % Ensure the activation event gets through
            mock_rtc:cbs(RTC), % Clear the history
            mock_rtc:next_event_to_error(RTC),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(active, state(Pid)),
                ?_assertMatch(error, deactivate(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_deactivated],
                    mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test an RTC failing to reset.
%% @spec reset_fail_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(reset_fail_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
reset_fail_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            mock_rtc:next_event_to_error(RTC),
            error = activate(Pid),
            sync(Pid), % Ensure the activation event gets through
            mock_rtc:cbs(RTC), % Clear the history
            mock_rtc:next_event_to_error(RTC),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(error, state(Pid)),
                ?_assertMatch(error, reset(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_reset], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test an RTC failing first-stage execution.
%% @spec fs_execute_fail_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(fs_execute_fail_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
fs_execute_fail_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            ok = activate(Pid),
            sync(Pid), % Ensure the activation event gets through
            mock_rtc:cbs(RTC), % Clear the history
            mock_rtc:next_event_to_error(RTC),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(active, state(Pid)),
                ?_assertMatch(error, tick(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_execute], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test an RTC failing second-stage execution.
%% @spec ss_execute_fail_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(ss_execute_fail_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
ss_execute_fail_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            ok = activate(Pid),
            sync(Pid), % Ensure the activation event gets through
            mock_rtc:cbs(RTC), % Clear the history
            mock_rtc:next_event_to_error(RTC),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(active, state(Pid)),
                ?_assertMatch(error, end_of_tick(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_state_update],
                    mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test first-stage execution.
%% @spec fs_execute_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(fs_execute_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
fs_execute_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(inactive, state(Pid)),
                ?_assertMatch(ok, tick(Pid)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                % Active state
                ?_assertMatch(ok, activate(Pid)),
                ?_assertMatch(active, state(Pid)),
                ?_assertMatch([on_activated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, tick(Pid)),
                ?_assertMatch([on_execute], mock_rtc:cbs(RTC)),
                % Inactive state
                ?_assertMatch(ok, deactivate(Pid)),
                ?_assertMatch(inactive, state(Pid)),
                ?_assertMatch([on_deactivated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, tick(Pid)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                % Error state
                ?_test(mock_rtc:next_event_to_error(RTC)),
                ?_assertMatch(error, activate(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_activated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, tick(Pid)),
                ?_assertMatch([on_error], mock_rtc:cbs(RTC))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test second-stage execution.
%% @spec ss_execute_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(ss_execute_test_() ->
        {setup, fun(() -> {log_level(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
ss_execute_test_() ->
    {setup,
        fun() ->
            {ok, RTC} = mock_rtc:start_link_fsm(dataflow),
            {ok, Pid} = start_fsm(RTC, none),
            {Pid, RTC}
        end,
        fun({Pid, RTC}) ->
            stop_fsm(Pid),
            mock_rtc:stop_fsm(RTC)
        end,
        fun({Pid, RTC}) ->
            [?_assertMatch(inactive, state(Pid)),
                ?_assertMatch(ok, end_of_tick(Pid)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                % Active state
                ?_assertMatch(ok, activate(Pid)),
                ?_assertMatch(active, state(Pid)),
                ?_assertMatch([on_activated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, end_of_tick(Pid)),
                ?_assertMatch([on_state_update], mock_rtc:cbs(RTC)),
                % Inactive state
                ?_assertMatch(ok, deactivate(Pid)),
                ?_assertMatch(inactive, state(Pid)),
                ?_assertMatch([on_deactivated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, end_of_tick(Pid)),
                ?_assertMatch([], mock_rtc:cbs(RTC)),
                % Error state
                ?_test(mock_rtc:next_event_to_error(RTC)),
                ?_assertMatch(error, activate(Pid)),
                ?_assertMatch(error, state(Pid)),
                ?_assertMatch([on_aborting, on_activated], mock_rtc:cbs(RTC)),
                ?_assertMatch(ok, end_of_tick(Pid)),
                ?_assertMatch([], mock_rtc:cbs(RTC))]
        end
    }.

-endif. % TEST

