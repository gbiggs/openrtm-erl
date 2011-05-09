%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Periodic execution context.
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
-module(periodic_ec).
-behaviour(gen_fsm).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("ec.hrl").
-include("rtc.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% FSM control
-export([start_link_fsm/1, start_fsm/1, stop_fsm/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
% States
-export([stopped/2, stopped/3, started/2, started/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start a state machine.
%% @spec start_link_fsm(Config) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Config = config()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link_fsm(Config::config()) ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link_fsm(Config) ->
    gen_fsm:start_link(periodic_ec, Config, []).


%%-----------------------------------------------------------------------------
%% @doc Start a state machine without linking.
%% @spec start_fsm(Config) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Config = config()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_fsm(Config::config()) ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_fsm(Config) ->
    gen_fsm:start(periodic_ec, Config, []).


%%-----------------------------------------------------------------------------
%% @doc Stop a state machine.
%% @spec stop_fsm(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(stop_fsm(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_fsm(FSM) ->
    gen_fsm:send_all_state_event(FSM, terminate_fsm).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise this periodic execution context.
%% @spec init(Config) -> {ok, StateName, State}
%% where
%%      Config = config()
%%      StateName = atom()
%%      State = #periodic_ec{}
%% @end
-spec(init(Config::config()) ->
        {ok, StateName::atom(), State::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
init(Config) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_info, "Initialising periodic EC with config ~p",
        [Config]),
    % Calculate the period from the frequency.
    RateStr = case config:get_value("rate", Config)
        of undefined ->
            "1.0"
         ; Other ->
            Other
    end,
    Rate = utils:str_to_float(RateStr),
    P = calc_period(Rate),
    ?LOG(rtl_debug, "Periodic EC will sleep for ~pms", [P]),
    {ok, stopped,
        #periodic_ec{config=Config, rate=Rate, period=P}}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, StateName, State) -> Result
%% where
%%      Event = terminate_fsm
%%      StateName = atom()
%%      State = #periodic_ec{}
%%      Result = {stop, normal, State}
%% @end
-spec(handle_event(Event::terminate_fsm, StateName::atom(),
        State::#periodic_ec{}) ->
    {stop, normal, State::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
handle_event(terminate_fsm, _StateName, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle a synchronous event.
%% @spec handle_sync_event(Event, From, StateName, State) -> Result
%% where
%%      Event = set_owner | get_rate | {set_rate, float()} | {add_comp, RTC} |
%%          {rem_comp, RTC} | {act_comp, RTC} | {deact_comp, RTC} |
%%          {reset_comp, RTC} | {get_state, RTC} | get_kind | get_profile |
%%          {set_corba_obj, Obj} | get_corba_obj
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateName = atom()
%%      State = #periodic_ec{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = float() | ok | bad_param | precon_not_met | #ec_profile{} | Obj
%%      Obj = object_ref()
%%      NextStateName = atom()
%%      NewState = #periodic_ec{}
%% @end
-spec(handle_sync_event(Event::{set_owner, Owner::pid()} | get_rate |
            {set_rate, float()} | {add_comp, RTC} | {rem_comp, RTC} |
            {act_comp, RTC} | {deact_comp, RTC} | {reset_comp, RTC} |
            {get_state, RTC} | get_kind | get_profile |
            {set_corba_obj, Pid::pid(), Obj::object_ref()} | get_corba_obj,
        From::{pid(), Tag::any()},
        StateName::atom(), State::#periodic_ec{}) ->
    {reply, Reply::float() | ok | bad_param | precon_not_met | #ec_profile{} |
            object_ref(),
        NextStateName::atom(), NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
handle_sync_event({set_owner, Owner}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_owner request: ~p", [Owner]),
    {reply, ok, StateName, State#periodic_ec{owner=Owner}};
handle_sync_event(get_rate, _From, StateName, #periodic_ec{rate=R}=State) ->
    ?LOG(rtl_paranoid, "get_rate request: ~p", [R]),
    {reply, R, StateName, State};
handle_sync_event({set_rate, NewRate}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_rate request: ~p", [NewRate]),
    {Reply, NewState} = set_rate_req(NewRate, State),
    {reply, Reply, StateName, NewState};
handle_sync_event({add_comp, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "add_comp request: ~p.", [RTC]),
    {Reply, NewState} = add_comp_req(RTC, State),
    {reply, Reply, StateName, NewState};
handle_sync_event({rem_comp, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "rem_comp request: ~p.", [RTC]),
    {Reply, NewState} = rem_comp_req(RTC, State),
    {reply, Reply, StateName, NewState};
handle_sync_event({act_comp, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "act_comp request: ~p.", [RTC]),
    {reply, act_comp_req(RTC, State), StateName, State};
handle_sync_event({deact_comp, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "deact_comp request: ~p.", [RTC]),
    {reply, deact_comp_req(RTC, State), StateName, State};
handle_sync_event({reset_comp, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "reset_comp request: ~p.", [RTC]),
    {reply, reset_comp_req(RTC, State), StateName, State};
handle_sync_event({get_state, RTC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_state request: ~p.", [RTC]),
    {reply, get_state_req(RTC, State), StateName, State};
handle_sync_event(get_kind, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_kind request."),
    {reply, periodic, StateName, State};
handle_sync_event(get_profile, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_profile request."),
    {reply, get_profile_req(State), StateName, State};
handle_sync_event(get_owner, _From, StateName, #periodic_ec{owner=O}=State) ->
    ?LOG(rtl_paranoid, "get_owner request: ~p", [O]),
    {reply, O, StateName, State};
handle_sync_event(get_parts, _From, StateName, #periodic_ec{parts=P}=State) ->
    ?LOG(rtl_paranoid, "get_parts request: ~p", [P]),
    {reply, P, StateName, State};
handle_sync_event({set_corba_obj, Pid, Obj}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_corba_obj request: ~p ~p", [Pid, Obj]),
    {reply, ok, StateName, State#periodic_ec{c_pid=Pid, c_obj=Obj}};
handle_sync_event(get_corba_obj, _From, StateName, State) ->
    Obj = get_corba_obj(State),
    ?LOG(rtl_paranoid, "get_corba_obj request: ~p", [Obj]),
    {reply, Obj, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle an info message.
%% @spec handle_info(Info, StateName, State) -> Result
%% where
%%      Info = {'EXIT', pid(), Reason}
%%      Reason = any()
%%      StateName = atom()
%%      State = #periodic_ec{}
%%      Result = {next_state, NextStateName, NewState}
%%      NextStateName = atom()
%%      NewState = #periodic_ec{}
%% @end
-spec(handle_info(Info::{'EXIT', pid(), Reason::any()}, StateName::atom(),
        State::#periodic_ec{}) ->
    {next_state, NextStateName::atom(), NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
handle_info({'EXIT', _, normal}, StateName, State) ->
    {next_state, StateName, State};
handle_info({'EXIT', Pid, Reason}, StateName, State) ->
    ?LOG(rtl_error,
        "Periodic EC ~p detected death of participant manager ~p, reason: ~p",
        [self(), Pid, Reason]),
    {next_state, StateName, restart_ecpart(Pid, State)}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, StateName, State) -> any()
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      StateName = atom()
%%      State = #periodic_ec{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        StateName::atom(), State::#periodic_ec{}) -> any()).
%%-----------------------------------------------------------------------------
terminate(normal, _StateName, State) ->
    ?LOG(rtl_info, "Periodic EC Shutting down normally."),
    cleanup(State);
terminate(shutdown, _StateName, State) ->
    ?LOG(rtl_info, "Periodic EC Shut down by supervisor."),
    cleanup(State);
terminate({shutdown, Reason}, _StateName, State) ->
    ?LOG(rtl_info, "Periodic EC Shutting down: ~p", [Reason]),
    cleanup(State);
terminate(Reason, _StateName, State) ->
    ?LOG(rtl_info, "Periodic EC Unusual shutdown: ~p", [Reason]),
    cleanup(State).


%%-----------------------------------------------------------------------------
%% @doc Last-resort clean up function. There should be no participants by the
%% time this function is called; if there are, they will be logged.
%% @spec cleanup(State) -> ok
%% where
%%      State = #periodic_ec{}
%% @end
-spec(cleanup(State::#periodic_ec{}) -> ok).
%%-----------------------------------------------------------------------------
cleanup(#periodic_ec{parts=Parts, c_pid=nil}=State) ->
    ?LOG(rtl_debug, "No CORBA interface to stop."),
    cleanup_participants(Parts, State);
cleanup(#periodic_ec{parts=Parts, c_pid=CORBA}=State) ->
    ?LOG(rtl_debug, "Stopping CORBA interface ~p", [CORBA]),
    corba_obj_mgr:stop_server(CORBA),
    cleanup_participants(Parts, State).


%%-----------------------------------------------------------------------------
%% @doc Clean up participants. Deactivates then detaches each participant, and
%% kills the lifecycle managers.
%% @spec cleanup_participants(Participants, State) -> ok
%% where
%%      Participants = [{Handle, ECP, RTC}]
%%      Handle = ec_handle()
%%      ECP = pid()
%%          The lifecycle manager for the RTC.
%%      RTC = pid()
%%      State = #periodic_ec{}
%% @end
-spec(cleanup_participants(Participants::[{ec_handle(), pid(), pid()}],
        State::#periodic_ec{}) -> ok).
%%-----------------------------------------------------------------------------
cleanup_participants([], _) ->
    ?LOG(rtl_debug, "EC is shutting down with no participants."),
    ok;
cleanup_participants([{Handle, ECP, RTC}|T], State) ->
    ?LOG(rtl_warn, "EC is shutting down while RTC ~p is participating.",
        [{Handle, ECP, RTC}]),
    deact_comp_req(RTC, State),
    rem_comp_req(RTC, State),
    cleanup_participants(T, State).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%      {ok, NextStateName, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      StateName = atom()
%%      State = #periodic_ec{}
%%      Extra = any()
%%      NextStateName = atom()
%%      NewState = #periodic_ec{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, StateName::atom(),
        State::#periodic_ec{}, Extra::any()) ->
    {ok, NextStateName::atom(), NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Stopped state.
%% @spec stopped(Event, State) -> Result
%% where
%%      Event = start
%%      State = #periodic_ec{}
%%      Result = {next_state, NextStateName, NewState}
%%      NextStateName = started
%%      NewState = #periodic_ec{}
%% @end
-spec(stopped(Event::start, State::#periodic_ec{}) ->
    {next_state, NextStateName::started, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
stopped(start, #periodic_ec{period=P, parts=Parts}=State) ->
    ?LOG(rtl_debug, "Periodic EC entered started state."),
    Ticker = gen_fsm:send_event_after(P, tick),
    lists:foreach(fun({H, _, RTC}) -> call_on_startup(RTC, H) end, Parts),
    {next_state, started, State#periodic_ec{ticker=Ticker}}.


%%-----------------------------------------------------------------------------
%% @doc Call on_startup on a local or CORBA reference.
%% @spec call_on_startup(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_startup(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_startup(RTC, Handle) when is_pid(RTC) ->
    rtc:on_startup(RTC, Handle);
call_on_startup({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_startup(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Stopped state synchronous event handler.
%% @spec stopped(Event, From, State) -> Result
%% where
%%      Event = state | is_running
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #periodic_ec{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = stopped | false
%%      NextStateName = stopped
%%      NewState = #periodic_ec{}
%% @end
-spec(stopped(Event::state | is_running, From::{pid(), Tag::any()},
        State::#periodic_ec{}) ->
    {reply, stopped | false, NextStateName::stopped,
        NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
stopped(state, _From, State) ->
    {reply, stopped, stopped, State};
stopped(is_running, _From, State) ->
    ?LOG(rtl_paranoid, "Periodic EC got is_running event in stopped state"),
    {reply, false, stopped, State}.


%%-----------------------------------------------------------------------------
%% @doc Started state.
%% @spec started(Event, State) -> Result
%% where
%%      Event = timeout | any()
%%      State = #periodic_ec{}
%%      Result = {next_state, NextStateName, NewState, timeout()}
%%      NextStateName = atom()
%%      NewState = #periodic_ec{}
%% @end
-spec(started(Event::timeout | any(), State::#periodic_ec{}) ->
    {next_state, NextStateName::atom(), NewState::#periodic_ec{},
        timeout()}).
%%-----------------------------------------------------------------------------
started(stop, #periodic_ec{ticker=T, parts=Parts}=State) ->
    ?LOG(rtl_debug, "Periodic EC entering stopped state."),
    gen_fsm:cancel_timer(T),
    lists:foreach(fun({H, _, RTC}) -> call_on_shutdown(RTC, H) end, Parts),
    {next_state, stopped, State};
started(tick, #periodic_ec{period=P}=State) ->
    ?LOG(rtl_debug, "Periodic EC spinning."),
    Ticker = gen_fsm:send_event_after(P, tick),
    % Call on_execute+on_state_update or on_error for each RTC
    {next_state, started, State#periodic_ec{ticker=Ticker}}.


%%-----------------------------------------------------------------------------
%% @doc Call on_shutdown on a local or CORBA reference.
%% @spec call_on_shutdown(RTC, Handle) -> rtc_cb_return()
%% where
%%      RTC = pid() | object_ref()
%%      Handle = ec_handle()
%% @end
-spec(call_on_shutdown(RTC::pid() | object_ref(), Handle::ec_handle()) ->
    rtc_cb_return()).
%%-----------------------------------------------------------------------------
call_on_shutdown(RTC, Handle) when is_pid(RTC) ->
    rtc:on_shutdown(RTC, Handle);
call_on_shutdown({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC, Handle) ->
    utils:from_rtc_rc('OpenRTM_DataFlowComponent':on_shutdown(RTC, Handle)).


%%-----------------------------------------------------------------------------
%% @doc Started state synchronous event handler.
%% @spec started(Event, From, State) -> Result
%% where
%%      Event = state | is_running
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #periodic_ec{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = started | true
%%      NextStateName = started
%%      NewState = #periodic_ec{}
%% @end
-spec(started(Event::state | is_running, From::{pid(), Tag::any()},
        State::#periodic_ec{}) ->
    {reply, started | true, NextStateName::started, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
started(state, _From, State) ->
    {reply, started, started, State};
started(is_running, _From, State) ->
    ?LOG(rtl_paranoid, "Periodic EC got is_running event in started state"),
    {reply, true, started, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle the set_rate synchronous event.
%% @spec set_rate_req(NewRate, State) -> Result
%% where
%%      NewRate = float()
%%      State = #periodic_ec{}
%%      Result = {Reply, NewState}
%%      Reply = ok | bad_rate
%%      NewState = #periodic_ec{}
%% @end
-spec(set_rate_req(NewRate::float(), State::#periodic_ec{}) ->
    {ok | bad_rate, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
set_rate_req(NewRate, State) when NewRate =< 0 ->
    {bad_param, State};
set_rate_req(NewRate, #periodic_ec{ticker=Ticker, parts=Parts}=State) ->
    {NewP, NewTicker} = change_rate(NewRate, Ticker, Parts),
    {ok, State#periodic_ec{rate=NewRate, period=NewP, ticker=NewTicker}}.


%%-----------------------------------------------------------------------------
%% @doc Change the EC's rate.
%% @spec change_rate(NewRate, Ticker, RTCs) -> {NewPeriod, NewTicker}
%% where
%%      NewRate = float()
%%          The new rate in Hertz.
%%      Ticker = reference()
%%          The reference to the ticker running at the old rate.
%%      RTCs = [pid()]
%%          The list of RTC FSMs that must be informed of the change.
%%      NewPeriod = float()
%%          The new period in milliseconds.
%%      NewTicker = reference()
%% @end
-spec(change_rate(NewRate::float(), Ticker::reference(), RTCs::[pid()]) ->
    {NewPeriod::float(), NewTicker::reference()}).
%%-----------------------------------------------------------------------------
change_rate(NewRate, Ticker, RTCs) ->
    NewP = calc_period(NewRate),
    NewTicker = case Ticker
        of undefined ->
            undefined
         ; _ ->
            TimeLeft = gen_fsm:cancel_timer(Ticker),
            % Figure out the time remaining in the new rate
            NewTimeLeft = erlang:max(NewP - TimeLeft, 0),
            gen_fsm:send_event_after(NewTimeLeft, tick)
    end,
    % Inform any participating components.
    lists:foreach(fun({H, _, RTC}) -> rtc:on_rate_changed(RTC, H) end, RTCs),
    {NewP, NewTicker}.


%%-----------------------------------------------------------------------------
%% @doc Handle the get_profile synchronous event.
%% @spec get_profile_req(State) -> #ec_profile{}
%% where
%%      State = #periodic_ec{}
%% @end
-spec(get_profile_req(State::#periodic_ec{}) -> #ec_profile{}).
%%-----------------------------------------------------------------------------
get_profile_req(#periodic_ec{rate=R, owner=O, parts=Parts, config=C}) ->
    #ec_profile{kind=periodic,
        rate=float(R),
        owner=O,
        parts=[P || {_, _, P} <- Parts],
        props=C}.


%%-----------------------------------------------------------------------------
%% @doc Add a new component.
%% @spec add_comp_req(RTC, State) -> {ok | precon_not_met, NewState}
%% where
%%      RTC = pid() | object_ref()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(add_comp_req(RTC::pid() | object_ref(), State::#periodic_ec{}) ->
    {ok | precon_not_met, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
add_comp_req(RTC, State) ->
    % Check the RTC is a data flow RTC
    case rtc:type(RTC)
        of dataflow ->
            add_comp_req1(RTC, State)
         ; Other ->
            ?LOG(rtl_error,
                "Tried to add non-data-flow RTC of type ~p to periodic EC",
                [Other]),
            {precon_not_met, State}
    end.

-spec(add_comp_req1(RTC::pid() | object_ref(), State::#periodic_ec{}) ->
    {ok, NewState::#periodic_ec{}}).
add_comp_req1(RTC, #periodic_ec{parts=Parts}=State) ->
    % Attach this context to the RTC.
    Handle = call_attach_context(RTC, State),
    % Create a lifecycle manager for this RTC/EC pair.
    {ok, ECPart} = ec_part:start_link_fsm(RTC, Handle),
    % Store the pair
    ?LOG(rtl_info,
        "Adding component ~p with handle ~p and manager ~p",
        [RTC, Handle, ECPart]),
    {ok, State#periodic_ec{parts=[{Handle, ECPart, RTC} | Parts]}}.

-spec(call_attach_context(RTC::pid() | object_ref(), State::#periodic_ec{}) ->
    ec_handle()).
call_attach_context(RTC, _State) when is_pid(RTC) ->
    rtc:attach_context(RTC, self());
call_attach_context({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC,
        #periodic_ec{c_obj=Obj}) ->
    'OpenRTM_DataFlowComponent':attach_context(RTC, Obj).


%%-----------------------------------------------------------------------------
%% @doc Remove a component.
%% @spec rem_comp_req(RTC, State) ->
%%      {ok | precon_not_met | bad_param, NewState}
%% where
%%      RTC = pid()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(rem_comp_req(RTC::pid(), State::#periodic_ec{}) ->
    {ok | precon_not_met | bad_param, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
rem_comp_req(RTC, #periodic_ec{parts=Parts}=State) ->
    % Confirm the RTC is a participant of this EC
    case lists:keyfind(RTC, 3, Parts)
        of false ->
            ?LOG(rtl_error,
                "Tried to remove non-participating component ~p from EC",
                [RTC]),
            {bad_param, State}
         ; Participant ->
            rem_comp_if_inactive(Participant, State)
    end.

%%-----------------------------------------------------------------------------
%% @doc Remove a component only if it is inactive. Called from rem_comp_req/2.
%% @spec rem_comp_if_inactive(Participant, State) ->
%%      {ok | precon_not_met, NewState}
%% where
%%      Participant = {Handle, ECP, RTC}
%%      Handle = ec_handle()
%%      ECP = pid()
%%      RTC = pid() | object_ref()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(rem_comp_if_inactive(Participant::{Handle::ec_handle(), ECP::pid(),
            RTC::pid() | object_ref()}, State::#periodic_ec{}) ->
    {ok | precon_not_met, NewState::#periodic_ec{}}).
%%-----------------------------------------------------------------------------
rem_comp_if_inactive({H, ECP, RTC}=P, #periodic_ec{parts=Parts}=State) ->
    % Confirm the RTC is inactive
    case ec_part:state(ECP)
        of inactive ->
            ?LOG(rtl_info, "Removing component ~p/~p from EC",
                [RTC, H]),
            call_detach_context(RTC, State),
            ec_part:stop_fsm(ECP),
            {ok, State#periodic_ec{parts=lists:delete(P, Parts)}}
         ; S ->
            ?LOG(rtl_error,
                "Tried to remove non-inactive component ~p/~p (~p) from EC",
                [RTC, H, S]),
            {precon_not_met, State}
    end.

-spec(call_detach_context(RTC::pid() | object_ref(), State::#periodic_ec{}) ->
    ec_handle()).
call_detach_context(RTC, _State) when is_pid(RTC) ->
    rtc:detach_context(RTC, self());
call_detach_context({'OpenRTM_DataFlowComponent', _, _, _, _, _}=RTC,
        #periodic_ec{c_obj=Obj}) ->
    'OpenRTM_DataFlowComponent':detach_context(RTC, Obj).


%%-----------------------------------------------------------------------------
%% @doc Activate a component.
%% @spec act_comp_req(RTC, State) -> ok | precon_not_met | bad_param | error
%% where
%%      RTC = pid()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(act_comp_req(RTC::pid(), State::#periodic_ec{}) ->
    ok | precon_not_met | bad_param | error).
%%-----------------------------------------------------------------------------
act_comp_req(RTC, #periodic_ec{parts=Parts}) ->
    % Confirm the RTC is a participant of this EC
    case lists:keyfind(RTC, 3, Parts)
        of false ->
            case lists:filter(is_rtc_by_obj(RTC), Parts)
                of [] ->
                    ?LOG(rtl_error,
                        "Tried to activate non-participating component ~p",
                        [RTC]),
                    bad_param
                 ; [Participant] ->
                    act_comp_if_inactive(Participant)
            end
         ; Participant ->
            act_comp_if_inactive(Participant)
    end.


%%-----------------------------------------------------------------------------
%% @doc Activate a component only if it is inactive. Called from act_comp_req/2.
%% @spec act_comp_if_inactive(Participant) -> ok | precon_not_met | error
%% where
%%      Participant = {Handle, ECP, RTC}
%%      Handle = ec_handle()
%%      ECP = pid()
%%      RTC = pid()
%% @end
-spec(act_comp_if_inactive(Participant::{Handle::ec_handle(), ECP::pid(),
            RTC::pid()}) -> ok | precon_not_met | error).
%%-----------------------------------------------------------------------------
act_comp_if_inactive({H, ECP, RTC}) ->
    % Confirm the RTC is inactive
    case ec_part:state(ECP)
        of inactive ->
            ?LOG(rtl_info, "Activating component ~p/~p",
                [RTC, H]),
            ec_part:activate(ECP)
         ; S ->
            ?LOG(rtl_error,
                "Tried to activate non-inactive component ~p/~p (~p)",
                [RTC, H, S]),
            precon_not_met
    end.


%%-----------------------------------------------------------------------------
%% @doc Deactivate a component.
%% @spec deact_comp_req(RTC, State) -> ok | bad_param | precon_not_met | error
%% where
%%      RTC = pid()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(deact_comp_req(RTC::pid(), State::#periodic_ec{}) ->
    ok | bad_param | precon_not_met | error).
%%-----------------------------------------------------------------------------
deact_comp_req(RTC, #periodic_ec{parts=Parts}) ->
    % Confirm the RTC is a participant of this EC
    case lists:keyfind(RTC, 3, Parts)
        of false ->
            case lists:filter(is_rtc_by_obj(RTC), Parts)
                of [] ->
                    ?LOG(rtl_error,
                        "Tried to deactivate non-participating component ~p",
                        [RTC]),
                    bad_param
                 ; [Participant] ->
                    deact_comp_if_active(Participant)
            end
         ; Participant ->
            deact_comp_if_active(Participant)
    end.


%%-----------------------------------------------------------------------------
%% @doc Activate a component only if it is inactive. Called from act_comp_req/2.
%% @spec deact_comp_if_active(Participant) ->
%%      ok | precon_not_met | error
%% where
%%      Participant = {Handle, ECP, RTC}
%%      Handle = ec_handle()
%%      ECP = pid()
%%      RTC = pid()
%% @end
-spec(deact_comp_if_active(Participant::{Handle::ec_handle(), ECP::pid(),
            RTC::pid()}) -> ok | precon_not_met | error).
%%-----------------------------------------------------------------------------
deact_comp_if_active({H, ECP, RTC}) ->
    % Confirm the RTC is inactive
    case ec_part:state(ECP)
        of active ->
            ?LOG(rtl_info, "Deactivating component ~p/~p", [RTC, H]),
            ec_part:deactivate(ECP)
         ; S ->
            ?LOG(rtl_error,
                "Tried to deactivate non-active component ~p/~p (~p)",
                [RTC, S, H]),
            precon_not_met
    end.


%%-----------------------------------------------------------------------------
%% @doc Reset a component.
%% @spec reset_comp_req(RTC, State) -> ok | precon_not_met | bad_param | error
%% where
%%      RTC = pid()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(reset_comp_req(RTC::pid(), State::#periodic_ec{}) ->
    ok | precon_not_met | bad_param | error).
%%-----------------------------------------------------------------------------
reset_comp_req(RTC, #periodic_ec{parts=Parts}) ->

    % Confirm the RTC is a participant of this EC
    case lists:keyfind(RTC, 3, Parts)
        of false ->
            case lists:filter(is_rtc_by_obj(RTC), Parts)
                of [] ->
                    ?LOG(rtl_error,
                        "Tried to reset non-participating component ~p",
                        [RTC]),
                    bad_param
                 ; [Participant] ->
                    reset_comp_if_error(Participant)
            end
         ; Participant ->
            reset_comp_if_error(Participant)
    end.


%%-----------------------------------------------------------------------------
%% @doc Reset a component only if it is in error. Called from reset_comp_req/2.
%% @spec reset_comp_if_error(Participant) ->
%%      ok | precon_not_met | error
%% where
%%      Participant = {Handle, ECP, RTC}
%%      Handle = ec_handle()
%%      ECP = pid()
%%      RTC = pid()
%% @end
-spec(reset_comp_if_error(Participant::{Handle::ec_handle(), ECP::pid(),
            RTC::pid()}) -> ok | precon_not_met | error).
%%-----------------------------------------------------------------------------
reset_comp_if_error({H, ECP, RTC}) ->
    % Confirm the RTC is inactive
    case ec_part:state(ECP)
        of error ->
            ?LOG(rtl_info, "Resetting component ~p/~p", [RTC, H]),
            ec_part:reset(ECP)
         ; S ->
            ?LOG(rtl_error,
                "Tried to reset non-error component ~p/~p (~p)",
                [RTC, H, S]),
            precon_not_met
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the state of a component.
%% @spec get_state_req(RTC, State) -> rtc_lifecyclestate()
%% where
%%      RTC = pid()
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(get_state_req(RTC::pid(), State::#periodic_ec{}) ->
    rtc_lifecyclestate()).
%%-----------------------------------------------------------------------------
get_state_req(RTC, #periodic_ec{parts=Parts}) ->
    % Confirm the RTC is a participant of this EC
    case lists:keyfind(RTC, 3, Parts)
        of false ->
            case lists:filter(is_rtc_by_obj(RTC), Parts)
                of [] ->
                    ?LOG(rtl_error,
                        "Tried to get state of non-participating component ~p",
                        [RTC]),
                    bad_param
                 ; [{_Handle, ECP, _RTCObj}] ->
                    ec_part:state(ECP)
            end
         ; {_Handle, ECP, RTC} ->
            ec_part:state(ECP)
    end.


%%-----------------------------------------------------------------------------
%% @doc Returns a closure that checks if a participant matches a CORBA object.
%% @spec is_rtc_by_obj(object_ref()) -> fun()
%% @end
-spec(is_rtc_by_obj(Obj::object_ref()) -> fun()).
%%-----------------------------------------------------------------------------
is_rtc_by_obj(Obj) ->
    fun({_, _, RTC}) when is_pid(RTC) ->
        case rtc:get_corba_obj(RTC)
            of Obj ->
                true
             ; _ ->
                false
        end
    end.


%%-----------------------------------------------------------------------------
%% @doc Restart an EC participant manager.
%% @spec restart_ecpart(ECP, State) -> NewState
%% where
%%      ECP = pid()
%%          PID of the participant manager to restart
%%      State = #periodic_ec{}
%%      NewState = #periodic_ec{}
%% @end
-spec(restart_ecpart(ECP::pid(), State::#periodic_ec{}) ->
    NewState::#periodic_ec{}).
%%-----------------------------------------------------------------------------
restart_ecpart(ECP, #periodic_ec{parts=Parts}=State) ->
    ?LOG(rtl_warn, "Periodic EC ~p is restarting participant manager ~p",
        [self(), ECP]),
    % Find the old participant entry to get the EC handle and RTC
    P = lists:keyfind(ECP, 2, Parts),
    {H, ECP, RTC} = P,
    % Delete the old participant entry
    Parts1 = lists:delete(P, Parts),
    % Create a new one
    {ok, NewECP} = ec_part:start_link_fsm(RTC, H),
    ?LOG(rtl_warn, "Periodic EC ~p has replaced participant manager ~p with ~p",
        [self(), ECP, NewECP]),
    % Shift it to the error state
    ec_part:go_to_error(NewECP),
    % Add it to the list of participants
    State#periodic_ec{parts=[{H, NewECP, RTC} | Parts1]}.


%%-----------------------------------------------------------------------------
%% @doc Convert a value in Hz to ms.
%% @spec calc_period(Hz) -> Period
%% where
%%      Hz = float()
%%      Period = integer()
%%          The period in milliseconds.
%% @end
-spec(calc_period(Hz::float()) -> Period::integer()).
%%-----------------------------------------------------------------------------
calc_period(Hz) ->
    round(1000.0 / Hz).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object via the supervisor.
%% @spec get_corba_obj(State) -> object_ref()
%% where
%%      State = #periodic_ec{}
%% @end
-spec(get_corba_obj(State::#periodic_ec{}) -> object_ref()).
%%-----------------------------------------------------------------------------
get_corba_obj(#periodic_ec{c_obj=Obj}) ->
    Obj.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

