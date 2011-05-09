%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc State machine implementing a Data Flow Component.
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
-module(dataflow_rtc).
-behaviour(gen_fsm).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("rtc.hrl").
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


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).
% Introspection
-export([state/1]).
% States
-export([init_wait/2, alive/2, finalise_wait/2, exit_wait/2]).
-export([created/3, init_wait/3, alive/3, finalise_wait/3, exit_wait/3,
        destroyed/3]).
% For tests
-export([test_helper_proc/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the state machine.
%% @spec start_link_fsm(InstName, Config, Module) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Config = config()
%%          The configuration of the component. This should also include any
%%          configuration of execution contexts that will be owned by this RTC.
%%      Module = atom()
%%          The module providing the functionality of the component.
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link_fsm(Config::config(), Module::atom()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link_fsm(Config, Module) ->
    gen_fsm:start_link(dataflow_rtc, {Config, Module}, []).


%%-----------------------------------------------------------------------------
%% @doc Start a state machine without linking.
%% @spec start_fsm(Config, Module) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @see start_link_fsm/2
%% @end
-spec(start_fsm(Config::config(), Module::atom()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_fsm(Config, Module) ->
    gen_fsm:start(dataflow_rtc, {Config, Module}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the state machine.
%% @spec stop_fsm(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(stop_fsm(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_fsm(FSM) ->
    gen_fsm:send_all_state_event(FSM, terminate).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise this state machine.
%% @spec init({Config, Module}) -> {ok, StateName, State}
%% where
%%      Config = config()
%%      Module = atom()
%%      StateName = atom()
%%      State = #df_rtc{}
%% @end
-spec(init({Config::config(), Module::atom()}) ->
    {ok, StateName::atom(), State::#df_rtc{}}).
%%-----------------------------------------------------------------------------
init({Config, Module}) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_info,
        "Initialising dataflow RTC with callback module ~p and config ~p",
        [Module, Config]),
    {ok, created, #df_rtc{cfg=Config, mod=Module}}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event.
%% @spec handle_event(Event, StateName, State) -> Result
%% where
%%      Event = terminate
%%      StateName = atom()
%%      State = #df_rtc{}
%%      Result = {stop, normal, State}
%% @end
-spec(handle_event(Event::terminate, StateName::atom(), State::#df_rtc{}) ->
    {stop, normal, State::#df_rtc{}}).
%%-----------------------------------------------------------------------------
handle_event(terminate, _StateName, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle a synchronous event.
%% @spec handle_sync_event(Event, From, StateName, State -> Result
%% where
%%      Event = {shift_to, NextStateName} | get_behv_data |
%%          {set_cfg_svr, Pid} | {set_port_mgr, Pid} | {set_ecs_sup, Pid} |
%%          {set_sup, Pid} | get_corba_obj | {reg_cb, Event, fun()} |
%%          {unreg_cb, Ref} | {get_context, Handle} | get_owned_ecs |
%%          get_part_ecs | {get_ec_handle, EC} | get_type | {attach_ec, EC} |
%%          {detach_ec, EC} | get_comp_prof | get_ports |
%%          {add_port, Type, Name, DataType, Config}
%%      Pid = pid()
%%      Event = atom()
%%      Ref = reference()
%%      Handle = ec_handle()
%%      EC = pid() | object_ref()
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Config = config()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      StateName = atom()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = ok | any() | pid() | object_ref() | {ok, reference()} |
%%          {error, bad_event | no_cb} | [pid() | object_ref] | ec_handle() |
%%          dataflow | rtc_cb_return() | #comp_prof{}
%%      NextStateName = atom()
%%      NewState = #df_rtc{}
%% @end
-spec(handle_sync_event(Event::{shift_to, NextStateName::atom()} |
            get_behv_data | {set_cfg_svr, pid()} | {set_port_mgr, pid()} |
            {set_ecs_sup, pid()} | {set_sup, pid()} | get_corba_obj |
            {reg_cb, Event::atom(), fun()} | {unreg_cb, Ref::reference()} |
            {get_context, Handle::ec_handle()} | get_owned_ecs | get_part_ecs |
            {get_ec_handle, EC::pid() | object_ref()} | get_type |
            {attach_ec, EC::pid() | object_ref()} |
            {detach_ec, EC::pid() | object_ref()} | get_comp_prof | get_ports |
            {add_port, Type::port_type, Name::string(), DataType::string(),
                Config::config()},
            From::{pid(), Tag::any()}, StateName::atom(), State::#df_rtc{}) ->
    {reply, Reply::ok | any() | pid() | object_ref() | {ok, reference()} |
        {error, bad_event | no_cb} | [pid() | object_ref] | ec_handle() |
        dataflow | rtc_cb_return() | #comp_prof{},
        NextStateName::atom(), NewState::any()}).
%%-----------------------------------------------------------------------------
handle_sync_event({shift_to, NewStateName}, _From, _StateName, State) ->
    ?LOG(rtl_paranoid, "Shifting to new state ~p", [NewStateName]),
    {reply, ok, NewStateName, State};
handle_sync_event(get_behv_data, _From, StateName, #df_rtc{data=D}=State) ->
    ?LOG(rtl_paranoid, "Handling get_behv_data ~p", [D]),
    {reply, D, StateName, State};
handle_sync_event({set_cfg_svr, Pid}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_cfg_svr request: ~p", [Pid]),
    {reply, ok, StateName, State#df_rtc{cfg_svr=Pid}};
handle_sync_event({set_port_mgr, Pid}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_port_mgr request: ~p", [Pid]),
    {reply, ok, StateName, State#df_rtc{port_mgr=Pid}};
handle_sync_event({set_ecs_sup, Pid}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_ecs_sup request: ~p", [Pid]),
    {reply, ok, StateName, State#df_rtc{ecs_sup=Pid}};
handle_sync_event({set_sup, Pid}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "set_sup request: ~p", [Pid]),
    {reply, ok, StateName, State#df_rtc{sup_pid=Pid}};
handle_sync_event(get_corba_obj, _From, StateName, State) ->
    Obj = get_corba_obj(State),
    ?LOG(rtl_paranoid, "get_corba_obj request: ~p", [Obj]),
    {reply, Obj, StateName, State};
handle_sync_event({reg_cb, Event, Fun}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "reg_cb request: ~p, ~p", [Event, Fun]),
    {Result, NewState} = reg_cb(Event, Fun, State),
    {reply, Result, StateName, NewState};
handle_sync_event({unreg_cb, Ref}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "unreg_cb request: ~p", [Ref]),
    {Result, NewState} = unreg_cb(Ref, State),
    {reply, Result, StateName, NewState};
handle_sync_event({get_context, Handle}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_context request: ~p", [Handle]),
    {reply, get_ec(Handle, State), StateName, State};
handle_sync_event(get_owned_ecs, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_owned_ecs request"),
    {reply, owned_refs(State), StateName, State};
handle_sync_event(get_part_ecs, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_part_ecs request"),
    {reply, part_refs(State), StateName, State};
handle_sync_event({get_ec_handle, EC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_ec_handle request: ~p", [EC]),
    {reply, get_ec_handle(EC, State), StateName, State};
handle_sync_event(get_type, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "get_type request"),
    {reply, dataflow, StateName, State};
handle_sync_event({attach_ec, EC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "attach_ec ~p", [EC]),
    {Result, NewState} = attach_ec(EC, State),
    {reply, Result, StateName, NewState};
handle_sync_event({detach_ec, EC}, _From, StateName, State) ->
    ?LOG(rtl_paranoid, "detach_ec ~p", [EC]),
    {Result, NewState} = detach_ec(EC, State),
    {reply, Result, StateName, NewState};
handle_sync_event(get_comp_prof, _From, StateName, State) ->
    Prof = make_comp_prof(State),
    ?LOG(rtl_paranoid, "get_comp_prof ~p", [Prof]),
    {reply, Prof, StateName, State};
handle_sync_event(get_ports, _From, StateName, State) ->
    Ports = make_ports_list(State),
    ?LOG(rtl_paranoid, "get_ports ~p", [Ports]),
    {reply, Ports, StateName, State};
handle_sync_event({add_port, Type, Name, DataType, Config}, _From, StateName,
        State) ->
    Result = add_port(State, Type, Name, DataType, Config),
    ?LOG(rtl_paranoid, "add_port result is ~p", [Result]),
    {reply, Result, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle an info message.
%% @spec handle_info(Info, StateName, State) -> Result
%% where
%%      Info = {'EXIT', pid(), Reason}
%%      Reason = any()
%%      StateName = atom()
%%      State = #df_rtc{}
%%      Result = {next_state, StateName, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(handle_info(Info::{'EXIT', pid(), Reason::any()}, StateName::atom(),
            State::#df_rtc{}) ->
    {next_state, StateName::atom(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, StateName, State) ->
    ?LOG(rtl_paranoid, "Helper process ~p has exited cleanly.", [Pid]),
    {next_state, StateName, State};
handle_info({'EXIT', Pid, Reason}, StateName, State) ->
    {next_state, StateName, handle_crashed_helper(Pid, Reason, State)}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, StateName, State) -> any()
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      StateName = atom()
%%      State = any()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        StateName::atom(), State::any()) -> any()).
%%-----------------------------------------------------------------------------
terminate(normal, StateName, State) ->
    ?LOG(rtl_info, "Shutting down normally."),
    cleanup(StateName, State);
terminate(shutdown, StateName, State) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    cleanup(StateName, State);
terminate({shutdown, Reason}, StateName, State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    cleanup(StateName, State);
terminate(Reason, StateName, State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    cleanup(StateName, State).


%%-----------------------------------------------------------------------------
%% @doc Last-resort clean up function. There should be no execution contexts
%% at this point and the FSM should be in the destroyed state. If these
%% conditions are not met, they will be logged.
%% @spec cleanup(StateName, State) -> ok
%% where
%%      StateName = atom()
%%      State = #df_rtc{}
%% @end
-spec(cleanup(StateName::atom(), State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
cleanup(destroyed, _State) ->
    ok;
cleanup(StateName, _State) ->
    ?LOG(rtl_error, "Dataflow RTC terminated while still in state ~p",
        [StateName]),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%      {ok, NextStateName, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      StateName = atom()
%%      State = #df_rtc{}
%%      Extra = any()
%%      NextStateName = atom()
%%      NewState = #df_rtc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, StateName::atom(),
        State::#df_rtc{}, Extra::any()) ->
    {ok, NextStateName::atom(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%-----------------------------------------------------------------------------
%% @doc created state synchronous event handler.
%% @spec created(Event, From, State) -> Result
%% where
%%      Event = {is_alive, ECHandle} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, created, NewState} |
%%          {next_state, init_wait, NewState}
%%      Reply = false | created | precon_not_met
%%      NextStateName = created
%%      NewState = #df_rtc{}
%% @end
-spec(created(Event::{is_alive, ECHandle::ec_handle()} | state | initialise |
            finalise | exit,
        From::{pid(), Tag::any()}, State::#df_rtc{}) ->
    {next_state, init_wait, NewState::#df_rtc{}} |
    {reply, Reply::false | created, NextStateName::created,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
created({is_alive, _ECHandle}, _From, State) ->
    {reply, false, created, State};
created(state, _From, State) ->
    {reply, created, created, State};
created(initialise, From, State) ->
    ?LOG(rtl_debug, "Initialise called; beginning initialisation process."),
    State1 = create_ecs(State),
    State2 = add_helper(fun rtc_initialiser:start_link/2,
        [owned_refs(State1)], From, State1),
    ?LOG(rtl_debug, "Going to init_wait state."),
    {next_state, init_wait, State2};
created(finalise, _From, State) ->
    {reply, precon_not_met, created, State};
created(exit, _From, State) ->
    {reply, precon_not_met, created, State}.


%%-----------------------------------------------------------------------------
%% @doc init_wait state.
%% @spec init_wait(Event, State) -> Result
%% where
%%      Event = {init_done, rtc_cb_return(), Helper}
%%      Helper = pid()
%%      State = #df_rtc{}
%%      Result = {next_state, alive | created, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(init_wait(Event::{init_done, rtc_cb_return(), Helper::pid()},
        State::#df_rtc{}) ->
    {next_state, NextStateName::alive | created, NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
init_wait({init_done, Res, Helper}=E, State) ->
    case is_valid_helper(Helper, State)
        of {Helper, C} ->
            init_wait_helper_resp(Res, C, del_helper(Helper, State))
         ; false ->
            ?LOG(rtl_error, "Spurious helper response in init_wait: ~p", [E]),
            {next_state, init_wait, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle the response from the initialisation helper process.
%% @spec init_wait_helper_resp(Result, Caller, State) ->
%%      {next_state, NextState, NewState}
%% where
%%      Result = rtc_cb_return()
%%      Caller = pid()
%%      State = #df_rtc{}
%%      NextState = created | alive
%%      NewState = #df_rtc{}
%% @end
-spec(init_wait_helper_resp(Result::rtc_cb_return(), Caller::pid,
        State::#df_rtc{}) ->
    {next_state, NextState::created | alive, NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
init_wait_helper_resp(ok, Caller, #df_rtc{mod=M}=State) ->
    case do_no_ec_behv_cb(M, on_initialize, State)
        of {ok, NewState} ->
            reply_caller(Caller, ok),
            do_callbacks(initialised, NewState),
            ?LOG(rtl_info, "Initialisation completed successfully."),
            {next_state, alive, NewState}
         ; {Result, NewState} ->
            reply_caller(Caller, Result),
            ?LOG(rtl_info, "Initialisation failed: ~p.", [Result]),
            {next_state, created, NewState}
    end;
init_wait_helper_resp(Error, Caller, State) ->
    reply_caller(Caller, Error),
    ?LOG(rtl_info, "Initialisation failed: ~p.", [Error]),
    {next_state, created, State}.


%%-----------------------------------------------------------------------------
%% @doc init_wait state synchronous event handler.
%% @spec init_wait(Event, From, State) -> Result
%% where
%%      Event = {is_alive, ECHandle} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = false | init_wait | precon_not_met
%%      NextStateName = init_wait
%%      NewState = #df_rtc{}
%% @end
-spec(init_wait(Event::{is_alive, ECHandle::ec_handle()} | state | initialise |
        finalise | exit, From::{pid(), Tag::any()}, State::#df_rtc{}) ->
    {reply, Reply::false | init_wait, NextStateName::init_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
init_wait({is_alive, _ECHandle}, _From, State) ->
    {reply, false, init_wait, State};
init_wait(state, _From, State) ->
    {reply, init_wait, init_wait, State};
init_wait(initialise, _From, State) ->
    {reply, precon_not_met, init_wait, State};
init_wait(finalise, _From, State) ->
    {reply, precon_not_met, init_wait, State};
init_wait(exit, _From, State) ->
    {reply, precon_not_met, init_wait, State};
init_wait({A, H}, _From, State) when is_atom(A) and is_reference(H) ->
    {reply, precon_not_met, init_wait, State}.


%%-----------------------------------------------------------------------------
%% @doc alive state.
%% @spec alive(Event, State) -> Result
%% where
%%      Event = {cb_done, CBResult, NewData, Helper}
%%      CBResult = rtc_cb_return()
%%      NewData = any()
%%      Helper = pid()
%%      State = #df_rtc{}
%%      Result = {next_state, alive, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(alive(Event::{cb_done, CBResult::rtc_cb_return(), NewData::any(),
            Helper::pid()}, State::#df_rtc{}) ->
    {next_state, NextStateName::alive, NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
alive({cb_done, CBResult, NewData, Helper}, State) ->
    handle_rtc_cb_result(CBResult, NewData, Helper, alive, State).


%%-----------------------------------------------------------------------------
%% @doc alive state synchronous event handler.
%% @spec alive(Event, From, State) -> Result
%% where
%%      Event = {is_alive, EC} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState} |
%%          {next_state, NextStateName, NewState}
%%      Reply = true | alive | precon_not_met
%%      NextStateName = alive | destroyed
%%      NewState = #df_rtc{}
%% @end
-spec(alive(Event::{is_alive, EC::pid() | object_ref()} | state | initialise |
        finalise | exit, From::{pid(), Tag::any()}, State::#df_rtc{}) ->
    {reply, Reply::true | alive | precon_not_met, NextStateName::alive,
        NewState::#df_rtc{}} |
    {next_state, NextStateName::finalise_wait | exit_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
alive({is_alive, EC}, _From, State) ->
    Result = alive_in_ec(EC, State),
    ?LOG(rtl_paranoid, "Alive check in EC ~p result is ~p", [EC, Result]),
    {reply, Result, alive, State};
alive(state, _From, State) ->
    {reply, alive, alive, State};
alive(initialise, _From, State) ->
    {reply, precon_not_met, alive, State};
alive(finalise, From, State) ->
    begin_finalise(From, State);
alive(exit, From, State) ->
    begin_exit(From, State);
alive({_, _}=Action, From, State) ->
    % The response to this event will come from alive/2 or handle_info (if the
    % helper crashes).
    {next_state, alive, handle_rtc_action(Action, From, State)}.


%%-----------------------------------------------------------------------------
%% @doc Checks if the RTC is alive in a given EC.
%% @spec alive_in_ec(EC, State) -> boolean()
%% where
%%      EC = pid() | object_ref()
%%      State = #df_rtc{}
%% @end
-spec(alive_in_ec(EC::pid() | object_ref(), State::#df_rtc{}) -> boolean()).
%%-----------------------------------------------------------------------------
alive_in_ec(EC, State) ->
    case find_ec_in_parts(EC, State)
        of false ->
            false
         ; _ ->
            true
    end.


%%-----------------------------------------------------------------------------
%% @doc finalise_wait state.
%% @spec finalise_wait(Event, State) -> Result
%% where
%%      Event = {finalise_done, Result, Helper}
%%      Result = rtc_cb_return()
%%      Helper = pid()
%%      State = #df_rtc{}
%%      Result = {next_state, destroyed | finalise_wait, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(finalise_wait(Event::{finalise_done, Result::rtc_cb_return(),
            Helper::pid()} |
        {cb_done, CBResult::rtc_cb_return(), NewData::any(), Helper::pid()},
        State::#df_rtc{}) ->
    {next_state, NextStateName::destroyed | finalise_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
finalise_wait({cb_done, CBResult, NewData, Helper}, State) ->
    % A trailing request that was being handled when the finalise call came in.
    % It still needs to be answered.
    handle_rtc_cb_result(CBResult, NewData, Helper, finalise_wait, State);
finalise_wait({finalise_done, Result, Helper}=E, State) ->
    case is_valid_helper(Helper, State)
        of {Helper, C} ->
            NewState = finish_finalise(State),
            reply_caller(C, Result),
            do_callbacks(finalised, NewState),
            ?LOG(rtl_info, "Finalisation completed with result ~p.",
                [Result]),
            {next_state, destroyed, NewState}
         ; false ->
            ?LOG(rtl_error, "Spurious helper response in finalise_wait: ~p",
                [E]),
            {next_state, finalise_wait, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc finalise_wait state synchronous event handler.
%% @spec finalise_wait(Event, From, State) -> Result
%% where
%%      Event = {is_alive, ECHandle} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = false | finalise_wait | precon_not_met
%%      NextStateName = finalise_wait
%%      NewState = #df_rtc{}
%% @end
-spec(finalise_wait(Event::{is_alive, ECHandle::ec_handle()} | state |
        initialise | finalise | exit, From::{pid(), Tag::any()},
        State::#df_rtc{}) ->
    {reply, Reply::false | finalise_wait, NextStateName::finalise_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
finalise_wait({is_alive, _ECHandle}, _From, State) ->
    {reply, false, finalise_wait, State};
finalise_wait(state, _From, State) ->
    {reply, finalise_wait, finalise_wait, State};
finalise_wait(initialise, _From, State) ->
    {reply, precon_not_met, finalise_wait, State};
finalise_wait(finalise, _From, State) ->
    {reply, precon_not_met, finalise_wait, State};
finalise_wait(exit, _From, State) ->
    {reply, precon_not_met, finalise_wait, State};
finalise_wait({A, H}, _From, State) when is_atom(A) and is_reference(H) ->
    {reply, precon_not_met, finalise_wait, State}.


%%-----------------------------------------------------------------------------
%% @doc exit_wait state.
%% @spec exit_wait(Event, State) -> Result
%% where
%%      Event = {exiting, Helper}
%%      Helper = pid()
%%      State = #df_rtc{}
%%      Result = {next_state, exit_wait | exit_wait, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(exit_wait(Event::{exiting, Helper::pid()} |
        {cb_done, CBResult::rtc_cb_return(), NewData::any(), Helper::pid()},
        State::#df_rtc{}) ->
    {next_state, NextStateName::exit_wait | exit_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
exit_wait({cb_done, CBResult, NewData, Helper}, State) ->
    % A trailing request that was being handled when the exit call came in.
    % It still needs to be answered.
    handle_rtc_cb_result(CBResult, NewData, Helper, exit_wait, State);
exit_wait({exiting, Helper}=E, State) ->
    case is_valid_helper(Helper, State)
        of {Helper, C} ->
            ?LOG(rtl_info, "Exit wait completed."),
            begin_finalise(C, State)
         ; false ->
            ?LOG(rtl_error, "Spurious helper response in exit_wait: ~p",
                [E]),
            {next_state, exit_wait, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc exit_wait state synchronous event handler.
%% @spec exit_wait(Event, From, State) -> Result
%% where
%%      Event = {is_alive, ECHandle} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = false | exit_wait | precon_not_met
%%      NextStateName = exit_wait
%%      NewState = #df_rtc{}
%% @end
-spec(exit_wait(Event::{is_alive, ECHandle::ec_handle()} | state |
        initialise | finalise | exit, From::{pid(), Tag::any()},
        State::#df_rtc{}) ->
    {reply, Reply::false | exit_wait, NextStateName::exit_wait,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
exit_wait({is_alive, _ECHandle}, _From, State) ->
    {reply, false, exit_wait, State};
exit_wait(state, _From, State) ->
    {reply, exit_wait, exit_wait, State};
exit_wait(initialise, _From, State) ->
    {reply, precon_not_met, exit_wait, State};
exit_wait(finalise, _From, State) ->
    {reply, precon_not_met, exit_wait, State};
exit_wait(exit, _From, State) ->
    {reply, precon_not_met, exit_wait, State};
exit_wait({A, H}, _From, State) when is_atom(A) and is_reference(H) ->
    {reply, precon_not_met, exit_wait, State}.


%%-----------------------------------------------------------------------------
%% @doc destroyed state synchronous event handler.
%% @spec created(Event, From, State) -> Result
%% where
%%      Event = {is_alive, ECHandle} | state | initialise | finalise | exit
%%      ECHandle = ec_handle()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #df_rtc{}
%%      Result = {reply, Reply, NextStateName, NewState}
%%      Reply = false | destroyed | precon_not_met
%%      NextStateName = destroyed
%%      NewState = #df_rtc{}
%% @end
-spec(destroyed(Event::{is_alive, ECHandle::ec_handle()} | state | initialise |
            finalise | exit,
        From::{pid(), Tag::any()}, State::#df_rtc{}) ->
    {reply, Reply::false | destroyed, NextStateName::destroyed,
        NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
destroyed({is_alive, _ECHandle}, _From, State) ->
    {reply, false, destroyed, State};
destroyed(state, _From, State) ->
    {reply, destroyed, destroyed, State};
destroyed(initialise, _From, State) ->
    {reply, precon_not_met, destroyed, State};
destroyed(finalise, _From, State) ->
    {reply, precon_not_met, destroyed, State};
destroyed(exit, _From, State) ->
    {reply, precon_not_met, destroyed, State};
destroyed({A, H}, _From, State) when is_atom(A) and is_reference(H) ->
    {reply, precon_not_met, destroyed, State}.


%%-----------------------------------------------------------------------------
%% @doc RTC callback result handler.
%% @spec handle_rtc_cb(CBResult, NewData, Helper, NextStateName, State) ->
%%      Result
%% where
%%      CBResult = rtc_cb_return()
%%      NewData = any()
%%      Helper = pid()
%%      NextStateName = atom()
%%      State = #df_rtc{}
%%      Result = {next_state, NextStateName, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(handle_rtc_cb_result(CBResult::rtc_cb_return(), NewData::any(),
        Helper::pid(), NextStateName::atom(), State::#df_rtc{}) ->
    {next_state, NextStateName::atom(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
handle_rtc_cb_result(CBResult, NewData, Helper, NextStateName, State) ->
    case is_valid_helper(Helper, State)
        of {Helper, C} ->
            reply_caller(C, CBResult),
            {next_state, NextStateName,
                del_helper(Helper, State#df_rtc{data=NewData})}
         ; false ->
            ?LOG(rtl_error, "Spurious helper response in ~p: ~p",
                [NextStateName, {cb_done, CBResult, NewData, Helper}]),
            {next_state, NextStateName, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Create the owned execution contexts of the RTC based on the config.
%% @spec create_ecs(State) -> NewState
%% where
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(create_ecs(State::#df_rtc{}) -> NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
create_ecs(#df_rtc{cfg=Cfg}=State) ->
    case config:get_value("exec_cxts", Cfg)
        of undefined ->
            ?LOG(rtl_info, "No default execution contexts specified."),
            State
         ; EC_Cfgs ->
            ?LOG(rtl_paranoid, "List of ECs to create: ~p", [EC_Cfgs]),
            lists:foldl(fun create_ec/2, State, EC_Cfgs)
    end.


%%-----------------------------------------------------------------------------
%% @doc Create an owned EC based on the given EC configuration.
%% @spec create_ec(Cfg, State) -> NewState
%% where
%%      Cfg = config()
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(create_ec(Cfg::config(), State::#df_rtc{}) -> NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
create_ec({ID, Cfg}, State) ->
    case config:get_value("type", Cfg)
        of undefined ->
            ?LOG(rtl_info, "Creating periodic EC using config ~p", [Cfg]),
            create_ec1(ID, Cfg, State)
         ; "periodic" ->
            ?LOG(rtl_info, "Creating periodic EC using config ~p", [Cfg]),
            create_ec1(ID, Cfg, State)
         ; Other ->
            ?LOG(rtl_warn, "Cannot create an EC of type ~p", [Other]),
            State
    end.

create_ec1(ID, Cfg, #df_rtc{ecs_sup=S, owned_ecs=OECs}=State) ->
    {ok, EC_Sup, EC_FSM} = supervisor:start_child(S,
        {ID, {ec, create, [Cfg]}, temporary, infinity, supervisor, [ec]}),
    ?LOG(rtl_debug, "Created new EC; supervisor is ~p, FSM is ~p",
        [EC_Sup, EC_FSM]),
    ok = ec:set_owner(EC_FSM, self()),
    {H, State1} = get_next_ec_handle(State),
    State1#df_rtc{owned_ecs=[{H, EC_Sup, EC_FSM, ID} | OECs]}.


%%-----------------------------------------------------------------------------
%% @doc Add and start a new helper process that will return the result later
%% as an event.
%% @spec add_helper(Fun, Args, Caller, State) -> NewState
%% where
%%      Fun = fun()
%%          The function to be called that will start the helper.
%%      Args = any()
%%          Arguments to the helper process.
%%      Caller = pid()
%%          The PID of the process to which the result of the task will be sent
%%          by *this* FSM after receiving the reply from the helper. The helper
%%          will not send anything to this process directly.
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(add_helper(Fun::fun(), Args::any(), Caller::pid(), State::#df_rtc{}) ->
    NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
add_helper(Fun, Args, Caller, #df_rtc{helpers=H}=State) ->
    {ok, Pid} = Fun(self(), Args),
    ?LOG(rtl_debug, "Added helper process ~p calling ~p(~p)",
        [Pid, Fun, Args]),
    State#df_rtc{helpers=[{Pid, Caller}|H]}.


%%-----------------------------------------------------------------------------
%% @doc Delete a helper. Presumably it has finished by this point.
%% @spec del_helper(Pid, State) -> NewState
%% where
%%      Pid = pid()
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(del_helper(Pid::pid(), State::#df_rtc{}) -> NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
del_helper(Pid, #df_rtc{helpers=H}=State) ->
    NewH = del_helper(Pid, H, []),
    State#df_rtc{helpers=NewH}.

del_helper(_, [], Acc) ->
    Acc;
del_helper(Pid, [{Pid, _}=H|T], Acc) ->
    case is_process_alive(Pid)
        of true ->
            ?LOG(rtl_warn, "Deleting helper ~p while it is still alive.",
                [H])
         ; false ->
            ok
    end,
    ?LOG(rtl_debug, "Deleted helper ~p", [H]),
    Acc ++ T;
del_helper(Pid, [H|T], Acc) ->
    del_helper(Pid, T, [H|Acc]).


%%-----------------------------------------------------------------------------
%% @doc Check if a helper is valid, and return the complete helper if it is.
%% @spec is_valid_helper(Pid, State) -> Result
%% where
%%      Pid = pid()
%%      State = #df_rtc{}
%%      Result = {Helper, Caller} | false
%%      Helper = pid()
%%      Caller = pid()
%% @end
-spec(is_valid_helper(Pid::pid(), State::#df_rtc{}) ->
        {Helper::pid(), Caller::pid()} | false).
%%-----------------------------------------------------------------------------
is_valid_helper(P, #df_rtc{helpers=H}) ->
    is_valid_helper1(P, H).

is_valid_helper1(_, []) ->
    false;
is_valid_helper1(Pid, [{Pid, _}=Helper|_]) ->
    Helper;
is_valid_helper1(Pid, [_|T]) ->
    is_valid_helper1(Pid, T).


%%-----------------------------------------------------------------------------
%% @doc Reply to the caller that triggered a helper execution.
%% @spec reply_caller(Caller, Result) -> ok
%% where
%%      Caller = pid()
%%      Result = any()
%% @end
-spec(reply_caller(Caller::pid(), Result::any()) -> ok).
%%-----------------------------------------------------------------------------
reply_caller({_, _}=Caller, Result) ->
    gen_fsm:reply(Caller, Result);
reply_caller(Caller, Result) ->
    Caller ! Result.


%%-----------------------------------------------------------------------------
%% @doc Directly call a callback in the behaviour module without an EC.
%% @spec do_no_ec_behv_cb(Module, CB, State) -> {rtc_cb_return(), NewState}
%% where
%%      Module = atom()
%%      CB = on_initialize | on_finalize
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(do_no_ec_behv_cb(Module::atom(), CB::on_initialize | on_finalize,
        State::#df_rtc{}) ->
    {rtc_cb_return(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
do_no_ec_behv_cb(Module, on_initialize, #df_rtc{port_mgr=PM}=State) ->
    {Result, NewData} = apply(Module, on_initialize, [self(), PM]),
    {Result, State#df_rtc{data=NewData}};
do_no_ec_behv_cb(Module, on_finalize, #df_rtc{port_mgr=PM, data=Data}=State) ->
    {Result, NewData} = apply(Module, on_finalize, [self(), PM, Data]),
    {Result, State#df_rtc{data=NewData}}.


%%-----------------------------------------------------------------------------
%% @doc Begin the finalisation process. The remainder will occur as events.
%% @spec begin_finalise(From, State) -> ok
%% where
%%      From = pid()
%%      State = #df_rtc{}
%% @end
-spec(begin_finalise(From::pid(), State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
begin_finalise(From, #df_rtc{part_ecs=P, mod=M}=State) ->
    ?LOG(rtl_debug, "Beginning finalisation process."),
    case length(P)
        of 0 ->
            {Result, State1} = do_no_ec_behv_cb(M, on_finalize, State),
            % The helper shifts all ECs to the stopped state.
            % When it finishes, it will send the done event.
            % The handler for this event will call a generic exiter function.
            State2 = add_helper(fun rtc_finaliser:start_link/2,
                [owned_refs(State), [], Result],
                From, State1),
            {next_state, finalise_wait, State2}
         ; L ->
            ?LOG(rtl_warn,
                "Tried to finalise while still participating in ~p ECs.",
                [L]),
            {reply, precon_not_met, alive, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Finish the finalisation process. This will shut down the CORBA
%% interfaces, shut down the ports, and finally shut down the ECs (which should
%% no longer be attached), in that order.
%% @spec finish_finalise(State) -> ok
%% where
%%      State = #df_rtc{}
%% @end
-spec(finish_finalise(State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
finish_finalise(#df_rtc{sup_pid=S}=State) ->
    ?LOG(rtl_debug, "Finishing finalisation process."),
    {corba, CORBA, worker, _} = lists:keyfind(corba, 1,
        supervisor:which_children(S)),
    corba_obj_mgr:stop_server(CORBA),
    NewState = shutdown_ecs(shutdown_ports(State)),
    ?LOG(rtl_debug, "Shutdown complete."),
    NewState.


%%-----------------------------------------------------------------------------
%% @doc Shut down all ports.
%% @spec shutdown_ports(State) -> ok
%% where
%%      State = #df_rtc{}
%% @end
-spec(shutdown_ports(State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
shutdown_ports(#df_rtc{port_mgr=PM}=State) ->
    ?LOG(rtl_debug, "Shutting down ports."),
    port_mgr:stop_server(PM),
    State.


%%-----------------------------------------------------------------------------
%% @doc Shut down all owned ECs.
%% @spec shutdown_ecs(State) -> ok
%% where
%%      State = #df_rtc{}
%% @end
-spec(shutdown_ecs(State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
shutdown_ecs(#df_rtc{owned_ecs=ECs}=State) ->
    % TODO: better EC management - use a separate process like the port manager
    ShutdownEC = fun({_, Sup, _, _}) ->
        ec:destroy(Sup)
    end,
    ?LOG(rtl_debug, "Shutting down owned ECs: ~p", [ECs]),
    lists:foreach(ShutdownEC, ECs),
    State#df_rtc{owned_ecs=[]}.


%%-----------------------------------------------------------------------------
%% @doc Begin the exit process. The remainder will occur as helper events.
%% @spec begin_exit(From, State) -> ok
%% where
%%      From = pid()
%%      State = #df_rtc{}
%% @end
-spec(begin_exit(From::pid(), State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
begin_exit(From, State) ->
    ?LOG(rtl_debug, "Beginning exit process."),
    % Deactivate in all participating ECs (will call on_deactivated).
    % Call remove_component on all ECs (will call detach_context).
    State1 = add_helper(fun rtc_exiter:start_link/2, [part_refs(State)],
        From, State),
    % Move to the exit_wait state, which will itself go to finalise_wait
    {next_state, exit_wait, State1}.


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
%% @doc Register a callback.
%% @spec reg_cb(Event, Fun, State) -> Result
%% where
%%      Event = atom()
%%      Fun = fun()
%%      State = #df_rtc{}
%%      Result = {{ok, reference()} | {error, bad_event}, NewState}
%%      NewState = #df_rtc{}
%% @end
-spec(reg_cb(Event::atom(), Fun::fun(), State::#df_rtc{}) ->
    {{ok, reference()} | {error, bad_event}, NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
reg_cb(Event, Fun, #df_rtc{cbs=CBs}=State) ->
    case lists:member(Event, valid_events())
        of true ->
            Ref = make_ref(),
            ?LOG(rtl_debug,
                "Registering new CB for event ~p with reference ~p",
                [Event, Ref]),
            {Ref, State#df_rtc{cbs=[{Ref, Event, Fun} | CBs]}}
         ; false ->
            ?LOG(rtl_warn, "Tried to register CB for invalid event ~p",
                [Event]),
            {{error, bad_event}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc The list of valid events.
%% @spec valid_events() -> [atom()]
%% @end
-spec(valid_events() -> [atom()]).
%%-----------------------------------------------------------------------------
valid_events() ->
    [initialised, finalised].


%%-----------------------------------------------------------------------------
%% @doc Unregister a callback.
%% @spec unreg_cb(Ref, State) -> {ok | {error, no_cb}, NewState}
%% where
%%      Ref = reference()
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(unreg_cb(Ref::reference(), State::#df_rtc{}) ->
    {ok | {error, no_cb}, NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
unreg_cb(Ref, #df_rtc{cbs=CBs}=State) ->
    case lists:keyfind(Ref, 1, CBs)
        of false ->
            {{error, no_cb}, State}
         ; {Ref, _, _} ->
         {ok, State#df_rtc{cbs=lists:keydelete(Ref, 1, CBs)}}
 end.


%%-----------------------------------------------------------------------------
%% @doc Launch the callback functions.
%% @spec do_callbacks(Event, State) -> ok
%% where
%%      Event = atom()
%%      State = #df_rtc{}
%% @end
-spec(do_callbacks(Event::atom(), State::#df_rtc{}) -> ok).
%%-----------------------------------------------------------------------------
do_callbacks(Event, #df_rtc{cbs=CBs}) ->
    ?LOG(rtl_debug, "Doing callbacks for internal event ~p", [Event]),
    lists:foreach(fun(F) -> spawn_cb_proc(F) end,
        [F || {_, E, F} <- CBs, E == Event]).


%%-----------------------------------------------------------------------------
%% @doc Launch a process to run a callback.
%% @spec spawn_cb_proc(F) -> ok
%% where
%%      F = fun()
%% @end
-spec(spawn_cb_proc(F::fun()) -> ok).
%%-----------------------------------------------------------------------------
spawn_cb_proc(F) ->
    Pid = spawn(F),
    ?LOG(rtl_debug, "Launched process ~p to call CB ~p", [Pid, F]).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object via the supervisor.
%% @spec get_corba_obj(State) -> object_ref()
%% where
%%      State = #df_rtc{}
%% @end
-spec(get_corba_obj(State::#df_rtc{}) -> object_ref()).
%%-----------------------------------------------------------------------------
get_corba_obj(#df_rtc{sup_pid=S}) ->
    {corba, CORBA, worker, _} = lists:keyfind(corba, 1,
        supervisor:which_children(S)),
    corba_obj_mgr:get_obj(CORBA).


%%-----------------------------------------------------------------------------
%% @doc Get the next available EC handle.
%% @spec get_next_ec_handle(State) -> ec_handle()
%% where
%%      State = #df_rtc{}
%% @end
-spec(get_next_ec_handle(State::#df_rtc{}) -> ec_handle()).
%%-----------------------------------------------------------------------------
get_next_ec_handle(#df_rtc{next_ech=Next}=State) ->
    {Next, State#df_rtc{next_ech=Next + 1}}.


%%-----------------------------------------------------------------------------
%% @doc Attach this RTC to an execution context.
%% @spec attach_ec(EC, State) -> {Result, NewState}
%% where
%%      EC = pid() | object_ref()
%%      State = #df_rtc{}
%%      Result = ec_handle() | none
%%      NewState = #df_rtc{}
%% @end
-spec(attach_ec(EC::pid() | object_ref(), State::#df_rtc{}) ->
    {Result::ec_handle(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
attach_ec(EC, #df_rtc{part_ecs=PECs}=State) ->
    case find_ec_in_owned(EC, State)
        of {Handle, _, EC, _} ->
            ?LOG(rtl_info, "Attaching to EC ~p using existing handle ~p",
                [EC, Handle]),
            {Handle, State#df_rtc{part_ecs=[{Handle, EC} | PECs]}}
         ; false ->
            {Handle, State1} = get_next_ec_handle(State),
            ?LOG(rtl_info, "Attaching to EC ~p using handle ~p", [EC, Handle]),
            {Handle, State1#df_rtc{part_ecs=[{Handle, EC} | PECs]}}
    end.


%%-----------------------------------------------------------------------------
%% @doc Detach this RTC from an execution context.
%% @spec detach_ec(EC, State) -> {Result, NewState}
%% where
%%      EC = pid()
%%      State = #df_rtc{}
%%      Result = rtc_cb_return()
%%      NewState = #df_rtc{}
%% @end
-spec(detach_ec(EC::pid(), State::#df_rtc{}) ->
    {Result::rtc_cb_return(), NewState::#df_rtc{}}).
%%-----------------------------------------------------------------------------
detach_ec(EC, #df_rtc{part_ecs=PECs}=State) ->
    ?LOG(rtl_debug, "Detaching from EC ~p (participating in ~p)", [EC, PECs]),
    case lists:keyfind(EC, 2, PECs)
        of {Handle, EC} ->
            ?LOG(rtl_info, "Detaching from execution context ~p",
                [{Handle, EC}]),
            {ok, State#df_rtc{part_ecs=lists:keydelete(EC, 2, PECs)}}
         ; false ->
            ?LOG(rtl_error, "Tried to detach from unattached EC ~p", [EC]),
            {precon_not_met, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Finds an EC in the owned ECs list by object reference or PID.
%% @spec find_ec_in_owned(EC, State) -> Participant
%% where
%%      EC = pid() | object_ref()
%%      State = #df_rtc{}
%%      OwnedEC = {ec_handle(), pid(), pid() | object_ref(), string()}
%% @end
-spec(find_ec_in_owned(EC::pid() | object_ref(), State::#df_rtc{}) ->
        OwnedEC::{ec_handle(), pid(), pid() | object_ref(), ID::string()}).
%%-----------------------------------------------------------------------------
find_ec_in_owned(EC, #df_rtc{owned_ecs=OECs}) when is_pid(EC) ->
    lists:keyfind(EC, 3, OECs);
find_ec_in_owned(EC, #df_rtc{owned_ecs=OECs}) ->
    F = fun({_, _, Ref, _}) when is_pid(Ref) ->
            case ec:get_corba_obj(Ref)
                of EC ->
                    true
                 ; _ ->
                    false
            end;
        ({_, _, Ref, _}) ->
            case Ref
                of EC ->
                    true
                 ; _ ->
                    false
            end
        end,
    case lists:filter(F, OECs)
        of [OEC] ->
            OEC
         ; [] ->
            false
    end.


%%-----------------------------------------------------------------------------
%% @doc Finds an EC in the participants list by object reference or PID.
%% @spec find_ec_in_parts(EC, State) -> Participant
%% where
%%      EC = pid() | object_ref()
%%      State = #df_rtc{}
%%      Participant = {ec_handle(), pid() | object_ref()}
%% @end
-spec(find_ec_in_parts(EC::pid() | object_ref(), State::#df_rtc{}) ->
        Participant::{ec_handle(), pid() | object_ref()}).
%%-----------------------------------------------------------------------------
find_ec_in_parts(EC, #df_rtc{part_ecs=PECs}) when is_pid(EC) ->
    lists:keyfind(EC, 2, PECs);
find_ec_in_parts(EC, #df_rtc{part_ecs=PECs}) ->
    F = fun({_, Ref}) when is_pid(Ref) ->
            case ec:get_corba_obj(Ref)
                of EC ->
                    true
                 ; _ ->
                    false
            end;
        ({_, Ref}) ->
            case Ref
                of EC ->
                    true
                 ; _ ->
                    false
            end
        end,
    case lists:filter(F, PECs)
        of [OEC] ->
            OEC
         ; [] ->
            false
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the PIDs of execution contexts the RTC owns.
%% @spec owned_refs(State) -> [pid() | object_ref()]
%% where
%%      State = #df_rtc{}
%% @end
-spec(owned_refs(State::#df_rtc{}) -> [pid() | object_ref()]).
%%-----------------------------------------------------------------------------
owned_refs(#df_rtc{owned_ecs=ECs}) ->
    ?LOG(rtl_paranoid, "Getting owned EC refs: ~p", [ECs]),
    [Ref || {_Handle, _, Ref, _} <- ECs].


%%-----------------------------------------------------------------------------
%% @doc Get the PIDs of execution contexts the RTC is participating in.
%% @spec part_refs(State) -> [pid() | object_ref()]
%% where
%%      State = #df_rtc{}
%% @end
-spec(part_refs(State::#df_rtc{}) -> [pid() | object_ref()]).
%%-----------------------------------------------------------------------------
part_refs(#df_rtc{part_ecs=ECs}) ->
    ?LOG(rtl_paranoid, "Getting participating EC refs: ~p", [ECs]),
    [Ref || {_Handle, Ref} <- ECs].


%%-----------------------------------------------------------------------------
%% @doc Get a context by its handle.
%% @spec get_ec(Handle, State) -> pid() | none
%% where
%%      Handle = ec_handle()
%%      State = #df_rtc{}
%% @end
-spec(get_ec(Handle::ec_handle(), State::#df_rtc{}) -> pid() | none).
%%-----------------------------------------------------------------------------
get_ec(Handle, #df_rtc{owned_ecs=OECs}=State) ->
    ?LOG(rtl_debug, "Looking for EC for handle ~p in owned ECs ~p",
        [Handle, OECs]),
    case lists:keyfind(Handle, 1, OECs)
        of {Handle, _, EC, _} ->
            ?LOG(rtl_debug, "Found EC: ~p", [EC]),
            EC
         ; false ->
            get_ec1(Handle, State)
    end.

-spec(get_ec1(Handle::ec_handle(), State::#df_rtc{}) -> pid() | none).
get_ec1(Handle, #df_rtc{part_ecs=PECs}) ->
    ?LOG(rtl_debug, "Looking for EC for handle ~p in participating ECs ~p",
        [Handle, PECs]),
    case lists:keyfind(Handle, 1, PECs)
        of {Handle, EC} ->
            ?LOG(rtl_debug, "Found EC: ~p", [EC]),
            EC
         ; false ->
            none
    end.


%%-----------------------------------------------------------------------------
%% @doc Get a context handle by its context PID.
%% @spec get_ec_handle(EC, State) -> ec_handle() | none
%% where
%%      EC = pid()
%%      State = #df_rtc{}
%% @end
-spec(get_ec_handle(EC::pid(), State::#df_rtc{}) -> ec_handle() | none).
%%-----------------------------------------------------------------------------
get_ec_handle(EC, #df_rtc{owned_ecs=OECs}=State) ->
    ?LOG(rtl_debug, "Looking for handle for EC ~p in owned ECs ~p",
        [EC, OECs]),
    case lists:keyfind(EC, 2, OECs)
        of {Handle, _, EC, _} ->
            ?LOG(rtl_debug, "Found handle: ~p", [Handle]),
            Handle
         ; false ->
            get_ec_handle1(EC, State)
    end.

-spec(get_ec_handle1(EC::pid(), State::#df_rtc{}) -> ec_handle() | none).
get_ec_handle1(EC, #df_rtc{part_ecs=PECs}) ->
    ?LOG(rtl_debug, "Looking for handle for EC ~p in participating ECs ~p",
        [EC, PECs]),
    case lists:keyfind(EC, 2, PECs)
        of {Handle, EC} ->
            ?LOG(rtl_debug, "Found handle: ~p", [Handle]),
            Handle
         ; false ->
            none
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle an RTC action event by starting a helper to execute the CB.
%% @spec handle_rtc_action(RTCAction, From, State) -> NewState
%% where
%%      RTCAction = {Action, ec_handle()}
%%      Action = on_execute | on_state_update | on_rate_changed | on_startup |
%%          on_shutdown | on_activated | on_deactivated | on_aborting |
%%          on_error | on_reset
%%      From = pid()
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(handle_rtc_action(RTCAction::{on_execute | on_state_update |
            on_rate_changed | on_startup | on_shutdown | on_activated |
            on_deactivated | on_aborting | on_error | on_reset, ec_handle()},
        From::pid(), State::#df_rtc{}) ->
    NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
handle_rtc_action({Action, EC}, From,
        #df_rtc{port_mgr=PM, mod=M, data=D}=State) ->
    add_helper(fun rtc_action_caller:start_link/2,
        [Action, M, self(), PM, EC, D], From, State).


%%-----------------------------------------------------------------------------
%% @doc Handle a helper crashing.
%% @spec handle_crashed_helper(Helper, Reason, State) -> NewState
%% where
%%      Helper = pid()
%%      Reason = any()
%%      State = #df_rtc{}
%%      NewState = #df_rtc{}
%% @end
-spec(handle_crashed_helper(Helper::pid(), Reason::any(), State::#df_rtc{}) ->
    NewState::#df_rtc{}).
%%-----------------------------------------------------------------------------
handle_crashed_helper(Helper, Reason, State) ->
    case is_valid_helper(Helper, State)
        of {Helper, C} ->
            ?LOG(rtl_error, "Helper ~p crashed with reason ~p",
                [Helper, Reason]),
            reply_caller(C, error),
            del_helper(Helper, State)
         ; false ->
            ?LOG(rtl_error,
                "Unknown process ~p linked to RTC crashed with reason ~p",
                [Helper, Reason]),
            State
    end.


%%-----------------------------------------------------------------------------
%% @doc Make a component profile from the current state.
%% @spec make_comp_prof(State) -> #comp_prof{}
%% where
%%      State = #df_rtc{}
%% @end
-spec(make_comp_prof(State::#df_rtc{}) -> #comp_prof{}).
%%-----------------------------------------------------------------------------
make_comp_prof(#df_rtc{cfg=Cfg, port_mgr=PM}) ->
    #comp_prof{inst_name=config:get_value("instance_name", "no_name", Cfg),
        type_name=config:get_value("type_name", "NoType", Cfg),
        desc=config:get_value("description", "", Cfg),
        ver=config:get_value("version", "0.0", Cfg),
        vendor=config:get_value("vendor", "", Cfg),
        category=config:get_value("category", "", Cfg),
        port_profs=port_mgr:port_profs(PM),
        parent=nil}.


%%-----------------------------------------------------------------------------
%% @doc Make a list of port PIDs.
%% @spec make_ports_list(State) -> Ports
%% where
%%      State = #df_rtc{}
%%      Ports = [pid()]
%% @end
-spec(make_ports_list(State::#df_rtc{}) -> Ports::[pid()]).
%%-----------------------------------------------------------------------------
make_ports_list(#df_rtc{port_mgr=PM}) ->
    port_mgr:get_ports(PM).


%%-----------------------------------------------------------------------------
%% @doc Add a new port.
%% @spec add_port(State, Type, Name, DataType, Config) ->
%%      ok | {error, bad_param}
%% where
%%      State = #df_rtc{}
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Config = config()
%% @end
-spec(add_port(State::#df_rtc{}, Type::port_type(), Name::string(),
        DataType::string(), Config::config()) ->
    ok | {error, bad_param}).
%%-----------------------------------------------------------------------------
add_port(#df_rtc{port_mgr=PM}, Type, Name, DataType, Config) ->
    port_mgr:add_port(PM, Type, Name, DataType, self(), Config).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test adding, checking and removing helpers.
%% @spec helper_test() -> ok
%% @end
-spec(helper_test() -> ok).
%%-----------------------------------------------------------------------------
helper_test() ->
    Ref = make_ref(),
    ?assertMatch(#df_rtc{}, add_helper(fun start_test_helper_proc/2, [args],
            Ref, #df_rtc{mod=none})),
    S = add_helper(fun start_test_helper_proc/2, [args], Ref,
        #df_rtc{mod=none}),
    [{P, _C}=H] = S#df_rtc.helpers,
    ?assertMatch(H, is_valid_helper(P, S)),
    S1 = del_helper(P, S),
    ?assertMatch([], S1#df_rtc.helpers),
    P ! stop,
    S2 = lists:foldl(fun(_N, State) ->
                add_helper(fun start_test_helper_proc/2, [args],
                    self(), State) end,
        #df_rtc{mod=none}, [1, 2, 3]),
    {H2Pid, _} = H2 = lists:nth(2, S2#df_rtc.helpers),
    ?assertMatch(H2, is_valid_helper(H2Pid, S2)),
    S3 = del_helper(H2Pid, S2),
    ?assertMatch(2, length(S3#df_rtc.helpers)),
    ?assertMatch(false, is_valid_helper(H2Pid, S3)),
    H2Pid ! stop,
    lists:foreach(fun({Pid, _}) -> Pid ! stop end,
        S3#df_rtc.helpers).


%%-----------------------------------------------------------------------------
%% @doc Start the helper emulator process.
%% @spec start_test_helper_proc(Helpee, Args) -> ok
%% @end
-spec(start_test_helper_proc(Helpee::pid(), Args::[atom()]) -> ok).
%%-----------------------------------------------------------------------------
start_test_helper_proc(_Helpee, Args) ->
    Pid = spawn_link(dataflow_rtc, test_helper_proc, Args),
    {ok, Pid}.

%%-----------------------------------------------------------------------------
%% @doc Simple process to emulator a helper for tests.
%% @spec test_helper_proc(args) -> ok
%% @end
-spec(test_helper_proc(args) -> ok).
%%-----------------------------------------------------------------------------
test_helper_proc(args) ->
    receive
        stop ->
            ok
    end.

-endif. % TEST

