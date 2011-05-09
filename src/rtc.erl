%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc RT Component Erlang interface.
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
-module(rtc).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("rtc.hrl").
-include("type_specs.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Start here
-export([create/3, destroy/1]).
% LightweightRTObject interface
-export([is_alive/2, exit/1, get_context/2, get_owned_contexts/1,
        get_participating_contexts/1, get_context_handle/2, type/1]).
% DataFlowComponentAction interface
-export([on_execute/2, on_state_update/2, on_rate_changed/2]).
% Operation events
-export([on_startup/2, on_shutdown/2, on_activated/2, on_deactivated/2,
        on_aborting/2, on_error/2, on_reset/2]).
% Introspection interface
-export([get_component_profile/1, get_ports/1]).
% Port management
-export([add_port/5]).
% Other useful functions.
-export([set_sup/2, get_corba_obj/1, event/2, register_cb/3,
        unregister_cb/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([initialize/1, finalize/1, attach_context/2, detach_context/2]).
% Used for testing only
-export([shift_to_state/2, get_behv_data/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new RT Component, including supervisor, FSM, CORBA interface
%% and other supporting processes.
%% @spec create(InstName, Config, Module) ->
%%      {ok, Supervisor, FSM} | {error, Reason}
%% where
%%      InstName = string()
%%          The unique name used to identify the component instance.
%%      Config = config()
%%          The configuration of the component. This should also include any
%%          configuration of execution contexts that will be owned by this RTC.
%%      Module = atom()
%%          The module providing the functionality of the component.
%%      Supervisor = pid()
%%          The top-level supervisor of the EC's personal supervision tree.
%%          You will need this to place the EC into a parent supervisor.
%%      FSM = pid()
%%          The FSM implementing the EC behaviour. Pass this to the functions
%%          exported by this module.
%%      Reason = any()
%% @end
-spec(create(InstName::string(), Config::config(), Module::atom()) ->
    {ok, Supervisor::pid(), FSM::pid()}).
%%-----------------------------------------------------------------------------
create(Config, SuffixGen, Module) ->
    InstName = config:get_value("type_name", Config) ++ SuffixGen(),
    Config1 = config:set_value("instance_name", InstName, Config),
    NameFormat = config:get_value(["naming", "formats"], "%n.rtc", Config1),
    FullName = naming:format_name(NameFormat, Config1),
    case rtc_sup:start_link(FullName, Config1, Module)
        of {ok, S} ->
            Children = supervisor:which_children(S),
            {config_svr, CfgSvr, worker, _} = lists:keyfind(config_svr, 1,
                Children),
            {fsm, FSM, worker, _} = lists:keyfind(fsm, 1, Children),
            {ports_sup, PS, supervisor, _} = lists:keyfind(ports_sup, 1,
                Children),
            {port_mgr, PM, worker, _} = lists:keyfind(port_mgr, 1,
                Children),
            port_mgr:set_ports_sup(PM, PS),
            {ecs_sup, ECs, supervisor, _} = lists:keyfind(ecs_sup, 1,
                Children),
            {cfg_corba, Cfg_Corba, worker, _} = lists:keyfind(cfg_corba, 1,
                Children),
            {corba, CORBA, worker, _} = lists:keyfind(corba, 1, Children),
            set_cfg_svr(FSM, CfgSvr),
            set_port_mgr(FSM, PM),
            set_ecs_sup(FSM, ECs),
            set_sup(FSM, S),
            corba_obj_mgr:send_info(Cfg_Corba, {set_target, CfgSvr}),
            corba_obj_mgr:send_info(CORBA, {set_target, FSM}),
            corba_obj_mgr:send_info(CORBA, {set_cfg_target, Cfg_Corba}),
            {ok, S, FSM}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Destroy an RT Component, from its supervisor on down.
%% @spec destroy(S) -> ok
%% where
%%      S = pid()
%%          PID of the RTC's top supervisor, as returned by create/1.
%% @end
-spec(destroy(S::pid()) -> ok).
%%-----------------------------------------------------------------------------
destroy(S) ->
    true = exit(S, normal),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Test if an RTC is alive. This only tests if the RTC is in one of the
%% lifecycle states (inactive, active, error) in the given execution context.
%% Query the EC directly to determine which of those states the RTC is in
%% within that context.
%% @spec is_alive(RTC, EC) -> boolean()
%% where
%%      RTC = pid()
%%      EC = pid()
%% @end
-spec(is_alive(RTC::pid(), EC::pid()) -> boolean()).
%%-----------------------------------------------------------------------------
is_alive(RTC, EC) ->
    gen_fsm:sync_send_event(RTC, {is_alive, EC}).


%%-----------------------------------------------------------------------------
%% @doc Stop all owned execution contexts, deactivate in all participating
%% contexts, and finalise the RTC. The RTC must be initialised first.
%% @spec exit(RTC) -> ok | precon_not_met
%% where
%%      RTC = pid()
%% @end
-spec(exit(RTC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
exit(RTC) ->
    gen_fsm:sync_send_event(RTC, exit).


%%-----------------------------------------------------------------------------
%% @doc Get the execution context represented by the given handle.
%% @spec get_context(RTC, Handle) -> EC
%% where
%%      RTC = pid()
%%      Handle = ec_handle()
%%      EC = pid() | object_ref()
%% @end
-spec(get_context(RTC::pid(), Handle::ec_handle()) ->
    EC::pid() | object_ref()).
%%-----------------------------------------------------------------------------
get_context(RTC, Handle) ->
    gen_fsm:sync_send_all_state_event(RTC, {get_context, Handle}).


%%-----------------------------------------------------------------------------
%% @doc Get the execution contexts owned by the RTC.
%% @spec get_owned_contexts(RTC) -> OwnedECs
%% where
%%      RTC = pid()
%%      OwnedECs = [pid() | object_ref()]
%% @end
-spec(get_owned_contexts(RTC::pid()) -> OwnedECs::[pid() | object_ref()]).
%%-----------------------------------------------------------------------------
get_owned_contexts(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_owned_ecs).


%%-----------------------------------------------------------------------------
%% @doc Get the execution contexts the RTC is participating in.
%% @spec get_participating_contexts(RTC) -> ECs
%% where
%%      RTC = pid()
%%      ECs = [pid() | object_ref()]
%% @end
-spec(get_participating_contexts(RTC::pid()) -> ECs::[pid() | object_ref()]).
%%-----------------------------------------------------------------------------
get_participating_contexts(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_part_ecs).


%%-----------------------------------------------------------------------------
%% @doc Get the handle associated with an execution context within the RTC.
%% @spec get_context_handle(RTC, EC) -> ec_handle()
%% where
%%      RTC = pid()
%%      EC = pid() | object_ref()
%% @end
-spec(get_context_handle(RTC::pid(), EC::pid() | object_ref()) -> ec_handle()).
%%-----------------------------------------------------------------------------
get_context_handle(RTC, EC) ->
    gen_fsm:sync_send_all_state_event(RTC, {get_ec_handle, EC}).


%%-----------------------------------------------------------------------------
%% @doc Get the type of component. Typically this will be dataflow or fsm, but
%% other types may be available.
%% @spec type(RTC) -> atom()
%% where
%%      RTC = pid() | object_ref()
%% @end
-spec(type(RTC::pid() | object_ref()) -> atom()).
%%-----------------------------------------------------------------------------
type(RTC) when is_pid(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_type);
type({'OpenRTM_DataFlowComponent', _, _, _, _, _}) ->
    dataflow.


%%-----------------------------------------------------------------------------
%% @doc Send an execute event to trigger the first execution stage of the RTC.
%% @spec on_execute(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_execute(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_execute(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_execute, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a state_update event to trigger the second execution stage of the
%% RTC.
%% @spec on_state_update(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_state_update(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_state_update(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_state_update, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a rate_changed event to trigger the rate_changed action of the
%% RTC.
%% @spec on_rate_changed(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_rate_changed(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_rate_changed(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_rate_changed, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a startup event to trigger the startup stage of the RTC.
%% @spec on_startup(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_startup(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_startup(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_startup, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a shutdown event to trigger the shutdown stage of the RTC.
%% @spec on_shutdown(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_shutdown(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_shutdown(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_shutdown, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send an activate event to trigger the activation stage of the RTC.
%% @spec on_activated(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_activated(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_activated(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_activated, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a deactivate event to trigger the deactivate stage of the RTC.
%% @spec on_deactivated(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_deactivated(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_deactivated(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_deactivated, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send an aborting event to trigger the aborting stage of the RTC.
%% @spec on_aborting(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_aborting(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_aborting(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_aborting, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send an error event to trigger the error stage of the RTC.
%% @spec on_error(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_error(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_error(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_error, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Send a reset event to trigger the reset stage of the RTC.
%% @spec on_reset(RTC, ECHandle) -> rtc_cb_return()
%% where
%%      RTC = pid()
%%      ECHandle = ec_handle()
%% @end
-spec(on_reset(RTC::pid(), ECHandle::ec_handle()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
on_reset(RTC, ECHandle) ->
    gen_fsm:sync_send_event(RTC, {on_reset, ECHandle}).


%%-----------------------------------------------------------------------------
%% @doc Get the profile of the RTC.
%% @spec get_component_profile(RTC) -> #comp_prof{}
%% where
%%      RTC = pid()
%% @end
-spec(get_component_profile(RTC::pid()) -> #comp_prof{}).
%%-----------------------------------------------------------------------------
get_component_profile(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_comp_prof).


%%-----------------------------------------------------------------------------
%% @doc Get the ports from the RTC.
%% @spec get_ports(RTC) -> Ports
%% where
%%      RTC = pid()
%%      Ports = [pid()]
%% @end
-spec(get_ports(RTC::pid()) -> Ports::[pid()]).
%%-----------------------------------------------------------------------------
get_ports(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_ports).


%%-----------------------------------------------------------------------------
%% @doc Add a new port.
%% @spec add_port(RTC, Type, Name, DataType, Config) ->
%%      ok | {error, bad_param}
%% where
%%      RTC = pid()
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Config = config()
%% @end
-spec(add_port(RTC::pid(), Type::port_type(), Name::string(),
        DataType::string(), Config::config()) ->
    ok | {error, bad_param}).
%%-----------------------------------------------------------------------------
add_port(RTC, Type, Name, DataType, Config) ->
    gen_server:call(RTC, {add_port, Type, Name, DataType, Config}).


%%-----------------------------------------------------------------------------
%% @doc Set the PID of the supervisor for the RTC.
%% @spec set_sup(RTC, Pid) -> ok
%% where
%%      RTC = pid()
%%      Pid = pid()
%% @end
-spec(set_sup(RTC::pid(), Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_sup(RTC, Pid) ->
    gen_fsm:sync_send_all_state_event(RTC, {set_sup, Pid}).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object providing the interface for an RTC.
%% @spec get_corba_obj(RTC) -> {Pid, Obj}
%% where
%%      RTC = pid()
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(get_corba_obj(RTC::pid()) -> {Pid::pid(), Obj::object_ref()}).
%%-----------------------------------------------------------------------------
get_corba_obj(RTC) ->
    gen_fsm:sync_send_all_state_event(RTC, get_corba_obj).


%%-----------------------------------------------------------------------------
%% @doc Send an event to an RT Component FSM.
%% @spec event(RTC, Event) -> ok.
%% where
%%      RTC = pid()
%%      Event = atom()
%% @end
-spec(event(RTC::pid(), Event::atom()) -> ok).
%%-----------------------------------------------------------------------------
event(RTC, Event) ->
    gen_fsm:sync_send_all_state_event(RTC, Event).


%%-----------------------------------------------------------------------------
%% @doc Register a function to be called when an event occurs. Allowed events
%% are:
%% - exited: The RTC has finished executing the exit() function.
%% @spec register_cb(RTC, Event, Fun) -> {ok, Ref} | {error, bad_event}.
%% where
%%      RTC = pid()
%%      Event = exited
%%      Fun = fun()
%%      Ref = reference()
%%          Unique reference to identify the callback.
%% @end
-spec(register_cb(RTC::pid(), Event::exited, Fun::fun()) ->
    {ok, Ref::reference()} | {error, bad_event}).
%%-----------------------------------------------------------------------------
register_cb(RTC, Event, Fun) ->
    gen_fsm:sync_send_all_state_event(RTC, {reg_cb, Event, Fun}).


%%-----------------------------------------------------------------------------
%% @doc Unregister a callback.
%% @spec unregister_cb(RTC, Ref) -> ok | {error, no_cb}.
%% where
%%      RTC = pid()
%%      Ref = reference()
%%          Unique reference to identify the callback.
%% @end
-spec(unregister_cb(RTC::pid(), Ref::reference()) -> ok | no_cb).
%%-----------------------------------------------------------------------------
unregister_cb(RTC, Ref) ->
    gen_fsm:sync_send_all_state_event(RTC, {unreg_cb, Ref}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise an RTC. The on_initialize/0 callback will be called.
%% Do not call this function directly.
%% @spec initialize(RTC) -> ok | precon_not_met
%% where
%%      RTC = pid()
%% @end
-spec(initialize(RTC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
initialize(RTC) ->
    gen_fsm:sync_send_event(RTC, initialise).


%%-----------------------------------------------------------------------------
%% @doc Finalise an RTC so it can be destroyed.. The on_finalize/0 callback
%% will be called. Do not call this function directly.
%% @spec finalize(RTC) -> ok | precon_not_met
%% where
%%      RTC = pid()
%% @end
-spec(finalize(RTC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
finalize(RTC) ->
    gen_fsm:sync_send_event(RTC, finalise).


%%-----------------------------------------------------------------------------
%% @doc Attach the RTC to an execution context. The RTC will create and return
%% a handle to the EC for this RTC. Do not call this function directly.
%% @spec attach_context(RTC, EC) -> ec_handle()
%% where
%%      RTC = pid()
%%      EC = pid()
%% @end
-spec(attach_context(RTC::pid(), EC::pid()) -> ec_handle()).
%%-----------------------------------------------------------------------------
attach_context(RTC, EC) ->
    gen_fsm:sync_send_all_state_event(RTC, {attach_ec, EC}).


%%-----------------------------------------------------------------------------
%% @doc Detach the RTC from an execution context in which it is participating.
%% The RTC must first be deactivated in the EC. Do not call this function
%% directly.
%% @spec detach_context(RTC, EC) -> ok | precon_not_met
%% where
%%      RTC = pid()
%%      EC = pid()
%% @end
-spec(detach_context(RTC::pid(), EC::pid()) -> ok | precon_not_met).
%%-----------------------------------------------------------------------------
detach_context(RTC, EC) ->
    gen_fsm:sync_send_all_state_event(RTC, {detach_ec, EC}).


%%-----------------------------------------------------------------------------
%% @doc Force the RTC FSM into a certain state.
%% @spec shift_to_state(FSM, State) -> ok
%% where
%%      FSM = pid()
%%      State = atom()
%% @end
-spec(shift_to_state(FSM::pid(), State::atom()) -> ok).
%%-----------------------------------------------------------------------------
shift_to_state(FSM, State) ->
    gen_fsm:sync_send_all_state_event(FSM, {shift_to, State}).


%%-----------------------------------------------------------------------------
%% @doc Get the current behaviour data.
%% @spec get_behv_data(FSM) -> ok
%% where
%%      FSM = pid()
%% @end
-spec(get_behv_data(FSM::pid()) -> ok).
%%-----------------------------------------------------------------------------
get_behv_data(FSM) ->
    gen_fsm:sync_send_all_state_event(FSM, get_behv_data).


%%-----------------------------------------------------------------------------
%% @doc Set the server providing configuration parameter services.
%% @spec set_cfg_svr(RTC, CfgSvr) -> ok
%% where
%%      RTC = pid()
%%      CfgSvr = pid()
%% @end
-spec(set_cfg_svr(RTC::pid(), CfgSvr::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_cfg_svr(RTC, CfgSvr) ->
    gen_fsm:sync_send_all_state_event(RTC, {set_cfg_svr, CfgSvr}).


%%-----------------------------------------------------------------------------
%% @doc Set the supervisor managing ports.
%% @spec set_port_mgr(RTC, Mgr) -> ok
%% where
%%      RTC = pid()
%%      Mgr = pid()
%% @end
-spec(set_port_mgr(RTC::pid(), Mgr::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_port_mgr(RTC, Mgr) ->
    gen_fsm:sync_send_all_state_event(RTC, {set_port_mgr, Mgr}).


%%-----------------------------------------------------------------------------
%% @doc Set the supervisor managing execution contexts.
%% @spec set_ecs_sup(RTC, Sup) -> ok
%% where
%%      RTC = pid()
%%      Sup = pid()
%% @end
-spec(set_ecs_sup(RTC::pid(), Sup::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_ecs_sup(RTC, Sup) ->
    gen_fsm:sync_send_all_state_event(RTC, {set_ecs_sup, Sup}).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

