%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc CORBA wrapper around the data flow component interfaces.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @source idl/RTC.idl
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
-module('OpenRTM_DataFlowComponent_impl').

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("idl/OpenRTM.hrl").
-include("idl/RTC.hrl").
-include("idl/SDOPackage.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("rtc.hrl").

%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(dfc_svc, {target=nil :: pid() | nil, % Target process for the interface
        cfg_target=nil :: pid() | nil, % Target configuration object
        name :: string(), % Name to register with
        nc :: object_ref() % Naming context to which the RTC is registered
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop_intf/1]).
% RTC::RTObject interface
-export([get_component_profile/2, get_ports/2]).
% RTC::LightweightRTObject interface
-export([initialize/2, finalize/2, is_alive/3, exit/2, attach_context/3,
        detach_context/3, get_context/3, get_owned_contexts/2,
        get_participating_contexts/2, get_context_handle/3]).
% RTC::ComponentAction interface
-export([on_initialize/2, on_finalize/2, on_startup/3, on_shutdown/3,
        on_activated/3, on_deactivated/3, on_aborting/3, on_error/3,
        on_reset/3]).
% SDOPackage::SDO interface
-export([get_sdo_id/2, get_sdo_type/2, get_device_profile/2,
        get_service_profiles/2, get_service_profile/3, get_sdo_service/3,
        get_configuration/2, get_monitoring/2, get_organizations/2,
        get_status_list/2, get_status/3]).
% SDOPackage::SDOSystemElement interface
-export([get_owned_organizations/2]).
% RTC::DataFlowComponentAction interface
-export([on_execute/3, on_state_update/3, on_rate_changed/3]).

%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_info/2, terminate/2, code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the CORBA DataFlowComponent interface.
%% @spec start_link({FullName, Config}) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      FullName = string()
%%          The name to register on the name server under.
%%      Config = config()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link({FullName::string(), Config::config()}) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link({FullName, Config}) ->
    NSAddress = config:get_value(["corba", "nameservers"], "localhost:2809",
        Config),
    {ok, Pid, Obj} = 'OpenRTM_DataFlowComponent':oe_create_link({FullName,
            NSAddress}, [{sup_child, true}]),
    Result = register_on_ns(FullName, NSAddress, Obj),
    {Result, Pid, Obj}.


%%-----------------------------------------------------------------------------
%% @doc Stop the CORBA DataFlowComponent interface.
%% @spec stop_intf(Pid) -> ok
%% where
%%      Pid = pid(
%%          The PID of the process running the interface.
%% @end
-spec(stop_intf(Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_intf(Pid) ->
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------------------
%% @doc Get the profile of the RTC.
%% @spec get_component_profile(This, State) ->
%%      {reply, #'RTC_ComponentProfile'{}, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(get_component_profile(This::object_ref(), State::#dfc_svc{}) ->
    {reply, #'RTC_ComponentProfile'{}, NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_component_profile(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_component_profile called."),
    Prof = rtc:get_component_profile(T),
    ?LOG(rtl_paranoid, "Raw profile is ~p", [Prof]),
    Parent = case Prof#comp_prof.parent
        of nil ->
            ?ORBER_NIL_OBJREF
         ; Pid ->
         rtc:get_corba_obj(Pid)
    end,
    Result = #'RTC_ComponentProfile'{instance_name=Prof#comp_prof.inst_name,
        type_name=Prof#comp_prof.type_name,
        description=Prof#comp_prof.desc,
        version=Prof#comp_prof.ver,
        vendor=Prof#comp_prof.vendor,
        category=Prof#comp_prof.category,
        port_profiles=lists:map(fun port_prof_corbafier/1,
            Prof#comp_prof.port_profs),
        parent=Parent,
        properties=nvlist:from_list(Prof#comp_prof.props)
    },
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Convert an Erlang port profile to the CORBA format.
%% @spec port_prof_corbafier(#port_prof{}) -> #'RTC_PortProfile'{}
%% @end
-spec(port_prof_corbafier(#port_prof{}) -> #'RTC_PortProfile'{}).
%%-----------------------------------------------------------------------------
port_prof_corbafier(Prof) ->
    Owner = case Prof#port_prof.owner
        of nil ->
            ?ORBER_NIL_OBJREF
         ; Pid ->
            rtc:get_corba_obj(Pid)
    end,
    #'RTC_PortProfile'{name=Prof#port_prof.name,
        interfaces=lists:map(fun port_intf_corbafier/1,
            Prof#port_prof.interfaces),
        port_ref=port:get_corba_obj(Prof#port_prof.port_ref),
        connector_profiles=lists:map(fun conn_prof_corbafier/1,
            Prof#port_prof.conn_profs),
        owner=Owner,
        properties=nvlist:from_list(Prof#port_prof.props)
    }.


%%-----------------------------------------------------------------------------
%% @doc Convert an Erlang port interface profile to the CORBA format.
%% @spec port_intf_corbafier(#port_intf_prof{}) ->
%%      #'RTC_PortInterfaceProfile'{}
%% @end
-spec(port_intf_corbafier(#port_intf_prof{}) -> #'RTC_PortInterfaceProfile'{}).
%%-----------------------------------------------------------------------------
port_intf_corbafier(Prof) ->
    Polarity = case Prof#port_intf_prof.polarity
        of provided ->
            'PROVIDED'
         ; required ->
            'REQUIRED'
    end,
    #'RTC_PortInterfaceProfile'{instance_name=Prof#port_intf_prof.inst_name,
        type_name=Prof#port_intf_prof.type_name,
        polarity=Polarity
    }.


%%-----------------------------------------------------------------------------
%% @doc Convert an Erlang connector profile to the CORBA format.
%% @spec conn_prof_corbafier(#conn_prof{}) -> #'RTC_ConnectorProfile'{}
%% @end
-spec(conn_prof_corbafier(#conn_prof{}) -> #'RTC_ConnectorProfile'{}).
%%-----------------------------------------------------------------------------
conn_prof_corbafier(Prof) ->
    ConnPortCorbafier = fun(Port) when is_pid(Port) ->
            port:get_corba_obj(Port);
        (Port) ->
            Port
    end,
    #'RTC_ConnectorProfile'{name=Prof#conn_prof.name,
        connector_id=Prof#conn_prof.id,
        ports=lists:map(ConnPortCorbafier, Prof#conn_prof.ports),
        properties=nvlist:from_list(Prof#conn_prof.props)
    }.


%%-----------------------------------------------------------------------------
%% @doc Get the list of ports on the RTC.
%% @spec get_ports(This, State) ->
%%      {reply, [PortObj], NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      PortObj = object_ref()
%% @end
-spec(get_ports(This::object_ref(), State::#dfc_svc{}) ->
    {reply, [PortObj::object_ref()], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_ports(_OE_This, State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_ports called."),
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Initialise the RTC.
%% @spec initialize(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(initialize(This::object_ref(), State::#dfc_svc{}) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
initialize(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:initialize called."),
    {reply, utils:to_rtc_rc(rtc:initialize(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Finalise the RTC.
%% @spec finalize(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(finalize(This::object_ref(), State::#dfc_svc{}) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
finalize(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:finalize called."),
    {reply, utils:to_rtc_rc(rtc:finalize(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Check if the RTC is alive in an execution context.
%% @spec is_alive(This, State, EC) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      EC = object_ref()
%% @end
-spec(is_alive(This::object_ref(), State::#dfc_svc{}, EC::object_ref()) ->
    {reply, boolean(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
is_alive(_OE_This, #dfc_svc{target=T}=State, EC) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:is_alive called."),
    {reply, rtc:is_alive(T, EC), State}.


%%-----------------------------------------------------------------------------
%% @doc Stop the RTC.
%% @spec exit(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(exit(This::object_ref(), State::#dfc_svc{}) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
exit(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:exit called."),
    {reply, utils:to_rtc_rc(rtc:exit(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Attach the RTC to an execution context.
%% @spec attach_context(This, State, EC) ->
%%      {reply, ECHandle, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      EC = object_ref()
%%      ECHandle = integer()
%% @end
-spec(attach_context(This::object_ref(), State::#dfc_svc{}, EC::object_ref) ->
    {reply, ECHandle::integer(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
attach_context(_OE_This, #dfc_svc{target=T}=State, EC) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:attach_context called."),
    {reply, rtc:attach_context(T, EC), State}.


%%-----------------------------------------------------------------------------
%% @doc Detach the RTC from an execution context.
%% @spec detach_context(This, State, EC) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      EC = object_ref()
%% @end
-spec(detach_context(This::object_ref(), State::#dfc_svc{},
        EC::object_ref()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
detach_context(_OE_This, #dfc_svc{target=T}=State, EC) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:detach_context called."),
    {reply, utils:to_rtc_rc(rtc:detach_context(T, EC)), State}.


%%-----------------------------------------------------------------------------
%% @doc Get the execution context represented by the given handle.
%% @spec get_context(This, State, ECHandle) ->
%%      {reply, EC, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%%      EC = object_ref()
%% @end
-spec(get_context(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, EC::object_ref(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_context(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_context called."),
    {reply, corbafy_ec_ref(rtc:get_context(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of all execution contexts owned by the RTC.
%% @spec get_owned_contexts(This, State) ->
%%      {reply, ECList, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECList = [object_ref()]
%% @end
-spec(get_owned_contexts(This::object_ref(), State::#dfc_svc{}) ->
    {reply, ECList::[object_ref()], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_owned_contexts(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_owned_contexts called."),
    {reply, lists:map(fun corbafy_ec_ref/1, rtc:get_owned_contexts(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of all execution contexts the RTC is participating in.
%% @spec get_participating_contexts(This, State) ->
%%      {reply, ECList, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECList = [object_ref()]
%% @end
-spec(get_participating_contexts(This::object_ref(), State::#dfc_svc{}) ->
    {reply, ECList::[object_ref()], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_participating_contexts(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_participating_contexts called."),
    {reply, lists:map(fun corbafy_ec_ref/1, rtc:get_participating_contexts(T)),
        State}.


%%-----------------------------------------------------------------------------
%% @doc Get the handle for an execution context.
%% @spec get_context_handle(This, State) ->
%%      {reply, ECHandle, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(get_context_handle(This::object_ref(), State::#dfc_svc{},
        EC::object_ref()) ->
    {reply, ECHandle::integer(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_context_handle(_OE_This, #dfc_svc{target=T}=State, EC) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_context_handle called."),
    {reply, rtc:get_context_handle(T, EC), State}.


%%-----------------------------------------------------------------------------
%% @doc Called as the RTC enters the alive state.
%% @spec on_initialize(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(on_initialize(This::object_ref(), State::#dfc_svc{}) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_initialize(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_initialize called."),
    {reply, utils:to_rtc_rc(rtc:on_initialize(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC is destroyed.
%% @spec on_finalize(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(on_finalize(This::object_ref(), State::#dfc_svc{}) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_finalize(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_finalize called."),
    {reply, utils:to_rtc_rc(rtc:on_finalize(T)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the execution context enters the running state.
%% @spec on_startup(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_startup(This::object_ref(), State::#dfc_svc{}, ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_startup(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_startup called."),
    {reply, utils:to_rtc_rc(rtc:on_startup(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the execution context enters the stopped state.
%% @spec on_shutdown(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_shutdown(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_shutdown(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_shutdown called."),
    {reply, utils:to_rtc_rc(rtc:on_shutdown(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC is activated in the execution context.
%% @spec on_activated(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_activated(This::object_ref(), State::#dfc_svc{}, ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_activated(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_activated called."),
    {reply, utils:to_rtc_rc(rtc:on_activated(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC is deactivated in the execution context.
%% @spec on_deactivated(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_deactivated(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_deactivated(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_deactivated called."),
    {reply, utils:to_rtc_rc(rtc:on_deactivated(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC moves to the error state in the execution context.
%% @spec on_aborting(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_aborting(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_aborting(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_aborting called."),
    {reply, utils:to_rtc_rc(rtc:on_aborting(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC is in the error state during execution context
%% cycling.
%% @spec on_error(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_error(This::object_ref(), State::#dfc_svc{}, ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_error(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_error called."),
    {reply, utils:to_rtc_rc(rtc:on_error(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called when the RTC attempts to leave the error state.
%% @spec on_reset(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_reset(This::object_ref(), State::#dfc_svc{}, ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_reset(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_reset called."),
    {reply, utils:to_rtc_rc(rtc:on_reset(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Returns the instance name of the RTC.
%% @spec get_sdo_id(This, State) ->
%%      {reply, string(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_sdo_id(This::object_ref(), State::#dfc_svc{}) ->
    {reply, string(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_sdo_id(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_sdo_id called."),
    Prof = rtc:get_component_profile(T),
    {reply, Prof#comp_prof.inst_name, State}.


%%-----------------------------------------------------------------------------
%% @doc Returns the RTC description.
%% @spec get_sdo_type(This, State) ->
%%      {reply, string(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_sdo_type(This::object_ref(), State::#dfc_svc{}) ->
    {reply, string(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_sdo_type(_OE_This, #dfc_svc{target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_sdo_type called."),
    Prof = rtc:get_component_profile(T),
    {reply, Prof#comp_prof.type_name, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the RTC's device profile.
%% @spec get_device_profile(This, State) ->
%%      {reply, #'SDOPackage_DeviceProfile'{}, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_device_profile(This::object_ref(), State::#dfc_svc{}) ->
    {reply, #'SDOPackage_DeviceProfile'{}, NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_device_profile(_OE_This, _State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_device_profile called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the RTC's service profiles.
%% @spec get_service_profiles(This, State) ->
%%      {reply, [#'SDOPackage_ServiceProfile'{}], NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_service_profiles(This::object_ref(), State::#dfc_svc{}) ->
    {reply, [#'SDOPackage_ServiceProfile'{}], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_service_profiles(_OE_This, _State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_service_profiles called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get a specific service profile.
%% @spec get_service_profile(This, State, ID) ->
%%      {reply, #'SDOPackage_ServiceProfile'{}, NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ID = string()
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_service_profile(This::object_ref(), State::#dfc_svc{},
        ID::string()) ->
    {reply, #'SDOPackage_ServiceProfile'{}, NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_service_profile(_OE_This, _State, _ID) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_service_profile called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the specified SDO service object.
%% @spec get_sdo_service(This, State, ID) ->
%%      {reply, object_ref(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ID = string()
%% @throws SDO::InvalidParameter, SDOPackage::NotAvailable,
%%      SDOPackage::InternalError
%% @end
-spec(get_sdo_service(This::object_ref(), State::#dfc_svc{}, ID::string()) ->
    {reply, object_ref(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_sdo_service(_OE_This, _State, _ID) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_sdo_service called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the configuration object.
%% @spec get_configuration(This, State) ->
%%      {reply, object_ref(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::InterfaceNotImplemented, SDOPackage::NotAvailable,
%%      SDOPackage::InternalError
%% @end
-spec(get_configuration(This::object_ref(), State::#dfc_svc{}) ->
    {reply, object_ref(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration(_OE_This, #dfc_svc{cfg_target=T}=State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_configuration called."),
    {reply, corba_obj_mgr:get_obj(T), State}.


%%-----------------------------------------------------------------------------
%% @doc Get the monitoring object.
%% @spec get_monitoring(This, State) ->
%%      {reply, object_ref(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::InterfaceNotImplemented, SDOPackage::NotAvailable,
%%      SDOPackage::InternalError
%% @end
-spec(get_monitoring(This::object_ref(), State::#dfc_svc{}) ->
    {reply, object_ref(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_monitoring(_OE_This, _State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_monitoring called."),
    corba:raise(#'SDOPackage_InterfaceNotImplemented'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the organisation list.
%% @spec get_organizations(This, State) ->
%%      {reply, [object_ref()], NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_organizations(This::object_ref(), State::#dfc_svc{}) ->
    {reply, [object_ref()], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_organizations(_OE_This, _State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_organizations called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the status list.
%% @spec get_status_list(This, State) ->
%%      {reply, [#'SDOPackage_NameValue'{}], NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_status_list(This::object_ref(), State::#dfc_svc{}) ->
    {reply, [#'SDOPackage_NameValue'{}], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_status_list(_OE_This, State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_status_list called."),
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Get a specific status.
%% @spec get_status(This, State, Name) ->
%%      {reply, any(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      Name = string()
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable,
%%      SDOPackage::InternalError
%% @end
-spec(get_status(This::object_ref(), State::#dfc_svc{}, Name::string()) ->
    {reply, any(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_status(_OE_This, _State, _Name) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_status called."),
    corba:raise(#'SDOPackage_InvalidParameter'{description=""}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of owned organisations
%% @spec get_owned_organizations(This, State) ->
%%      {reply, [object_ref()], NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_owned_organizations(This::object_ref(), State::#dfc_svc{}) ->
    {reply, [object_ref()], NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
get_owned_organizations(_OE_This, _State) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:get_owned_organizations called."),
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Invoked by execution contexts periodically.
%% @spec on_execute(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_execute(This::object_ref(), State::#dfc_svc{}, ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_execute(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_execute called."),
    {reply, utils:to_rtc_rc(rtc:on_execute(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Invoked by execution contexts periodically.
%% @spec on_state_update(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_state_update(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_state_update(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_state_update called."),
    {reply, utils:to_rtc_rc(rtc:on_state_update(T, ECHandle)), State}.


%%-----------------------------------------------------------------------------
%% @doc Called by the execution context when its execution rate changes.
%% @spec on_rate_changed(This, State, ECHandle) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%%      ECHandle = integer()
%% @end
-spec(on_rate_changed(This::object_ref(), State::#dfc_svc{},
        ECHandle::integer()) ->
    {reply, rtc_returncode(), NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
on_rate_changed(_OE_This, #dfc_svc{target=T}=State, ECHandle) ->
    ?LOG(rtl_paranoid,
        "OpenRTM_DataFlowComponent_impl:on_rate_changed called."),
    {reply, utils:to_rtc_rc(rtc:on_rate_changed(T, ECHandle)), State}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init({Name, Address}) -> {ok, State} | {stop, Reason}
%% where
%%      Name = string()
%%      Address = string()
%%      State = #dfc_svc{}
%%      Reason = any()
%% @end
-spec(init({Name::string(), Address::string()}) ->
    {ok, State::#dfc_svc{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({Name, Address}) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_info, "Initialising CORBA DataFlowComponent interface."),
    ?LOG(rtl_debug, "Naming context address is ~p", [Address]),
    ?LOG(rtl_debug, "Full address is ~p", [naming:full_address(Address)]),
    NC = corba:string_to_object(naming:full_address(Address)),
    %NC = corba:resolve_initial_references("NameService"),
    {ok, #dfc_svc{name=Name, nc=NC}}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = {set_target, Target} | {set_cfg_target, Target}
%%      Target = pid()
%%      State = #dfc_svc{}
%%      NewState = #dfc_svc{}
%% @end
-spec(handle_info(Info::{set_target, Target::pid()} |
        {set_cfg_target, Target::pid()}, State::#dfc_svc{}) ->
    {noreply, NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
handle_info({set_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received DataFlowComponent target: ~p", [Target]),
    {noreply, State#dfc_svc{target=Target}};
handle_info({set_cfg_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received Configuration target: ~p", [Target]),
    {noreply, State#dfc_svc{cfg_target=Target}}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #dfc_svc{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#dfc_svc{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, #dfc_svc{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shutting down normally."),
    naming:deregister_below_nc(NC, Name);
terminate(shutdown, #dfc_svc{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    naming:deregister_below_nc(NC, Name);
terminate({shutdown, Reason}, #dfc_svc{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    naming:deregister_below_nc(NC, Name);
terminate(Reason, #dfc_svc{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    naming:deregister_below_nc(NC, Name).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #dfc_svc{}
%%      Extra = any()
%%      NewState = #dfc_svc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#dfc_svc{},
        Extra::any()) -> {ok, NewState::#dfc_svc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Ensure an EC reference is the object reference version.
%% @spec corbafy_ec_ref(pid() | object_ref()) -> object_ref()
%% @end
-spec(corbafy_ec_ref(EC::pid() | object_ref()) -> object_ref()).
%%-----------------------------------------------------------------------------
corbafy_ec_ref(EC) when is_pid(EC) ->
    ec:get_corba_obj(EC);
corbafy_ec_ref(EC) ->
    EC.


%%-----------------------------------------------------------------------------
%% @doc Register an RTC object on the name service.
%% @spec register_on_ns(Name, Address, Obj) -> ok
%% where
%%      Name = string()
%%          The name to register under.
%%      Address = string()
%%          The address of the server to register on.
%%      Obj = object_ref()
%% @end
-spec(register_on_ns(Name::string(), Address::string(), Obj::object_ref()) ->
    ok).
%%-----------------------------------------------------------------------------
register_on_ns(Name, Address, Obj) ->
    ?LOG(rtl_info,
        "Registering data flow RTC CORBA interface as ~s on server ~s",
        [Name, Address]),
    NS = corba:string_to_object(naming:full_address(Address)),
    Name1 = naming:str_name_to_corba(Name),
    case naming:register_below_nc(NS, Name1, Obj)
        of {error, Reason} ->
            ?LOG(rtl_error, "Failed to register data flow RTC ~s: ~p",
                [Name, Reason]),
            ok
         ; _ ->
            ok
    end,
    ?LOG(rtl_paranoid, "Name server contents after registration:"),
    ?LOG(rtl_paranoid, "~p", [orber_diagnostics:nameservice()]).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

