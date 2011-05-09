%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc RTC:PortService implementation.
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
-module('RTC_PortService_impl').

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("orber/src/orber_iiop.hrl").
-include("idl/RTC.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("port.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(port_svc, {target=nil :: pid() | nil % Target process for the interface
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop_intf/1]).
% RTC:PortService interface
-export([get_port_profile/2, get_connector_profiles/2, get_connector_profile/3,
        connect/3, disconnect/3, disconnect_all/2, notify_connect/3,
        notify_disconnect/3]).


%%-----------------------------------------------------------------------------
%% Internal Exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_info/2, terminate/2, code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the PortService interface.
%% @spec start_link(none) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(none) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(none) ->
    'RTC_PortService':oe_create_link(none, [{sup_child, true}]).


%%-----------------------------------------------------------------------------
%% @doc Stop the PortService interface.
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
%% @doc Get the profile of the port.
%% @spec get_port_profile(This, State) ->
%%      {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Result = #'RTC_PortProfile'{}
%%      NewState = #port_svc{}
%% @end
-spec(get_port_profile(This::object_ref(), State::#port_svc{}) ->
    {reply, #'RTC_PortProfile'{}, NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
get_port_profile(_OE_This, #port_svc{target=T}=State) ->
    Result = corbafy_portprof(portsvc:get_port_profile(T)),
    ?LOG(rtl_debug, "Result of get_port_profile: ~p", [Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profiles of all connectors.
%% @spec get_connector_profiles(This, State) ->
%%      {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Result = [#'RTC_ConnectorProfile{}]
%%      NewState = #port_svc{}
%% @end
-spec(get_connector_profiles(This::object_ref(), State::#port_svc{}) ->
    {reply, [#'RTC_ConnectorProfile'{}], NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
get_connector_profiles(_OE_This, #port_svc{target=T}=State) ->
    Result = lists:map(fun corbafy_connprof/1,
        portsvc:get_connector_profiles(T)),
    ?LOG(rtl_debug, "Result of get_conn_profiles: ~p", [Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get a specific connector's profile.
%% @spec get_connector_profile(This, State, Connector_id) ->
%%      {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Connector_id = string()
%%      Result = #'RTC_ConnectorProfile'{}
%%      NewState = #port_svc{}
%% @end
-spec(get_connector_profile(This::object_ref(), State::#port_svc{},
        Connector_id::string()) ->
    {reply, #'RTC_ConnectorProfile'{}, NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
get_connector_profile(_OE_This, #port_svc{target=T}=State, Connector_id) ->
    Result = corbafy_connprof(portsvc:get_connector_profile(T, Connector_id)),
    ?LOG(rtl_debug, "Result of get_conn_profile(~p): ~p",
        [Connector_id, Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Connect this and other ports.
%% @spec connect(This, State, ConnectorProfile) ->
%%      {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      ConnectorProfile = #'RTC_ConnectorProfile'{}
%%      Result = {rtc_returncode(), #'RTC_ConnectorProfile'{}}
%%      NewState = #port_svc{}
%% @end
-spec(connect(This::object_ref(), State::#port_svc{},
        Connector_profile::#'RTC_ConnectorProfile'{}) ->
    {reply, {rtc_returncode(), #'RTC_ConnectorProfile'{}},
        NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
connect(_OE_This, #port_svc{target=T}=State, Connector_profile) ->
    {Result, NewProf} = portsvc:connect(T,
        decorbafy_connprof(Connector_profile)),
    Result1 = utils:to_rtc_rc(Result),
    NewProf1 = corbafy_connprof(NewProf),
    ?LOG(rtl_debug, "Result of connect: ~p~n~p~n(from profile ~p)",
        [Result1, NewProf1, Connector_profile]),
    {reply, {Result1, NewProf1}, State}.


%%-----------------------------------------------------------------------------
%% @doc Disconnect a single connection.
%% @spec disconnect(This, State, Connector_id) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Connector_id = string()
%%      NewState = #port_svc{}
%% @end
-spec(disconnect(This::object_ref(), State::#port_svc{},
        Connector_id::string()) ->
    {reply, Result::rtc_returncode(), NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
disconnect(_OE_This, #port_svc{target=T}=State, Connector_id) ->
    Result = utils:to_rtc_rc(portsvc:disconnect(T, Connector_id)),
    ?LOG(rtl_debug, "Result of disconnect(~p): ~p", [Connector_id, Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Disconnect all connections on this port.
%% @spec disconnect_all(This, State) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      NewState = #port_svc{}
%% @end
-spec(disconnect_all(This::object_ref(), State::#port_svc{}) ->
    {reply, rtc_returncode(), NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
disconnect_all(_OE_This, #port_svc{target=T}=State) ->
    Result = utils:to_rtc_rc(portsvc:disconnect_all(T)),
    ?LOG(rtl_debug, "Result of disconnect_all: ~p", [Result]),
    {reply, Result, State}.


%%=============================================================================
%% Internal Functions
%%=============================================================================


%%-----------------------------------------------------------------------------
%% @doc Notify the PortService that it is being connected.
%% @spec notify_connect(This, State, Connector_profile) ->
%%      {reply, Result, NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Connector_profile = #'RTC_ConnectorProfile'{}
%%      Result = {rtc_returncode(), #'RTC_ConnectorProfile'{}}
%%      NewState = #port_svc{}
%% @end
-spec(notify_connect(This::object_ref(), State::#port_svc{},
        Connector_profile::#'RTC_ConnectorProfile'{}) ->
    {reply, {rtc_returncode(), #'RTC_ConnectorProfile'{}},
        NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
notify_connect(_OE_This, #port_svc{target=T}=State, Connector_profile) ->
    {Result, NewProf} = portsvc:notify_connect(T,
        decorbafy_connprof(Connector_profile)),
    Result1 = utils:to_rtc_rc(Result),
    NewProf1 = corbafy_connprof(NewProf),
    ?LOG(rtl_debug, "Result of notify_connect: ~p~n~p~n(from profile ~p)",
        [Result1, NewProf1, Connector_profile]),
    {reply, {Result1, NewProf1}, State}.


%%-----------------------------------------------------------------------------
%% @doc Notify the port service that it is being disconnected.
%% @spec notify_disconnect(This, State, Connector_id) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #port_svc{}
%%      Connector_id = string()
%%      NewState = #port_svc{}
%% @end
-spec(notify_disconnect(This::object_ref(), State::#port_svc{},
        Connector_id::string()) ->
    {reply, rtc_returncode(), NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
notify_disconnect(_OE_This, #port_svc{target=T}=State, Connector_id) ->
    Result = utils:to_rtc_rc(portsvc:notify_disconnect(T, Connector_id)),
    ?LOG(rtl_debug, "Result of notify_disconnect(~p): ~p",
        [Connector_id, Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init(none) -> {ok, State} | {stop, Reason}
%% where
%%      State = #port_svc{}
%%      Reason = any()
%% @end
-spec(init(none) -> {ok, State::#port_svc{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(none) ->
    ?LOG(rtl_info, "Initialising CORBA RTC:PortService interface."),
    {ok, #port_svc{}}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = {set_target, Target}
%%      Target = pid()
%%      State = #port_svc{}
%%      NewState = #port_svc{}
%% @end
-spec(handle_info(Info::{set_target, Target::pid()}, State::#port_svc{}) ->
    {noreply, NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
handle_info({set_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received portsvc target: ~p", [Target]),
    {noreply, State#port_svc{target=Target}}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #port_svc{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#port_svc{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "Shutting down normally.");
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "Shut down by supervisor.");
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #port_svc{}
%%      Extra = any()
%%      NewState = #port_svc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#port_svc{},
        Extra::any()) -> {ok, NewState::#port_svc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Change a port profile from native to CORBA format.
%% @spec corbafy_portprof(#port_prof{}) -> #'RTC_PortProfile'{}
%% @end
-spec(corbafy_portprof(#port_prof{}) -> #'RTC_PortProfile'{}).
%%-----------------------------------------------------------------------------
corbafy_portprof(Prof) ->
    Intfs = lists:map(fun corbafy_intfprof/1, Prof#port_prof.interfaces),
    ConnProfs = lists:map(fun corbafy_connprof/1, Prof#port_prof.conn_profs),
    #'RTC_PortProfile'{name=Prof#port_prof.name,
        interfaces=Intfs,
        port_ref=corbafy_port_ref(Prof#port_prof.port_ref),
        connector_profiles=ConnProfs,
        owner=corbafy_rtc_ref(Prof#port_prof.owner),
        properties=nvlist:from_list(config:flatten(Prof#port_prof.props))}.


%%-----------------------------------------------------------------------------
%% @doc Change an interface profile from native to CORBA format.
%% @spec corbafy_intfprof(#port_intf_prof{}) -> #'RTC_PortInterfaceProfile'{}
%% @end
-spec(corbafy_intfprof(#port_intf_prof{}) -> #'RTC_PortInterfaceProfile'{}).
%%-----------------------------------------------------------------------------
corbafy_intfprof(Prof) ->
    #'RTC_PortInterfaceProfile'{instance_name=Prof#port_intf_prof.inst_name,
        type_name=Prof#port_intf_prof.type_name,
        polarity=corbafy_polarity(Prof#port_intf_prof.polarity)}.


%%-----------------------------------------------------------------------------
%% @doc Change a connector profile from native to CORBA format.
%% @spec corbafy_connprof(#conn_prof{}) -> #'RTC_ConnectorProfile'{}
%% @end
-spec(corbafy_connprof(#conn_prof{}) -> #'RTC_ConnectorProfile'{}).
%%-----------------------------------------------------------------------------
corbafy_connprof(Prof) ->
    ?LOG(rtl_debug, "CORBAfying conn_prof ~p", [Prof]),
    Ports = lists:map(fun corbafy_port_ref/1, Prof#conn_prof.ports),
    #'RTC_ConnectorProfile'{name=Prof#conn_prof.name,
        connector_id=Prof#conn_prof.id,
        ports=Ports,
        properties=nvlist:from_list(config:flatten(Prof#conn_prof.props))}.


%%-----------------------------------------------------------------------------
%% @doc Ensure a port reference is the object reference version.
%% @spec corbafy_port_ref(pid() | object_ref()) -> object_ref()
%% @end
-spec(corbafy_port_ref(Port::pid() | object_ref()) -> object_ref()).
%%-----------------------------------------------------------------------------
corbafy_port_ref(nil) ->
    ?ORBER_NIL_OBJREF;
corbafy_port_ref(?ORBER_NIL_OBJREF) ->
    ?ORBER_NIL_OBJREF;
corbafy_port_ref(Port) when is_pid(Port) ->
    portsvc:get_corba_obj(Port);
corbafy_port_ref(Port) ->
    Port.


%%-----------------------------------------------------------------------------
%% @doc Ensure an RTC reference is the object reference version.
%% @spec corbafy_rtc_ref(pid() | object_ref()) -> object_ref()
%% @end
-spec(corbafy_rtc_ref(RTC::pid() | object_ref()) -> object_ref()).
%%-----------------------------------------------------------------------------
corbafy_rtc_ref(nil) ->
    ?ORBER_NIL_OBJREF;
corbafy_rtc_ref(?ORBER_NIL_OBJREF) ->
    ?ORBER_NIL_OBJREF;
corbafy_rtc_ref(RTC) when is_pid(RTC) ->
    rtc:get_corba_obj(RTC);
corbafy_rtc_ref(RTC) ->
    RTC.


%%-----------------------------------------------------------------------------
%% @doc Convert a port polarity from native to CORBA format.
%% @spec corbafy_polarity(port_intf_polarity()) -> 'PROVIDED' | 'REQUIRED'
%% @end
-spec(corbafy_polarity(port_intf_polarity()) -> 'PROVIDED' | 'REQUIRED').
%%-----------------------------------------------------------------------------
corbafy_polarity(provided) ->
    'PROVIDED';
corbafy_polarity(required) ->
    'REQUIRED'.


%%-----------------------------------------------------------------------------
%% @doc Change a connector profile from CORBA to native format.
%% @spec decorbafy_connprof(#'RTC_ConnectorProfile'{}) -> #conn_prof{}
%% @end
-spec(decorbafy_connprof(#'RTC_ConnectorProfile'{}) -> #conn_prof{}).
%%-----------------------------------------------------------------------------
decorbafy_connprof(Prof) ->
    #conn_prof{name=Prof#'RTC_ConnectorProfile'.name,
        id=Prof#'RTC_ConnectorProfile'.connector_id,
        ports=Prof#'RTC_ConnectorProfile'.ports,
        props=config:unflatten(nvlist:to_list(
                Prof#'RTC_ConnectorProfile'.properties))}.

