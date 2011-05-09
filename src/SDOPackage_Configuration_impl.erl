%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc CORBA wrapper around the configuration interfaces.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @source idl/SDOPackage.idl
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
-module('SDOPackage_Configuration_impl').

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


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(sdoc_svc, {target=nil :: pid() | nil % Target process for the interface
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/1, stop_intf/1]).
% SDOPackage::Configuration interface
-export([set_device_profile/3, add_service_profile/3, add_organization/3,
        remove_service_profile/3, remove_organization/3,
        get_configuration_parameters/2, get_configuration_parameter_values/2,
        get_configuration_parameter_value/3, set_configuration_parameter/4,
        get_configuration_sets/2, get_configuration_set/3,
        set_configuration_set_values/3, get_active_configuration_set/2,
        add_configuration_set/3, remove_configuration_set/3,
        activate_configuration_set/3]).

%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_info/2, terminate/2, code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the SDOPackage Configuration interface.
%% @spec start_link(none) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(none) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(none) ->
    'SDOPackage_Configuration':oe_create_link(none, [{sup_child, true}]).


%%-----------------------------------------------------------------------------
%% @doc Stop the SDOPackage Configuration interface.
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
%% @doc Set the SDO's device profile, replacing any existing one.
%% @spec set_device_profile(This, State, DProfile) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      DProfile = #'SDOPackage_DeviceProfile'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(set_device_profile(This::object_ref(), State::#sdoc_svc{},
        DProfile::#'SDOPackage_DeviceProfile'{}) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
set_device_profile(_OE_This, _State, _DProfile) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Add a service profile, replacing any existing one with the same ID.
%% @spec add_service_profile(This, State, SProfile) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      SProfile = #'SDOPackage_ServiceProfile'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(add_service_profile(This::object_ref(), State::#sdoc_svc{},
        SProfile::#'SDOPackage_ServiceProfile'{}) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
add_service_profile(_OE_This, _State, _SProfile) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Add an organisation.
%% @spec add_organization(This, State, OrgObj) -> {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      OrgObj = object_ref()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(add_organization(This::object_ref(), State::#sdoc_svc{},
        OrgObj::object_ref()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
add_organization(_OE_This, _State, _Organization_object) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Remove a service profile from the interface.
%% @spec remove_service_profile(This, State, Id) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Id = string()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(remove_service_profile(This::object_ref(), State::#sdoc_svc{},
        Id::string()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
remove_service_profile(_OE_This, _State, _Id) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Remove an organisation.
%% @spec remove_organization(This, State, Id) -> {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Id = string()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(remove_organization(This::object_ref(), State::#sdoc_svc{},
        Id::string()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
remove_organization(_OE_This, _State, _Organization_id) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of all configuration parameters.
%% @spec get_configuration_parameters(This, State) -> {reply, Params, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Params = [#'SDOPackage_Parameter'{}]
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_configuration_parameters(This::object_ref(), State::#sdoc_svc{}) ->
    {reply, Params::[#'SDOPackage_Parameter'{}], NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration_parameters(_OE_This, _State) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of all configuration parameters and their values.
%% @spec get_configuration_parameter_values(This, State) ->
%%      {reply, Params, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Params = [#'SDOPackage_NameValue'{}]
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_configuration_parameter_values(This::object_ref(),
        State::#sdoc_svc{}) ->
    {reply, Params::[#'SDOPackage_NameValue'{}], NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration_parameter_values(_OE_This, _State) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get the value of a configuration parameter.
%% @spec get_configuration_parameter_value(This, State, Name) ->
%%      {reply, Value, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Name = string()
%%      Value = any()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(get_configuration_parameter_value(This::object_ref(), State::#sdoc_svc{},
        Name::string()) ->
    {reply, Value::any(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration_parameter_value(_OE_This, _State, _Name) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Set a configuration parameter value.
%% @spec set_configuration_parameter(This, State, Name, Value) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Name = string()
%%      Value = any()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(set_configuration_parameter(This::object_ref(), State::#sdoc_svc{},
        Name::string(), Value::any()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
set_configuration_parameter(_OE_This, _State, _Name, _Value) ->
    corba:raise(#'SDOPackage_NotAvailable'{description="Not supported"}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of the configuration sets.
%% @spec get_configuration_sets(This, State) -> {reply, Sets, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Sets = [#'SDOPackage_ConfigurationSet'{}]
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_configuration_sets(This::object_ref(), State::#sdoc_svc{}) ->
    {reply, Sets::[#'SDOPackage_ConfigurationSet'{}], NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration_sets(_OE_This, #sdoc_svc{target=T}=State) ->
    SetNames = rtc_cfg_svr:get_sets(T),
    Sets = lists:map(fun(N) ->
                corbafy_set(N, rtc_cfg_svr:get_values(T, N)) end, SetNames),
    {reply, Sets, State}.


%%-----------------------------------------------------------------------------
%% @doc Get a specific configuration set.
%% @spec get_configuration_set(This, State, Id) -> {reply, Set, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Id = string()
%%      Set = #'SDOPackage_ConfigurationSet'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_configuration_set(This::object_ref(), State::#sdoc_svc{},
        Id::string()) ->
    {reply, Set::#'SDOPackage_ConfigurationSet'{}, NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_configuration_set(_OE_This, #sdoc_svc{target=T}=State, Config_id) ->
    case rtc_cfg_svr:has_set(T, Config_id)
        of true ->
            {reply,
                corbafy_set(Config_id, rtc_cfg_svr:get_values(T, Config_id)),
                State}
         ; false ->
            corba:raise(#'SDOPackage_InvalidParameter'{description="No such set"})
    end.


%%-----------------------------------------------------------------------------
%% @doc Set all values in a configuration set.
%% @spec set_configuration_set_values(This, State, Set) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Set = #'SDOPackage_ConfigurationSet'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(set_configuration_set_values(This::object_ref(), State::#sdoc_svc{},
        Set::#'SDOPackage_ConfigurationSet'{}) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
set_configuration_set_values(_OE_This, #sdoc_svc{target=T}=State,
        Configuration_set) ->
    {SetName, Values} = decorbafy_set(Configuration_set),
    Result = case rtc_cfg_svr:has_set(T, SetName)
        of true ->
            case lists:foldl(fun({P, _}, Acc) ->
                        not rtc_cfg_svr:has_key(T, SetName, P) or Acc end,
                    false, Values)
                of true ->
                    false
                 ; false ->
                    rtc_cfg_svr:set_values(T, SetName, Values),
                    true
            end
         ; false ->
            false
    end,
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the active configuration set.
%% @spec get_active_configuration_set(This, State) -> {reply, Set, NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Set = #'SDOPackage_ConfigurationSet'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::NotAvailable, SDOPackage::InternalError
%% @end
-spec(get_active_configuration_set(This::object_ref(), State::#sdoc_svc{}) ->
    {reply, Set::#'SDOPackage_ConfigurationSet'{}, NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
get_active_configuration_set(_OE_This, #sdoc_svc{target=T}=State) ->
    SetName = rtc_cfg_svr:get_active_set(T),
    Set = corbafy_set(SetName, rtc_cfg_svr:get_values(T, SetName)),
    {reply, Set, State}.


%%-----------------------------------------------------------------------------
%% @doc Add a new configuration set.
%% @spec add_configuration_set(This, State, Set) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Set = #'SDOPackage_ConfigurationSet'{}
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(add_configuration_set(This::object_ref(), State::#sdoc_svc{},
        Set::#'SDOPackage_ConfigurationSet'{}) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
add_configuration_set(_OE_This, #sdoc_svc{target=T}=State, Configuration_set) ->
    {SetName, Values} = decorbafy_set(Configuration_set),
    case SetName
        of "" ->
            corba:raise(#'SDOPackage_InvalidParameter'{description="Empty ID"})
         ; _ ->
            ok
    end,
    Result = case rtc_cfg_svr:has_set(T, SetName)
        of true ->
            false
         ; false ->
            rtc_cfg_svr:set_values(T, SetName, Values),
            true
    end,
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Remove a configuration set.
%% @spec remove_configuration_set(This, State, Id) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Id = string()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(remove_configuration_set(This::object_ref(), State::#sdoc_svc{},
        Id::string()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
remove_configuration_set(_OE_This, _State, "") ->
    corba:raise(#'SDOPackage_InvalidParameter'{description="Empty ID"});
remove_configuration_set(_OE_This, #sdoc_svc{target=T}=State, Config_id) ->
    case rtc_cfg_svr:rem_set(T, Config_id)
        of {error, active_set} ->
            {reply, false, State}
         ; {error, no_set} ->
            {reply, false, State}
         ; ok ->
            {reply, true, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Make the specified configuration set the active one.
%% @spec activate_configuration_set(This, State, Id) ->
%%      {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #sdoc_svc{}
%%      Id = string()
%%      NewState = #sdoc_svc{}
%% @throws SDOPackage::InvalidParameter, SDOPackage::NotAvailable
%%      SDOPackage::InternalError
%% @end
-spec(activate_configuration_set(This::object_ref(), State::#sdoc_svc{},
        Id::string()) ->
    {reply, boolean(), NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
activate_configuration_set(_OE_This, State, [$_|_]) ->
    % Set names beginning with an underscore are hidden and cannot be activated
    {reply, false, State};
activate_configuration_set(_OE_This, #sdoc_svc{target=T}=State, Config_id) ->
    case rtc_cfg_svr:set_active_set(T, Config_id)
        of {error, no_set} ->
            {reply, false, State}
         ; ok ->
            {reply, true, State}
    end.


%%=============================================================================
%% Internal Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init(none) -> {ok, State} | {stop, Reason}
%% where
%%      State = #sdoc_svc{}
%%      Reason = any()
%% @end
-spec(init(none) -> {ok, State::#sdoc_svc{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(none) ->
    ?LOG(rtl_info, "Initialising CORBA SDOPackage::Configuration interface."),
    {ok, #sdoc_svc{}}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = {set_target, Target}
%%      Target = pid()
%%      State = #sdoc_svc{}
%%      NewState = #sdoc_svc{}
%% @end
-spec(handle_info(Info::{set_target, Target::pid()}, State::#sdoc_svc{}) ->
    {noreply, NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
handle_info({set_target, Target}, State) ->
    ?LOG(rtl_paranoid, "Received rtc_cfg_svr target: ~p", [Target]),
    {noreply, State#sdoc_svc{target=Target}}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #sdoc_svc{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#sdoc_svc{}) -> ok).
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
%%      State = #sdoc_svc{}
%%      Extra = any()
%%      NewState = #sdoc_svc{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#sdoc_svc{},
        Extra::any()) -> {ok, NewState::#sdoc_svc{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc CORBAfy a configuration set.
%% @spec corbafy_set(Name, Set) -> CorbafiedSet
%% where
%%      Name = string()
%%      Set = proplist()
%%      CorbafiedSet = #'SDOPackage_ConfigurationSet'{}
%% @end
-spec(corbafy_set(Name::string(), Set::proplist()) ->
    CorbafiedSet::#'SDOPackage_ConfigurationSet'{}).
%%-----------------------------------------------------------------------------
corbafy_set(Name, Set) ->
    #'SDOPackage_ConfigurationSet'{id=Name, description="",
        configuration_data=nvlist:from_list(Set)}.


%%-----------------------------------------------------------------------------
%% @doc De-CORBAfy a configuration set.
%% @spec decorbafy_set(CorbafiedSet) -> {Name, Set}
%% where
%%      CorbafiedSet = #'SDOPackage_ConfigurationSet'{}
%%      Name = string()
%%      Set = proplist()
%% @end
-spec(decorbafy_set(CorbafiedSet::#'SDOPackage_ConfigurationSet'{}) ->
    {Name::string(), Set::proplist()}).
%%-----------------------------------------------------------------------------
decorbafy_set(CS) ->
    Name = CS#'SDOPackage_ConfigurationSet'.id,
    Set = nvlist:to_list(CS#'SDOPackage_ConfigurationSet'.configuration_data),
    {Name, Set}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

