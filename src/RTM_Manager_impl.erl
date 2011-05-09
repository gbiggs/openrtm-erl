%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc CORBA wrapper around the manager introspection interface.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @source idl/Manager.idl
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
-module('RTM_Manager_impl').
-behaviour(gen_server).

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include("idl/RTM.hrl").
-include("idl/RTC.hrl").
-include("idl/SDOPackage.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include("type_specs.hrl").
-include("log.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(mgr_corba, {
        name :: [#'CosNaming_NameComponent'{}], % Registered name
        nc :: object_ref() % Naming context to which the manager is registered
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/0, stop/0]).
-export([load_module/4, unload_module/3, get_loadable_modules/2,
        get_loaded_modules/2, get_factory_profiles/2]).
-export([create_component/3, delete_component/3, get_components/2,
        get_component_profiles/2]).
-export([get_profile/2, get_configuration/2, set_configuration/4]).
-export([is_master/2, get_master_managers/2, add_master_manager/3,
        remove_master_manager/3, get_slave_managers/2, add_slave_manager/3,
        remove_slave_manager/3]).
-export([fork/2, shutdown/2, restart/2, get_service/3]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the CORBA manager introspection interface.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    Name = naming:format_name(config_svr:get_value(["manager",
                "naming_formats"])),
    {ok, Pid, Obj} = 'RTM_Manager':oe_create_link(Name,
        [{regname, {global, mgr_corba}}, {sup_child, true},
            {persistent, true}]),
    ok = register_on_ns(Name, Obj),
    {ok, Pid}.


%%-----------------------------------------------------------------------------
%% @doc Stop the CORBA manager introspection interface.
%% @spec stop() -> ok
%% @end
-spec(stop() -> ok).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:cast({global, mgr_corba}, stop).


%%-----------------------------------------------------------------------------
%% @doc Load and initialise a module ready for use.
%% @spec load_module(This, State, Path, InitFunc) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Path = string()
%%      InitFunc = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(load_module(This::object_ref(), State::#mgr_corba{},
        Path::string(), InitFunc::string()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
load_module(_This, State, Path, InitFunc) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Unload a previously-loaded module.
%% @spec unload_module(This, State, Path) -> {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Path = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(unload_module(This::object_ref(), State::#mgr_corba{}, Path::string()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
unload_module(_This, State, Pathname) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of the loadable modules.
%% @spec get_loadable_modules(This, State) -> {reply, Modules, NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Modules = [#'RTM_ModuleProfile'{Properties}]
%%      Properties = [#'SDOPackage_NameValue'{string(), any()}]
%%      NewState = #mgr_corba{}
%% @end
-spec(get_loadable_modules(This::object_ref(), State::#mgr_corba{}) ->
    {reply, [#'RTM_ModuleProfile'{}], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_loadable_modules(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of the loaded modules.
%% @spec get_loaded_modules(State) -> {rThis, eply, Modules, NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Modules = [#'RTM_ModuleProfile'{Properties}]
%%      Properties = [#'SDOPackage_NameValue'{string(), any()}]
%%      NewState = #mgr_corba{}
%% @end
-spec(get_loaded_modules(This::object_ref(), State::#mgr_corba{}) ->
    {reply, [#'RTM_ModuleProfile'{}], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_loaded_modules(_This, State) ->
    Mods = mod_svr:loaded_mods(),
    {reply, nvlist:from_list([{"file_path", M} || M <- Mods]), State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of the component profiles contained in loaded modules.
%% @spec get_factory_profiles(This, State) -> {reply, Modules, NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Modules = [#'RTM_ModuleProfile'{Properties}]
%%      Properties = [#'SDOPackage_NameValue'{string(), any()}]
%%      NewState = #mgr_corba{}
%% @end
-spec(get_factory_profiles(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, [#'RTM_ModuleProfile'{}], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_factory_profiles(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Create an RT Component instance from a loaded module.
%% @spec create_component(This, State, ModuleName) ->
%%      {reply, object_ref(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      ModuleName = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(create_component(This::object_ref(), State::#mgr_corba{},
        ModuleName::string()) -> 
    {reply, object_ref(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
create_component(This, State, Module_name) ->
    {reply, none, State}.


%%-----------------------------------------------------------------------------
%% @doc Delete an existing RT Component instance.
%% @spec delete_component(This, State, InstanceName) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      InstanceName = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(delete_component(This::object_ref(), State::#mgr_corba{},
        InstanceName::string()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
delete_component(_This, State, Instance_name) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Get the list of components running in this manager.
%% @spec get_components(This, State) -> {reply, [object_ref()], NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_components(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, object_ref(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_components(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Get a list of the profiles of components running in this manager.
%% @spec get_component_profiles(This, State) ->
%%      {reply, [#'RTC_ComponentProfile'{}], NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_component_profiles(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, [#'RTC_ComponentProfile'{}], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_component_profiles(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Get the manager's profile.
%% @spec get_profile(This, State) -> {reply, #'RTM_ManagerProfile'{}, NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_profile(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, #'RTM_ManagerProfile'{}, NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_profile(This, State) ->
    {reply, #'RTM_ManagerProfile'{}, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the manager's configuration.
%% @spec get_configuration(This, State) ->
%%      {reply, [#'SDOPackage_NameValue'{}], NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_configuration(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, [#'SDOPackage_NameValue'{}], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_configuration(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Set the manager's configuration.
%% @spec set_configuration(This, State, Name, Value) ->
%%      {reply, rtc_returncode(), NewState}
%%      This = object_ref()
%% where
%%      State = #mgr_corba{}
%%      Name = string()
%%      Value = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(set_configuration(This::object_ref(), State::#mgr_corba{},
        Name::string(), Value::string()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
set_configuration(_This, State, Name, Value) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Checks if this manager is a master or not.
%% @spec is_master(This, State) -> {reply, boolean(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(is_master(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, boolean(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
is_master(_This, State) ->
    {reply, true, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the list of this manager's masters, if it is a slave.
%% @spec get_master_managers(This, State) -> {reply, [object_ref()], NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_master_managers(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, [object_ref()], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_master_managers(_This, State) ->
    {reply, [], State}.

%%-----------------------------------------------------------------------------
%% @doc Add a new master manager to this manager.
%% @spec add_master_manager(This, State, Mgr) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Mgr = object_ref()
%%      NewState = #mgr_corba{}
%% @end
-spec(add_master_manager(This::object_ref(), State::#mgr_corba{},
        Mgr::object_ref()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
add_master_manager(_This, State, Mgr) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Remov a master manager from this manager.
%% @spec remove_master_manager(This, State, Mgr) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Mgr = object_ref()
%%      NewState = #mgr_corba{}
%% @end
-spec(remove_master_manager(This::object_ref(), State::#mgr_corba{},
        Mgr::object_ref()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
remove_master_manager(_This, State, Mgr) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Get a lest of the slave managers of this manager.
%% @spec get_slave_managers(This, State) -> {reply, [object_ref()], NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(get_slave_managers(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, [object_ref()], NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_slave_managers(_This, State) ->
    {reply, [], State}.


%%-----------------------------------------------------------------------------
%% @doc Add another manager as a slave of this manager.
%% @spec add_slave_manager(This, State, Mgr) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Mgr = object_ref()
%%      NewState = #mgr_corba{}
%% @end
-spec(add_slave_manager(This::object_ref(), State::#mgr_corba{},
        Mgr::object_ref()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
add_slave_manager(_This, State, Mgr) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Remove a slave from this manager.
%% @spec remove_slave_manager(This, State, Mgr) ->
%%      {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Mgr = object_ref()
%%      NewState = #mgr_corba{}
%% @end
-spec(remove_slave_manager(This::object_ref(), State::#mgr_corba{},
        Mgr::object_ref()) ->
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
remove_slave_manager(_This, State, Mgr) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc ???
%% @spec fork(This, State) -> {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(fork(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
fork(_This, State) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Shut down the manager.
%% @spec shutdown(This, State) -> {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(shutdown(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
shutdown(_This, State) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc Restart the manager.
%% @spec restart(This, State) -> {reply, rtc_returncode(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(restart(This::object_ref(), State::#mgr_corba{}) -> 
    {reply, rtc_returncode(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
restart(_This, State) ->
    {reply, 'UNSUPPORTED', State}.


%%-----------------------------------------------------------------------------
%% @doc ???
%% @spec get_service(This, State, Name) -> {reply, object_ref(), NewState}
%% where
%%      This = object_ref()
%%      State = #mgr_corba{}
%%      Name = string()
%%      NewState = #mgr_corba{}
%% @end
-spec(get_service(This::object_ref(), State::#mgr_corba{}, Name::string()) -> 
    {reply, object_ref(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
get_service(_This, State, Name) ->
    {reply, none, State}.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc gen_server initialisation callback function.
%% @spec init(Name) -> {ok, State} | {stop, Reason}
%% where
%%      Name = string()
%%          The name this interface is registered on the naming context with.
%%      State = #mgr_corba{}
%%      Reason = any()
%% @end
-spec(init(Name::string()) -> {ok, State::#mgr_corba{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(Name) ->
    ?LOG(rtl_info, "Initialising CORBA manager introspection interface."),
    process_flag(trap_exit, true),
    NC = corba:resolve_initial_references("NameService"),
    {ok, #mgr_corba{name=Name, nc=NC}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #mgr_corba{}
%%      Reply = any()
%%      NewState = #mgr_corba{}
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()},
        State::#mgr_corba{}) ->
    {reply, Reply::any(), NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, _From, State) ->
    {reply, not_supported, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(handle_cast(Request::any(), State::#mgr_corba{}) ->
    {stop, normal, NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = timeout | any()
%%      State = #mgr_corba{}
%%      NewState = #mgr_corba{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#mgr_corba{}) ->
    {ok, NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any} | any()
%%      State = #mgr_corba{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#mgr_corba{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, #mgr_corba{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shutting down normally."),
    naming:deregister_below_nc(NC, Name);
terminate(shutdown, #mgr_corba{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    naming:deregister_below_nc(NC, Name);
terminate({shutdown, Reason}, #mgr_corba{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    naming:deregister_below_nc(NC, Name);
terminate(Reason, #mgr_corba{name=Name, nc=NC}) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    naming:deregister_below_nc(NC, Name).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #mgr_corba{}
%%      Extra = any()
%%      NewState = #mgr_corba{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#mgr_corba{},
        Extra::any()) -> {ok, NewState::#mgr_corba{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Register a manager object on the name service.
%% @spec register_on_ns(Name, Obj) -> ok
%% where
%%      Name = string()
%%          The name to register under.
%%      Obj = object_ref()
%% @end
-spec(register_on_ns(Name::string(), Obj::object_ref()) -> ok).
%%-----------------------------------------------------------------------------
register_on_ns(Name, Obj) ->
    ?LOG(rtl_info, "Registering manager CORBA introspection interface as ~s",
        [Name]),
    NS = corba:resolve_initial_references("NameService"),
    Name1 = naming:str_name_to_corba(Name),
    case naming:register_below_nc(NS, Name1, Obj)
        of {error, Reason} ->
            {error, Reason}
         ; _ ->
            ok
    end.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

