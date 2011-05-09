%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Manager server for the ports of an RTC.
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
-module(port_mgr).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("port.hrl").
-include("log.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------
-type(port_entry() :: {Name::string(), Type::port_type(), Sup::pid(),
        Worker::pid()}).
-record(pm_state, {sup=nil :: pid() | nil, % PID of the supervisor to register ports with
        ports=[] :: [port_entry()] % List of managed ports
    }).


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/0, stop_server/1]).
% Port management
-export([add_port/6, rem_port/2, get_port_names/1, get_port/2, get_ports/1,
        port_profs/1]).
% Setup
-export([set_ports_sup/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the server.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link(port_mgr, [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server(Svr) -> ok
%% where
%%      Svr = pid()
%% @end
-spec(stop_server(Svr::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_server(Svr) ->
    gen_server:cast(Svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Add a new port.
%% @spec add_port(Svr, Type, Name, DataType, Owner, Config) ->
%%      ok | {error, bad_param}
%% where
%%      Svr = pid()
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Owner = pid()
%%      Config = config()
%% @end
-spec(add_port(Svr::pid(), Type::port_type(), Name::string(),
        DataType::string(), Owner::pid(), Config::config()) ->
    ok | {error, bad_param}).
%%-----------------------------------------------------------------------------
add_port(Svr, Type, Name, DataType, Owner, Config) ->
    gen_server:call(Svr, {add_port, Type, Name, DataType, Owner, Config}).


%%-----------------------------------------------------------------------------
%% @doc Remove a port.
%% @spec rem_port(Svr, Name) -> ok | {error, bad_param}
%% where
%%      Svr = pid()
%%      Name = string()
%% @end
-spec(rem_port(Svr::pid(), Name::string()) -> ok | {error, bad_param}).
%%-----------------------------------------------------------------------------
rem_port(Svr, Name) ->
    gen_server:call(Svr, {rem_port, Name}).


%%-----------------------------------------------------------------------------
%% @doc Get a list of the names of managed ports.
%% @spec get_port_names(Svr) -> PortNames
%% where
%%      Svr = pid()
%%      PortNames = [string()]
%% @end
-spec(get_port_names(Svr::pid()) -> PortNames::[string()]).
%%-----------------------------------------------------------------------------
get_port_names(Svr) ->
    gen_server:call(Svr, get_port_names).


%%-----------------------------------------------------------------------------
%% @doc Get a list of the port PIDs.
%% @spec get_ports(Svr) -> Ports
%% where
%%      Svr = pid()
%%      Ports = [pid()]
%% @end
-spec(get_ports(Svr::pid()) -> Ports::[pid()]).
%%-----------------------------------------------------------------------------
get_ports(Svr) ->
    gen_server:call(Svr, get_ports).


%%-----------------------------------------------------------------------------
%% @doc Get the PID of a port's worker process.
%% @spec get_port(Svr, Name) -> Port | {error, bad_param}
%% where
%%      Svr = pid()
%%      Name = string()
%%      Port = pid()
%% @end
-spec(get_port(Svr::pid(), Name::string()) ->
    Port::pid() | {error, bad_param}).
%%-----------------------------------------------------------------------------
get_port(Svr, Name) ->
    gen_server:call(Svr, {get_port, Name}).


%%-----------------------------------------------------------------------------
%% @doc Get the profiles of the ports.
%% @spec port_profs(Svr) -> Profiles
%% where
%%      Svr = pid()
%%      Profiles = [#port_prof{}]
%% @end
-spec(port_profs(Svr::pid()) -> Profiles::[#port_prof{}]).
%%-----------------------------------------------------------------------------
port_profs(Svr) ->
    gen_server:call(Svr, get_port_profs).


%%-----------------------------------------------------------------------------
%% @doc Set the PID of the supervisor used to supervise ports.
%% @spec set_ports_sup(Svr, Sup) -> ok
%% where
%%      Svr = pid()
%%      Sup = pid()
%% @end
-spec(set_ports_sup(Svr::pid(), Sup::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_ports_sup(Svr, Sup) ->
    gen_server:call(Svr, {set_ports_sup, Sup}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init([]) -> {ok, State} | {stop, Reason}
%% where
%%      State = #pm_state{}
%%      Reason = any()
%% @end
-spec(init([]) -> {ok, State::#pm_state{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #pm_state{}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = {set_ports_sup, Sup} |
%%          {add_port, Type, Name, DataType, Owner, Config} | {rem_port, Name} |
%%          get_port_names | get_ports | {get_port, Name}
%%      Sup = pid()
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Config = config()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #pm_state{}
%%      Reply = ok | {error, bad_param} | PortNames | Ports | Port
%%      NewState = #pm_state{}
%%      PortNames = [string()]
%%      Port = pid()
%%      Ports = [Port]
%% @end
-spec(handle_call(Request::{set_ports_sup, Sup::pid()} |
        {add_port, Type::port_type(), Name::string(),
            DataType::string(), Owner::pid(), Config::config()} |
        {rem_port, Name::string()}, From::{pid(), Tag::any()} |
        get_port_names | get_ports | {get_port, Name::string()},
        State::#pm_state{}) ->
    {reply, Reply::ok | {error, bad_param} | [string()] | [pid()] | pid(),
        NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
handle_call({set_ports_sup, Sup}, _From, State) ->
    {reply, ok, State#pm_state{sup=Sup}};
handle_call({add_port, Type, Name, DataType, Owner, Config}, _From, State) ->
    {Result, NewState} = add_port_req(Type, Name, DataType, Owner, Config,
        State),
    ?LOG(rtl_debug, "add_port reply: ~p", [Result]),
    ?LOG(rtl_debug, "New state: ~p", [NewState]),
    {reply, Result, NewState};
handle_call({rem_port, Name}, _From, State) ->
    {Result, NewState} = rem_port_req(Name, State),
    ?LOG(rtl_debug, "rem_port reply: ~p", [Result]),
    ?LOG(rtl_debug, "New state: ~p", [NewState]),
    {reply, Result, NewState};
handle_call(get_port_names, _From, State) ->
    R = get_port_names_req(State),
    ?LOG(rtl_debug, "get_port_names reply: ~p", [R]),
    {reply, R, State};
handle_call(get_ports, _From, State) ->
    R = get_ports_req(State),
    ?LOG(rtl_debug, "get_ports reply: ~p", [R]),
    {reply, R, State};
handle_call({get_port, Name}, _From, State) ->
    R = get_port_req(Name, State),
    ?LOG(rtl_debug, "get_port(~p) reply: ~p", [Name, R]),
    {reply, R, State};
handle_call(get_port_profs, _From, State) ->
    R = get_port_profs(State),
    ?LOG(rtl_debug, "get_port_profs reply: ~p", [R]),
    {reply, R, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, State}
%% where
%%      Request = stop
%%      State = #pm_state{}
%% @end
-spec(handle_cast(Request::stop, State::#pm_state{}) ->
    {stop, normal, State::#pm_state{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = not_supported
%%      State = #pm_state{}
%% @end
-spec(handle_info(Info::not_supported, State::#pm_state{}) ->
    {noreply, State::#pm_state{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = any()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::any()) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, State) ->
    ?LOG(rtl_info, "Shutting down normally."),
    rem_all_ports(State);
terminate(shutdown, State) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    rem_all_ports(State);
terminate({shutdown, Reason}, State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    rem_all_ports(State);
terminate(Reason, State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    rem_all_ports(State).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #pm_state{}
%%      Extra = any()
%%      NewState = #pm_state{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#pm_state{},
        Extra::any()) -> {ok, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Add a new port.
%% @spec add_port_req(Type, Name, DataType, Owner, Config, State) ->
%%      {ok | {error, bad_param}, NewState}
%% where
%%      Type = port_type()
%%      Name = string()
%%      DataType = string()
%%      Owner = pid()
%%      Config = config()
%%      State = #pm_state{}
%%      NewState = #pm_state{}
%% @end
-spec(add_port_req(Type::port_type(), Name::string(), DataType::string(),
        Owner::pid(), Config::config(), State::#pm_state{}) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
add_port_req(datain, Name, DataType, Owner, Config, State) ->
    add_datain_port(Name, DataType, Owner, Config, State);
add_port_req(dataout, Name, DataType, Owner, Config, State) ->
    add_dataout_port(Name, DataType, Owner, Config, State);
add_port_req(svc, Name, _DataType, Owner, Config, State) ->
    add_svc_port(Name, Owner, Config, State).


%%-----------------------------------------------------------------------------
%% @doc Add a new datain port.
%% @spec add_datain_port(Name, DataType, Owner, Config, State) ->
%%      {ok | {error, bad_param}, NewState}
%% where
%%      Name = string()
%%      DataType = string()
%%      Owner = pid()
%%      Config = config()
%%      State = #pm_state{}
%%      NewState = #pm_state{}
%% @end
-spec(add_datain_port(Name::string(), DataType::string(), Owner::pid(),
        Config::config(), State::#pm_state{}) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
add_datain_port(Name, DataType, Owner, Config, #pm_state{sup=S}=State) ->
    ?LOG(rtl_debug,
        "Adding new DataIn port ~p, datatype ~p, owned by ~p, config ~p",
        [Name, DataType, Owner, Config]),
    case supervisor:start_child(S, {Name, {portsvc, create_datain,
                    [Name, DataType, Owner, Config]}, temporary, 30000,
                supervisor, [portsvc]})
        of {ok, Sup, Worker} ->
            ?LOG(rtl_debug, "Created new port ~p (~p:~p)", [Name, Sup,
                    Worker]),
            {ok, NewState} = reg_port(State, Name, datain, Sup, Worker),
            {ok, NewState}
         ; {error, Reason} ->
            ?LOG(rtl_error, "Failed to create new port ~p: ~p", [Name,
                    Reason]),
            {{error, bad_param}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Add a new dataout port.
%% @spec add_dataout_port(Name, DataType, Owner, Config, State) ->
%%      {ok | {error, bad_param}, NewState}
%% where
%%      Name = string()
%%      DataType = string()
%%      Owner = pid()
%%      Config = config()
%%      State = #pm_state{}
%%      NewState = #pm_state{}
%% @end
-spec(add_dataout_port(Name::string(), DataType::string(), Owner::pid(),
        Config::config(), State::#pm_state{}) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
add_dataout_port(Name, DataType, Owner, Config, #pm_state{sup=S}=State) ->
    ?LOG(rtl_debug,
        "Adding new DataOut port ~p, datatype ~p, owned by ~p, config ~p",
        [Name, DataType, Owner, Config]),
    case supervisor:start_child(S, {Name, {portsvc, create_dataout,
                    [Name, DataType, Owner, Config]}, temporary, 30000,
                supervisor, [portsvc]})
        of {ok, Sup, Worker} ->
            ?LOG(rtl_debug, "Created new port ~p (~p:~p)", [Name, Sup,
                    Worker]),
            {ok, NewState} = reg_port(State, Name, dataout, Sup, Worker),
            {ok, NewState}
         ; {error, Reason} ->
            ?LOG(rtl_error, "Failed to create new port ~p: ~p", [Name,
                    Reason]),
            {{error, bad_param}, State}
    end.



%%-----------------------------------------------------------------------------
%% @doc Add a new service port.
%% @spec add_svc_port(Name, Owner, Config, State) ->
%%      {ok | {error, bad_param}, NewState}
%% where
%%      Config = config()
%%      State = #pm_state{}
%%      NewState = #pm_state{}
%% @end
-spec(add_svc_port(Name::string(), Owner::pid(), Config::config(),
        State::#pm_state{}) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
add_svc_port(Name, _Owner, Config, State) ->
    ?LOG(rtl_debug, "Adding new Service port ~p with config ~p",
        [Name, Config]),
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Remove a port.
%% @spec rem_port_req(Name, State) -> {ok | {error, bad_param}, NewState}
%% where
%%      Name = string()
%%      State = #pm_state{}
%%      NewState = #pm_state{}
%% @end
-spec(rem_port_req(Name::string(), State::#pm_state{}) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
rem_port_req(Name, #pm_state{sup=S}=State) ->
    ?LOG(rtl_debug, "Removing port ~p", [Name]),
    case supervisor:terminate_child(S, Name)
        of ok ->
            dereg_port(State, Name)
         ; _ ->
            {{error, bad_param}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the names of managed ports.
%% @spec get_port_names_req(State) -> PortNames
%% where
%%      State = #pm_state{}
%%      PortNames = [string()]
%% @end
-spec(get_port_names_req(State::#pm_state{}) -> PortNames::[string()]).
%%-----------------------------------------------------------------------------
get_port_names_req(#pm_state{ports=Ports}) ->
    [N || {N, _, _, _} <- Ports].


%%-----------------------------------------------------------------------------
%% @doc Get the PIDs of managed ports.
%% @spec get_ports_req(State) -> PortNames
%% where
%%      State = #pm_state{}
%%      Ports = [pid()]
%% @end
-spec(get_ports_req(State::#pm_state{}) -> Ports::[pid()]).
%%-----------------------------------------------------------------------------
get_ports_req(#pm_state{ports=Ports}) ->
    [P || {_, _, _, P} <- Ports].


%%-----------------------------------------------------------------------------
%% @doc Get a port's worker process.
%% @spec get_port_req(Name, State) -> Port | {error, bad_param}
%% where
%%      Name = string()
%%      State = #pm_state{}
%% @end
-spec(get_port_req(Name::string(), State::#pm_state{}) ->
    string() | {error, bad_param}).
%%-----------------------------------------------------------------------------
get_port_req(Name, #pm_state{ports=Ports}) ->
    case lists:keyfind(Name, 1, Ports)
        of {Name, _, _, Worker} ->
            Worker
         ; false ->
            {error, bad_param}
    end.


%%-----------------------------------------------------------------------------
%% @doc Make port profiles.
%% @spec get_port_profs(State) -> Profiles
%% where
%%      State = #pm_state{}
%%      Profiles = [#port_prof{}]
%% @end
-spec(get_port_profs(State::#pm_state{}) -> Profiles::[#port_prof{}]).
%%-----------------------------------------------------------------------------
get_port_profs(#pm_state{ports=Ports}) ->
    lists:map(fun portsvc:get_port_profile/1, [P || {_, _, _, P} <- Ports]).


%%-----------------------------------------------------------------------------
%% @doc Shutdown and remove all ports.
%% @spec rem_all_ports(State) -> ok
%% where
%%      State = #pm_state{}
%% @end
-spec(rem_all_ports(State::#pm_state{}) -> ok).
%%-----------------------------------------------------------------------------
rem_all_ports(#pm_state{ports=Ports}=State) ->
    lists:foreach(fun({N, _, _, _}) -> {ok, _} = rem_port_req(N, State) end,
        Ports),
    State#pm_state{ports=[]}.


%%-----------------------------------------------------------------------------
%% @doc Register a new port in the manager.
%% @spec reg_port(State, Name, Type, Sup, Worker) -> {ok, NewState}
%% where
%%      State = #pm_state{}
%%      Name = string()
%%      Type = port_type()
%%      Sup = pid()
%%      Worker = pid()
%%      NewState = #pm_state{}
%% @end
-spec(reg_port(State::#pm_state{}, Name::string(), Type::port_type(),
        Sup::pid(), Worker::pid()) ->
    {ok, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
reg_port(#pm_state{ports=Ps}=State, Name, Type, Sup, Worker) ->
    {ok, State#pm_state{ports=[{Name, Type, Sup, Worker}|Ps]}}.


%%-----------------------------------------------------------------------------
%% @doc Delete a port registration from the manager.
%% @spec dereg_port(State, Name) ->
%%      {ok, NewState} | {{error, bad_param}, State}
%% where
%%      State = #pm_state{}
%%      Name = string()
%%      NewState = #pm_state{}
%% @end
-spec(dereg_port(State::#pm_state{}, Name::string()) ->
    {ok | {error, bad_param}, NewState::#pm_state{}}).
%%-----------------------------------------------------------------------------
dereg_port(#pm_state{ports=Ps}=State, Name) ->
    case lists:keyfind(Name, 1, Ps)
        of {Name, _, _, _} ->
            {ok, State#pm_state{ports=lists:keydelete(Name, 1, Ps)}}
         ; false ->
            {{error, bad_param}, State}
    end.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

