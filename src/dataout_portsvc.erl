%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Implementation of the PortService for a data in port.
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
-module(dataout_portsvc).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("port.hrl").
-include("idl/RTC.hrl").


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(dataout, {name :: string(), % The port's name
        owner :: pid() | nil, % The port's owner; should be an RTC
        cfg :: config(), % Configuration parameters used to set up the port
        conns_sup=nil :: pid() | nil, % Supervisor managing connection workers
        conns=[] :: [{Id::string(), Sup::pid(), Worker::pid()}], % Connections
        obj=nil :: object_ref() | nil, % CORBA object for the port
        buf=nil :: pid() | nil % Process managing the buffer
    }).


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/3, stop_server/1]).


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
%% @spec start_link(Name, Owner, Config) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Name = string()
%%      Owner = pid() | nil
%%      Config = config()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(Name::string(), Owner::pid() | nil, Config::config()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(Name, Owner, Config) ->
    gen_server:start_link(dataout_portsvc, {Name, Owner, Config}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server(Pid) -> ok
%% where
%%      Pid = pid()
%% @end
-spec(stop_server(Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_server(Pid) ->
    gen_server:cast(Pid, stop).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init({Name, Owner, Config}) -> {ok, State} | {stop, Reason}
%% where
%%      Name = string()
%%      Owner = pid() | nil
%%      Config = config()
%%      State = #dataout{}
%%      Reason = any()
%% @end
-spec(init({Name::string(), Owner::pid(), Config::config()}) ->
    {ok, State::#dataout{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({Name, Owner, Config}) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_info, "Data-in PortService process initialising."),
    ?LOG(rtl_info, "Name: ~p, Owner: ~p, Config: ~p", [Name, Owner, Config]),
    {ok, #dataout{name=Name, owner=Owner, cfg=Config}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = {set_conns_sup, Pid} | {set_buf, Pid} |
%%          {set_corba_obj, Obj} | get_corba_obj | get_port_prof |
%%          get_conn_profs | {get_conn_prof, Id} | {connect, ConnProf} |
%%          {disconnect, Id} | disconnect_all | {notify_conn, ConnProf} |
%%          {notify_dis, Id} | get_datatype | {write, any()}
%%      Pid = pid()
%%      Obj = object_ref()
%%      Id = string()
%%      ConnProf = #conn_prof{}
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #dataout{}
%%      Reply = ok | object_ref() | #port_prof{} | [ConnProf] | ConnProf |
%%          {rtc_cb_return(), ConnProf} | rtc_cb_return() | string() |
%%          write_failed
%%      NewState = #dataout{}
%% @end
-spec(handle_call(Request::{set_conns_sup, Pid::pid()} |
        {set_buf, Pid::pid()} | {set_corba_obj, Obj::object_ref()} |
        get_port_prof | get_conn_profs | {get_conn_prof, Id::string()} |
        {connect, ConnProf::#conn_prof{}} | {disconnect, Id::string()} |
        disconnect_all | {notify_conn, ConnProf::#conn_prof{}} |
        {notify_dis, Id::string()} | get_datatype | {write, Data::any()},
        From::{pid(), Tag::any()}, State::#dataout{}) ->
    {reply, Reply::ok | object_ref() | #port_prof{} | [#conn_prof{}] |
        #conn_prof{} | {rtc_cb_return(), #conn_prof{}} | rtc_cb_return() |
        string() | write_failed, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
handle_call({set_conns_sup, Pid}, _From, State) ->
    ?LOG(rtl_debug, "Setting connections supervisor to ~p", [Pid]),
    {reply, ok, State#dataout{conns_sup=Pid}};
handle_call({set_buf, Pid}, _From, State) ->
    ?LOG(rtl_debug, "Setting buffer server to ~p", [Pid]),
    {reply, ok, State#dataout{buf=Pid}};
handle_call({set_corba_obj, Obj}, _From, State) ->
    ?LOG(rtl_debug, "Setting CORBA object to ~p", [Obj]),
    {reply, ok, State#dataout{obj=Obj}};
handle_call(get_corba_obj, _From, #dataout{obj=Obj}=State) ->
    ?LOG(rtl_debug, "Handling get_corba_obj: ~p", [Obj]),
    {reply, Obj, State};
handle_call(get_port_prof, _From, State) ->
    {Result, NewState} = get_port_prof(State),
    ?LOG(rtl_debug, "get_port_prof response: ~p", [Result]),
    {reply, Result, NewState};
handle_call(get_conn_profs, _From, State) ->
    {Result, NewState} = get_conn_profs(State),
    ?LOG(rtl_debug, "get_conn_profs response: ~p", [Result]),
    {reply, Result, NewState};
handle_call({get_conn_prof, Id}, _From, State) ->
    {Result, NewState} = get_conn_prof(State, Id),
    ?LOG(rtl_debug, "get_conn_prof(~p) response: ~p", [Id, Result]),
    {reply, Result, NewState};
handle_call({connect, ConnProf}, _From, State) ->
    {Result, NewState} = connect(State, ConnProf),
    {reply, Result, NewState};
handle_call({disconnect, Id}, _From, State) ->
    {Result, NewState} = disconnect(State, Id),
    {reply, Result, NewState};
handle_call(disconnect_all, _From, State) ->
    {Result, NewState} = disconnect_all(State),
    {reply, Result, NewState};
handle_call({notify_conn, ConnProf}, _From, State) ->
    {Result, NewState} = notify_connect(State, ConnProf),
    {reply, Result, NewState};
handle_call({notify_dis, Id}, _From, State) ->
    {Result, NewState} = notify_disconnect(State, Id),
    {reply, Result, NewState};
handle_call(get_datatype, _From, State) ->
    {Result, NewState} = get_datatype(State),
    {reply, Result, NewState};
handle_call({write, Data}, _From, State) ->
    Result = write(State, Data),
    ?LOG(rtl_debug, "write(~p) response: ~p", [Data, Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #dataout{}
%%      NewState = #dataout{}
%% @end
-spec(handle_cast(Request::any(), State::#dataout{}) ->
    {noreply, NewState::#dataout{}} | {stop, normal, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = #dataout{}
%%      NewState = #dataout{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#dataout{}) ->
    {noreply, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #dataout{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#dataout{}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, State) ->
    ?LOG(rtl_info, "Shutting down normally."),
    cleanup(State);
terminate(shutdown, State) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    cleanup(State);
terminate({shutdown, Reason}, State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    cleanup(State);
terminate(Reason, State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    cleanup(State).


%%-----------------------------------------------------------------------------
%% @doc Clean up for shutdown.
%% @spec cleanup(State) -> ok
%% where
%%      State = #dataout{}
%% @end
-spec(cleanup(State::#dataout{}) -> ok).
%%-----------------------------------------------------------------------------
cleanup(_State) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #dataout{}
%%      Extra = any()
%%      NewState = #dataout{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#dataout{},
        Extra::any()) -> {ok, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profile of the port from the state.
%% @spec get_port_prof(State) -> Result
%% where
%%      State = #dataout{}
%%      Result = {#port_prof{}, NewState}
%%      NewState = #dataout{}
%% @end
-spec(get_port_prof(State::#dataout{}) ->
    Result::{#port_prof{}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
get_port_prof(#dataout{name=N, owner=O, cfg=C}=State) ->
    ?LOG(rtl_paranoid, "Getting port profile."),
    {ConnProfs, _} = get_conn_profs(State),
    Result = #port_prof{name=N,
        interfaces=[],
        port_ref=self(),
        conn_profs=ConnProfs,
        owner=O,
        props=C},
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profiles of all connections.
%% @spec get_conn_profs(State) -> Result
%% where
%%      State = #dataout{}
%%      Result = {[#conn_prof{}], NewState}
%%      NewState = #dataout{}
%% @end
-spec(get_conn_profs(State::#dataout{}) ->
    Result::{[#conn_prof{}], NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
get_conn_profs(#dataout{conns=C}=State) ->
    ?LOG(rtl_paranoid, "Getting connector profiles."),
    Result = lists:map(fun({_, _, W}) -> connection:get_prof(W) end, C),
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profile of one connection.
%% @spec get_conn_prof(State, Id) -> Result
%% where
%%      State = #dataout{}
%%      Id = string()
%%      Result = {#conn_prof{}, NewState}
%%      NewState = #dataout{}
%% @end
-spec(get_conn_prof(State::#dataout{}, Id::string()) ->
    Result::{#conn_prof{}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
get_conn_prof(#dataout{conns=C}=State, Id) ->
    ?LOG(rtl_paranoid, "Getting connector profile for connector ID ~p", [Id]),
    Result = case lists:keyfind(Id, 1, C)
        of {Id, _, W} ->
            connection:get_prof(W)
         ; false ->
            #conn_prof{name="", id="", ports=[], props=[]}
    end,
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Connect this port to others.
%% @spec connect(State, ConnProf) -> Result
%% where
%%      State = #dataout{}
%%      ConnProf = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #dataout{}
%% @end
-spec(connect(State::#dataout{}, ConnProf::#conn_prof{}) ->
    Result::{{rtc_cb_return(), NewConnProf::#conn_prof{}},
        NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
connect(State, ConnProf) ->
    ?LOG(rtl_debug, "Starting connect process; profile: ~p", [ConnProf]),
    case validate_connprof(State, ConnProf)
        of true ->
            connect1(State, ConnProf)
         ; false ->
            {{bad_param, ConnProf}, State}
    end.

-spec(connect1(State::#dataout{}, ConnProf::#conn_prof{}) ->
    Result::{{rtc_cb_return(), NewConnProf::#conn_prof{}},
        NewState::#dataout{}}).
connect1(#dataout{obj=O}=State, #conn_prof{ports=Ports}=ConnProf) ->
    ConnProf1 = case is_pure_erlang(ConnProf)
        of true ->
            % Move this port to the front of the list to get correct indexing
            % during the notify_connect calls without missing ports.
            ConnProf#conn_prof{ports=[self() |
                    lists:delete(self(), lists:delete(O, Ports))]}
         ; false ->
            % Move this port to the front of the list to get correct indexing
            % during the notify_connect calls without missing ports.
            ConnProf#conn_prof{ports=[O |
                    lists:delete(self(), lists:delete(O,
                            use_corba_refs(Ports)))]}
    end,
    notify_connect(State, ConnProf1).


%%-----------------------------------------------------------------------------
%% @doc Validate that the options in a connector profile are correct.
%% @spec validate_connprof(State, ConnProf) -> boolean()
%% where
%%      State = #dataout{}
%%      ConnProf = #conn_prof{}
%% @end
-spec(validate_connprof(State::#dataout{}, ConnProf::#conn_prof{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
validate_connprof(_, #conn_prof{ports=[]}) ->
    ?LOG(rtl_error, "Connector profile has no ports."),
    false;
validate_connprof(_, #conn_prof{ports=[_]}) ->
    ?LOG(rtl_error, "Connector profile has only one port."),
    false;
validate_connprof(#dataout{cfg=C}, #conn_prof{props=P}) ->
    config:compare_split(C, P, true).


%%-----------------------------------------------------------------------------
%% @doc Change a list of ports to use only CORBA object references.
%% @spec use_corba_refs(Ports0) -> Ports1
%% where
%%      Ports0 = [pid() | object_ref()]
%%      Ports1 = [pid() | object_ref()]
%% @end
-spec(use_corba_refs(Ports0::[pid() | object_ref()]) ->
        Ports1::[pid() | object_ref()]).
%%-----------------------------------------------------------------------------
use_corba_refs(Ports) ->
    use_corba_refs1(Ports, []).

use_corba_refs1([], Acc) ->
    lists:reverse(Acc);
use_corba_refs1([H|T], Acc) when is_pid(H) ->
    use_corba_refs1(T, [portsvc:get_corba_obj(H) | Acc]);
use_corba_refs1([H|T], Acc) ->
    use_corba_refs1(T, [H|Acc]).


%%-----------------------------------------------------------------------------
%% @doc Disconnect a specific connection.
%% @spec disconnect(State, Id) -> Result
%% where
%%      State = #dataout{}
%%      Id = string()
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #dataout{}
%% @end
-spec(disconnect(State::#dataout{}, Id::string()) ->
    Result::{rtc_cb_return(), NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
disconnect(State, Id) ->
    ?LOG(rtl_debug, "Starting disconnect process on connection ~p", [Id]),
    notify_disconnect(State, Id).


%%-----------------------------------------------------------------------------
%% @doc Disconnect all connections.
%% @spec disconnect_all(State) -> Result
%% where
%%      State = #dataout{}
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #dataout{}
%% @end
-spec(disconnect_all(State::#dataout{}) ->
    Result::{rtc_cb_return(), NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
disconnect_all(#dataout{conns=C}=State) ->
    ?LOG(rtl_debug, "Starting disconnect_all process"),
    F = fun({Id, _, _}, {Res, AccState}) ->
        {Res1, AccState1} = disconnect(AccState, Id),
        {merge_rc(Res, Res1), AccState1}
    end,
    lists:foldl(F, {ok, State}, C).

%%-----------------------------------------------------------------------------
%% @doc Notify this port that it is being connected.
%% @spec notify_connect(State, ConnProf) -> Result
%% where
%%      State = #dataout{}
%%      ConnProf = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #dataout{}
%% @end
-spec(notify_connect(State::#dataout{}, ConnProf::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
notify_connect(State, ConnProf) ->
    ?LOG(rtl_debug, "notify_connect call; profile is ~p", [ConnProf]),
    case is_pure_erlang(ConnProf)
        of true ->
            pure_erlang_conn(State, ConnProf)
         ; false ->
            corba_intf_conn(State, ConnProf)
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle a notify_connect call for a connection involving only Erlang
%% processes.
%% @spec pure_erlang_conn(State, Prof) -> Result
%% where
%%      State = #dataout{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #dataout{}
%% @end
-spec(pure_erlang_conn(State::#dataout{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
pure_erlang_conn(State, #conn_prof{ports=Ports}=Prof) ->
    ?LOG(rtl_debug, "Making connection via Erlang interface."),
    Self = self(),
    IsNotMe = fun(P) -> case P of Self -> false; _ -> true end end,
    {Res, Prof1} = case lists:dropwhile(IsNotMe, Ports)
        of [Self, Next|_] ->
            portsvc:notify_connect(Next, Prof)
         ; [Self] ->
            {ok, Prof}
    end,
    ?LOG(rtl_debug, "Intermediary result is ~p", [{Res, Prof1}]),
    case Res
        of ok ->
            add_connection(State, Prof1)
         ; _ ->
            {{Res, Prof1}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle a notify_connect call for a connection involving CORBA objects.
%% @spec corba_intf_conn(State, Prof) -> Result
%% where
%%      State = #dataout{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #dataout{}
%% @end
-spec(corba_intf_conn(State::#dataout{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
corba_intf_conn(#dataout{obj=O}=State, #conn_prof{ports=Ports}=Prof) ->
    ?LOG(rtl_debug, "Making connection via CORBA interface."),
    IsNotMe = fun(P) -> case P of O -> false; _ -> true end end,
    {Res, Prof1} = case lists:dropwhile(IsNotMe, Ports)
        of [O, Next|_] ->
            {RC, CorbaProf} = 'RTC_PortService':notify_connect(Next,
                corbafy_connprof(Prof)),
            {utils:from_rtc_rc(RC), decorbafy_connprof(CorbaProf)}
         ; [O] ->
            {ok, Prof}
    end,
    ?LOG(rtl_debug, "Intermediary result is ~p", [Res]),
    case Res
        of ok ->
            add_connection(State, Prof1)
         ; _ ->
            {{Res, Prof1}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Add a new connection.
%% @spec add_connection(State, Prof) -> Result
%% where
%%      State = #dataout{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #dataout{}
%% @end
-spec(add_connection(State::#dataout{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
add_connection(State, #conn_prof{id=""}=Prof) ->
    Id = io_lib:format("~p", [make_ref()]),
    ?LOG(rtl_debug, "Generated ID for connection: ~p", [Id]),
    add_connection(State, Prof#conn_prof{id=Id});
add_connection(#dataout{conns_sup=S, conns=C}=State,
        #conn_prof{id=Id}=Prof) ->
    ?LOG(rtl_debug, "Adding connection to ~p with ID ~p using profile~n~p",
        [S, Id, Prof]),
    case supervisor:start_child(S, {Id, {connection, create,
                    [outport, Prof, nil]}, temporary, 30000,
            supervisor, [connection]})
        of {ok, Sup, Worker} ->
            ?LOG(rtl_debug, "Created new outport connection ~p:~p with ID ~p",
                [Sup, Worker, Id]),
            {{ok, Prof}, State#dataout{conns=[{Id, Sup, Worker} | C]}}
         ; {error, Reason} ->
            ?LOG(rtl_error, "Failed to create new connection process: ~p",
                [Reason]),
            {{error, Prof}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Notify this port that it is being disconnected.
%% @spec notify_disconnect(State, Id) -> Result
%% where
%%      State = #dataout{}
%%      Id = string()
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #dataout{}
%% @end
-spec(notify_disconnect(State::#dataout{}, Id::string()) ->
    {rtc_cb_return(), NewState::#dataout{}}).
%%-----------------------------------------------------------------------------
notify_disconnect(#dataout{conns=Conns, obj=O}=State, Id) ->
    ?LOG(rtl_debug, "notify_disconnect call; ID is ~p", [Id]),
    case lists:keyfind(Id, 1, Conns)
        of {Id, Sup, Worker} ->
            Prof = connection:get_prof(Worker),
            connection:disconnect(Sup),
            Res = disconnect_next(Id, Prof, O),
            {Res, State#dataout{conns=lists:keydelete(Id, 1, Conns)}}
         ; false ->
            ?LOG(rtl_error, "Connection not found: ~p", [Id]),
            {bad_param, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Disconnect the next port in the chain.
%% @spec disconnect_next(Id, Prof, Obj) -> Result
%% where
%%      Id = string()
%%      Prof = #conn_prof{}
%%      Obj = object_ref()
%%      Result = rtc_cb_return()
%% @end
-spec(disconnect_next(Id::string(), Prof::#conn_prof{}, Obj::object_ref()) ->
    Result::rtc_cb_return()).
%%-----------------------------------------------------------------------------
disconnect_next(Id, #conn_prof{ports=Ports}, O) ->
    Self = self(),
    IsNotMe = fun(P) -> case P of O -> false; Self -> false; _ -> true end end,
    case lists:dropwhile(IsNotMe, Ports)
        % Note that dropwhile leaves the one that broke the pattern at the head
        of [_, Next|_] when is_pid(Next) ->
            portsvc:notify_disconnect(Next, Id)
         ; [_, Next|_] ->
            'RTC_PortService':notify_disconnect(Next, Id)
         ; [_] ->
            ok
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the datatype transported by this port.
%% @spec get_datatype(State) -> Result
%% where
%%      State = #dataout{}
%%      Result = string()
%% @end
-spec(get_datatype(State::#dataout{}) -> Result::string()).
%%-----------------------------------------------------------------------------
get_datatype(#dataout{cfg=C}=State) ->
    ?LOG(rtl_debug, "Getting data type from config."),
    Result = config:get_value(["dataport", "data_type"], C),
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Write data to connected ports.
%% @spec write(State, Data) -> Result
%% where
%%      State = #datain{}
%%      Data = any()
%%      Result = ok | write_failed
%% @end
-spec(write(State::#dataout{}, Data::any()) -> Result::ok | write_failed).
%%-----------------------------------------------------------------------------
write(#dataout{conns=Conns}, Data) ->
    write1(Conns, Data, ok).

write1([], _, Res) ->
    Res;
write1([{_, _, C}|T], Data, Res) ->
    case connection:write(C, Data)
        of ok ->
            write1(T, Data, Res)
         ; Other ->
            write1(T, Data, Other)
    end.


%%-----------------------------------------------------------------------------
%% @doc Check if a connection profile involves only native Erlang processes.
%% @spec is_pure_erlang(Prof) -> boolean()
%% where
%%      Prof = #conn_prof{}
%% @end
-spec(is_pure_erlang(Prof::#conn_prof{}) -> boolean()).
%%-----------------------------------------------------------------------------
is_pure_erlang(#conn_prof{ports=Ports}) ->
    lists:all(fun erlang:is_pid/1, Ports).


%%-----------------------------------------------------------------------------
%% @doc Change a connector profile from native to CORBA format.
%% @spec corbafy_connprof(#conn_prof{}) -> #'RTC_ConnectorProfile'{}
%% @end
-spec(corbafy_connprof(#conn_prof{}) -> #'RTC_ConnectorProfile'{}).
%%-----------------------------------------------------------------------------
corbafy_connprof(Prof) ->
    Ports = lists:map(fun corbafy_port_ref/1, Prof#conn_prof.ports),
    #'RTC_ConnectorProfile'{name=Prof#conn_prof.name,
        connector_id=Prof#conn_prof.id,
        ports=Ports,
        properties=nvlist:from_list(Prof#conn_prof.props)}.


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


%%-----------------------------------------------------------------------------
%% @doc Merge return codes, ensuring that errors triumph over success.
%% @spec merge_rc(rtc_cb_return(), rtc_cb_return()) -> rtc_cb_return()
%% @end
-spec(merge_rc(rtc_cb_return(), rtc_cb_return()) -> rtc_cb_return()).
%%-----------------------------------------------------------------------------
merge_rc(RC, _) when RC =/= ok ->
    RC;
merge_rc(_, RC) when RC =/= ok ->
    RC;
merge_rc(ok, ok) ->
    ok.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

