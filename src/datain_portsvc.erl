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
-module(datain_portsvc).
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
-record(datain, {name :: string(), % The port's name
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
    gen_server:start_link(datain_portsvc, {Name, Owner, Config}, []).


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
%%      State = #datain{}
%%      Reason = any()
%% @end
-spec(init({Name::string(), Owner::pid(), Config::config()}) ->
    {ok, State::#datain{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({Name, Owner, Config}) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_info, "Data-in PortService process initialising."),
    ?LOG(rtl_info, "Name: ~p, Owner: ~p, Config: ~p", [Name, Owner, Config]),
    {ok, #datain{name=Name, owner=Owner, cfg=Config}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = {set_conns_sup, Pid} | {set_buf, Pid} |
%%          {set_corba_obj, Obj} | get_corba_obj | get_port_prof |
%%          get_conn_profs | {get_conn_prof, Id} | {connect, ConnProf} |
%%          {disconnect, Id} | disconnect_all | {notify_conn, ConnProf} |
%%          {notify_dis, Id} | get_datatype | is_new | read
%%      Pid = pid()
%%      Obj = object_ref()
%%      Id = string()
%%      ConnProf = #conn_prof{}
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #datain{}
%%      Reply = ok | object_ref() | #port_prof{} | [ConnProf] | ConnProf |
%%          {rtc_cb_return(), ConnProf} | rtc_cb_return() | string() |
%%          boolean | {ok, any()} | read_failed
%%      NewState = #datain{}
%% @end
-spec(handle_call(Request::{set_conns_sup, Pid::pid()} |
        {set_buf, Pid::pid()} | {set_corba_obj, Obj::object_ref()} |
        get_port_prof | get_conn_profs | {get_conn_prof, Id::string()} |
        {connect, ConnProf::#conn_prof{}} | {disconnect, Id::string()} |
        disconnect_all | {notify_conn, ConnProf::#conn_prof{}} |
        {notify_dis, Id::string()} | get_datatype | is_new | read,
        From::{pid(), Tag::any()}, State::#datain{}) ->
    {reply, Reply::ok | object_ref() | #port_prof{} | [#conn_prof{}] |
        #conn_prof{} | {rtc_cb_return(), #conn_prof{}} | rtc_cb_return() |
        string() | boolean() | {ok, Data::any()} | read_failed,
        NewState::#datain{}}).
%%-----------------------------------------------------------------------------
handle_call({set_conns_sup, Pid}, _From, State) ->
    ?LOG(rtl_debug, "Setting connections supervisor to ~p", [Pid]),
    {reply, ok, State#datain{conns_sup=Pid}};
handle_call({set_buf, Pid}, _From, State) ->
    ?LOG(rtl_debug, "Setting buffer server to ~p", [Pid]),
    {reply, ok, State#datain{buf=Pid}};
handle_call({set_corba_obj, Obj}, _From, State) ->
    ?LOG(rtl_debug, "Setting CORBA object to ~p", [Obj]),
    {reply, ok, State#datain{obj=Obj}};
handle_call(get_corba_obj, _From, #datain{obj=Obj}=State) ->
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
    ?LOG(rtl_debug, "connect response: ~p", [Result]),
    {reply, Result, NewState};
handle_call({disconnect, Id}, _From, State) ->
    {Result, NewState} = disconnect(State, Id),
    ?LOG(rtl_debug, "disconnect response: ~p", [Result]),
    {reply, Result, NewState};
handle_call(disconnect_all, _From, State) ->
    {Result, NewState} = disconnect_all(State),
    ?LOG(rtl_debug, "disconnect_all response: ~p", [Result]),
    {reply, Result, NewState};
handle_call({notify_conn, ConnProf}, _From, State) ->
    {Result, NewState} = notify_connect(State, ConnProf),
    ?LOG(rtl_debug, "notify_conn response: ~p", [Result]),
    {reply, Result, NewState};
handle_call({notify_dis, Id}, _From, State) ->
    {Result, NewState} = notify_disconnect(State, Id),
    ?LOG(rtl_debug, "notify_diss response: ~p", [Result]),
    {reply, Result, NewState};
handle_call(get_datatype, _From, State) ->
    {Result, NewState} = get_datatype(State),
    ?LOG(rtl_debug, "get_datatype response: ~p", [Result]),
    {reply, Result, NewState};
handle_call(is_new, _From, State) ->
    Result = is_new(State),
    ?LOG(rtl_debug, "is_new response: ~p", [Result]),
    {reply, Result, State};
handle_call(read, _From, State) ->
    Result = read(State),
    ?LOG(rtl_debug, "read response: ~p", [Result]),
    {reply, Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #datain{}
%%      NewState = #datain{}
%% @end
-spec(handle_cast(Request::any(), State::#datain{}) ->
    {noreply, NewState::#datain{}} | {stop, normal, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = #datain{}
%%      NewState = #datain{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#datain{}) ->
    {noreply, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #datain{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#datain{}) -> ok).
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
%%      State = #datain{}
%% @end
-spec(cleanup(State::#datain{}) -> ok).
%%-----------------------------------------------------------------------------
cleanup(_State) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = #datain{}
%%      Extra = any()
%%      NewState = #datain{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#datain{},
        Extra::any()) -> {ok, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profile of the port from the state.
%% @spec get_port_prof(State) -> Result
%% where
%%      State = #datain{}
%%      Result = {#port_prof{}, NewState}
%%      NewState = #datain{}
%% @end
-spec(get_port_prof(State::#datain{}) ->
    Result::{#port_prof{}, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
get_port_prof(#datain{name=N, owner=O, cfg=C}=State) ->
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
%%      State = #datain{}
%%      Result = {[#conn_prof{}], NewState}
%%      NewState = #datain{}
%% @end
-spec(get_conn_profs(State::#datain{}) ->
    Result::{[#conn_prof{}], NewState::#datain{}}).
%%-----------------------------------------------------------------------------
get_conn_profs(#datain{conns=C}=State) ->
    ?LOG(rtl_paranoid, "Getting connector profiles."),
    Result = lists:map(fun({_, _, W}) -> connection:get_prof(W) end, C),
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the profile of one connection.
%% @spec get_conn_prof(State, Id) -> Result
%% where
%%      State = #datain{}
%%      Id = string()
%%      Result = {#conn_prof{}, NewState}
%%      NewState = #datain{}
%% @end
-spec(get_conn_prof(State::#datain{}, Id::string()) ->
    Result::{#conn_prof{}, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
get_conn_prof(#datain{conns=C}=State, Id) ->
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
%%      State = #datain{}
%%      ConnProf = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #datain{}
%% @end
-spec(connect(State::#datain{}, ConnProf::#conn_prof{}) ->
    Result::{{rtc_cb_return(), NewConnProf::#conn_prof{}},
        NewState::#datain{}}).
%%-----------------------------------------------------------------------------
connect(State, ConnProf) ->
    ?LOG(rtl_debug, "Starting connect process; profile: ~p", [ConnProf]),
    case validate_connprof(State, ConnProf)
        of true ->
            connect1(State, ConnProf)
         ; false ->
            {{bad_param, ConnProf}, State}
    end.

-spec(connect1(State::#datain{}, ConnProf::#conn_prof{}) ->
    Result::{{rtc_cb_return(), NewConnProf::#conn_prof{}},
        NewState::#datain{}}).
connect1(#datain{obj=O}=State, #conn_prof{ports=Ports}=ConnProf) ->
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
%%      State = #datain{}
%%      ConnProf = #conn_prof{}
%% @end
-spec(validate_connprof(State::#datain{}, ConnProf::#conn_prof{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
validate_connprof(_, #conn_prof{ports=[]}) ->
    ?LOG(rtl_error, "Connector profile has no ports."),
    false;
validate_connprof(_, #conn_prof{ports=[_]}) ->
    ?LOG(rtl_error, "Connector profile has only one port."),
    false;
validate_connprof(#datain{cfg=C}, #conn_prof{props=P}) ->
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
%%      State = #datain{}
%%      Id = string()
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #datain{}
%% @end
-spec(disconnect(State::#datain{}, Id::string()) ->
    Result::{rtc_cb_return(), NewState::#datain{}}).
%%-----------------------------------------------------------------------------
disconnect(State, Id) ->
    ?LOG(rtl_debug, "Starting disconnect process on connection ~p", [Id]),
    notify_disconnect(State, Id).


%%-----------------------------------------------------------------------------
%% @doc Disconnect all connections.
%% @spec disconnect_all(State) -> Result
%% where
%%      State = #datain{}
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #datain{}
%% @end
-spec(disconnect_all(State::#datain{}) ->
    Result::{rtc_cb_return(), NewState::#datain{}}).
%%-----------------------------------------------------------------------------
disconnect_all(#datain{conns=C}=State) ->
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
%%      State = #datain{}
%%      ConnProf = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #datain{}
%% @end
-spec(notify_connect(State::#datain{}, ConnProf::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#datain{}}).
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
%%      State = #datain{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #datain{}
%% @end
-spec(pure_erlang_conn(State::#datain{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
pure_erlang_conn(State, #conn_prof{ports=Ports}=Prof) ->
    ?LOG(rtl_debug, "Making connection via Erlang interface."),
    Self = self(),
    IsNotMe = fun(P) -> case P of Self -> false; _ -> true end end,
    % Add the connection and record it in the profile
    {{ok, Prof1}, State1} = add_connection(State, Prof),
    Prof2 = publish_erlang_listener(State1, Prof1),
    ?LOG(rtl_debug, "Added myself (~p) to the profile: ~n~p", [self(), Prof2]),
    {Res, FinalProf} = case lists:dropwhile(IsNotMe, Ports)
        of [Self, Next|_] ->
            portsvc:notify_connect(Next, Prof2)
         ; [Self] ->
            {ok, Prof2}
    end,
    {{Res, FinalProf}, State1}.


%%-----------------------------------------------------------------------------
%% @doc Add an Erlang connection listener to a connection profile.
%% @spec publish_erlang_listener(State, Prof) -> NewProf
%% where
%%      State = #datain{}
%%      Prof = NewProf = #conn_prof{}
%% @end
-spec(publish_erlang_listener(State::#datain{}, Prof::#conn_prof{}) ->
    NewProf::#conn_prof{}).
%%-----------------------------------------------------------------------------
publish_erlang_listener(#datain{conns=[{_, _, W}|_]},
        #conn_prof{props=Props}=Prof) ->
    case config:get_value(["dataport", "erlang", "listener"], Props)
        of undefined ->
            Prof#conn_prof{props=config:set_value(["dataport", "erlang",
                        "listener"], [W], Props)}
         ; Value ->
            Prof#conn_prof{props=config:set_value(["dataport", "erlang",
                        "listener"], [W|Value], Props)}
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle a notify_connect call for a connection involving CORBA objects.
%% @spec corba_intf_conn(State, Prof) -> Result
%% where
%%      State = #datain{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #datain{}
%% @end
-spec(corba_intf_conn(State::#datain{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
corba_intf_conn(#datain{obj=O}=State, #conn_prof{ports=Ports}=Prof) ->
    ?LOG(rtl_debug, "Making connection via CORBA interface."),
    IsNotMe = fun(P) -> case P of O -> false; _ -> true end end,
    % Add the connection and record it in the profile
    {{ok, Prof1}, State1} = add_connection(State, Prof),
    Prof2 = publish_corba_listener(State1, Prof1),
    ?LOG(rtl_debug, "Added myself (~p) to the profile: ~n~p", [self(), Prof2]),
    {Res, FinalProf} = case lists:dropwhile(IsNotMe, Ports)
        of [O, Next|_] ->
            {RC, CorbaProf} = 'RTC_PortService':notify_connect(Next,
                corbafy_connprof(Prof2)),
            {utils:from_rtc_rc(RC), decorbafy_connprof(CorbaProf)}
         ; [O] ->
            {ok, Prof2}
    end,
    {{Res, FinalProf}, State1}.


%%-----------------------------------------------------------------------------
%% @doc Add a CORBA connection listener to a connection profile.
%% @spec publish_corba_listener(State, Prof) -> NewProf
%% where
%%      State = #datain{}
%%      Prof = NewProf = #conn_prof{}
%% @end
-spec(publish_corba_listener(State::#datain{}, Prof::#conn_prof{}) ->
    NewProf::#conn_prof{}).
%%-----------------------------------------------------------------------------
publish_corba_listener(#datain{conns=[{_, _, W}|_]},
        #conn_prof{props=Props}=Prof) ->
    IOR = connection:get_ior(W),
    Prof#conn_prof{props=config:set_value(["dataport", "corba_cdr",
                "inport_ior"], IOR, Props)}.


%%-----------------------------------------------------------------------------
%% @doc Add a new connection.
%% @spec add_connection(State, Prof) -> Result
%% where
%%      State = #datain{}
%%      Prof = #conn_prof{}
%%      Result = {{rtc_cb_return(), NewConnProf}, NewState}
%%      NewConnProf = #conn_prof{}
%%      NewState = #datain{}
%% @end
-spec(add_connection(State::#datain{}, Prof::#conn_prof{}) ->
    {{rtc_cb_return(), NewConnProf::#conn_prof{}}, NewState::#datain{}}).
%%-----------------------------------------------------------------------------
add_connection(State, #conn_prof{id=""}=Prof) ->
    Id = io_lib:format("~p", [make_ref()]),
    ?LOG(rtl_debug, "Generated ID for connection: ~p", [Id]),
    add_connection(State, Prof#conn_prof{id=Id});
add_connection(#datain{conns_sup=S, conns=C, buf=Buf}=State,
        #conn_prof{id=Id}=Prof) ->
    case supervisor:start_child(S, {Id, {connection, create,
                    [inport, Prof, Buf]}, temporary, 30000,
            supervisor, [connection]})
        of {ok, Sup, Worker} ->
            ?LOG(rtl_debug, "Created new inport connection ~p:~p with ID ~p",
                [Sup, Worker, Id]),
            {{ok, Prof}, State#datain{conns=[{Id, Sup, Worker} | C]}}
         ; {error, Reason} ->
            ?LOG(rtl_error, "Failed to create new connection process: ~p",
                [Reason]),
            {{error, Prof}, State}
    end.


%%-----------------------------------------------------------------------------
%% @doc Notify this port that it is being disconnected.
%% @spec notify_disconnect(State, Id) -> Result
%% where
%%      State = #datain{}
%%      Id = string()
%%      Result = {rtc_cb_return(), NewState}
%%      NewState = #datain{}
%% @end
-spec(notify_disconnect(State::#datain{}, Id::string()) ->
    {rtc_cb_return(), NewState::#datain{}}).
%%-----------------------------------------------------------------------------
notify_disconnect(#datain{conns=Conns, obj=O}=State, Id) ->
    ?LOG(rtl_debug, "notify_disconnect call; ID is ~p", [Id]),
    case lists:keyfind(Id, 1, Conns)
        of {Id, Sup, Worker} ->
            Prof = connection:get_prof(Worker),
            connection:disconnect(Sup),
            Res = disconnect_next(Id, Prof, O),
            ?LOG(rtl_debug, "disconnect_next result is ~p", [Res]),
            {Res, State#datain{conns=lists:keydelete(Id, 1, Conns)}}
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
            utils:from_rtc_rc('RTC_PortService':notify_disconnect(Next, Id))
         ; [_] ->
            ok
    end.


%%-----------------------------------------------------------------------------
%% @doc Get the datatype transported by this port.
%% @spec get_datatype(State) -> Result
%% where
%%      State = #datain{}
%%      Result = string()
%% @end
-spec(get_datatype(State::#datain{}) -> Result::string()).
%%-----------------------------------------------------------------------------
get_datatype(#datain{cfg=C}=State) ->
    ?LOG(rtl_debug, "Getting data type from config."),
    Result = config:get_value(["dataport", "data_type"], C),
    {Result, State}.


%%-----------------------------------------------------------------------------
%% @doc Check if there is data waiting to be read.
%% @spec is_new(State) -> Result
%% where
%%      State = #datain{}
%%      Result = boolean()
%% @end
-spec(is_new(State::#datain{}) -> Result::boolean()).
%%-----------------------------------------------------------------------------
is_new(#datain{buf=Buf}) ->
    case buffer_svr:count(Buf)
        of 0 ->
            false
         ; _ ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @doc Read the latest data (if any).
%% @spec read(State) -> Result
%% where
%%      State = #datain{}
%%      Result = {ok, any()} | read_failed
%% @end
-spec(read(State::#datain{}) -> Result::{ok, Data::any()} | read_failed).
%%-----------------------------------------------------------------------------
read(#datain{buf=Buf, conns=Conns}) ->
    read_conns(Conns),
    case buffer_svr:read(Buf)
        of {error, Reason} ->
            ?LOG(rtl_debug, "Read from datain port failed with reason ~p",
                [Reason]),
            read_failed
         ; Value ->
            Value
    end.


%%-----------------------------------------------------------------------------
%% @doc Read each connector.
%% @spec read_conns(Conns) -> ok
%% where
%%      Conns = [{Id::string(), Sup::pid(), Worker::pid()}]
%% @end
-spec(read_conns(Conns::[{Id::string(), Sup::pid(), Worker::pid()}]) -> ok).
%%-----------------------------------------------------------------------------
read_conns([]) ->
    ok;
read_conns([{_, _, W} | T]) ->
    connection:read(W),
    read_conns(T).


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

