%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Push/new connection (outport side).
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
-module(push_new).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("port.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------
-record(pn, {erl_dests=[] :: [pid()], % Destination of Erlang connection.
        corba_dest=nil :: nil | string(), % Destination of CORBA connection.
        obj=nil :: object_ref() | nil, % CORBA object for the connection
        prof :: #conn_prof{} % Profile of the connection
    }).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/1, stop_server/1]).


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
%% @spec start_link(Prof) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Prof = #conn_prof{}
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(Prof::#conn_prof{}) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(Prof) ->
    gen_server:start_link(?MODULE, Prof, []).


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


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init(Prof) -> {ok, State} | {stop, Reason}
%% where
%%      Prof = #conn_prof{}
%%      State = #pn{}
%%      Reason = any()
%% @end
-spec(init(Prof::#conn_prof{}) ->
    {ok, State::#pn{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(Prof) ->
    process_flag(trap_exit, true),
    State = get_dests(#pn{prof=Prof}),
    ?LOG(rtl_debug, "Push/New connector ~p initialised: ~p.", [self(), State]),
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = get_prof | {set_corba_obj, Obj} | {set_buf, pid()} |
%%          {write, any()}
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #pn{}
%%      Reply = #conn_prof{} | ok
%%      NewState = #pn{}
%% @end
-spec(handle_call(Request::get_prof | {set_corba_obj, Obj::object_ref()} |
        {set_buf, Buf::pid()} | {write, Data::any()},
        From::{pid(), Tag::any()}, State::#pn{}) ->
    {reply, Reply::#conn_prof{} | ok, NewState::#pn{}}).
%%-----------------------------------------------------------------------------
handle_call(get_prof, _From, #pn{prof=Prof}=State) ->
    ?LOG(rtl_debug, "get_prof reply: ~p", [Prof]),
    {reply, Prof, State};
handle_call({set_corba_obj, Obj}, _From, State) ->
    ?LOG(rtl_debug, "Setting CORBA object to ~p", [Obj]),
    {reply, ok, State#pn{obj=Obj}};
handle_call({set_buf, _}, _From, State) ->
    {reply, ok, State};
handle_call({write, Data}, _From, State) ->
    {reply, write_data(State, Data), State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #pn{}
%%      NewState = #pn{}
%% @end
-spec(handle_cast(Request::any(), State::#pn{}) ->
    {stop, normal, NewState::#pn{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    ?LOG(rtl_debug, "Push/New connector ~p stopping.", [self()]),
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = #pn{}
%%      NewState = #pn{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#pn{}) ->
    {noreply, NewState::#pn{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #pn{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#pn{}) -> ok).
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
%%      State = #pn{}
%%      Extra = any()
%%      NewState = #pn{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#pn{},
        Extra::any()) -> {ok, NewState::#pn{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Push the data to the destination.
%% @spec write_data(State, Data) -> ok
%% where
%%      State = #pl{}
%%      Data = any()
%% @end
-spec(write_data(State::#pn{}, Data::any()) -> ok).
%%-----------------------------------------------------------------------------
write_data(#pn{erl_dests=EDests, corba_dest=CDest}, Data) ->
    Res = write_data_erl(EDests, Data, port_ok),
    case write_data_corba(CDest, Data)
        of port_ok ->
            Res
         ; Other ->
            Other
    end.

write_data_erl([], _, Res) ->
    Res;
write_data_erl([Dest|T], Data, Res) ->
    ?LOG(rtl_debug, "Push/new is writing data ~p to Erlang destinations ~p.",
        [Data, Dest]),
    case connection:put(Dest, Data)
        of port_ok ->
            write_data_erl(T, Data, Res)
         ; Other ->
            write_data_erl(T, Data, Other)
    end.

write_data_corba(nil, _) ->
    port_ok;
write_data_corba(Dest, Data) ->
    ?LOG(rtl_debug, "Push/new is writing data ~p to CORBA destination~n~p.",
        [Data, Dest]),
    R = 'OpenRTM_InPortCdr':put(Dest, Data),
    ?LOG(rtl_debug, "Put result: ~p", [R]),
    utils:port_rc_from_corba(R).


%%-----------------------------------------------------------------------------
%% @doc Get the destinations from the connection profile.
%% @spec get_dests(#pn{}) -> #pn{}
%% @end
-spec(get_dests(State::#pn{}) -> NewState::#pn{}).
%%-----------------------------------------------------------------------------
get_dests(State) ->
    get_corba_dests(get_erl_dests(State)).

-spec(get_erl_dests(#pn{}) -> #pn{}).
get_erl_dests(#pn{prof=#conn_prof{props=P}}=State) ->
    case config:get_value(["dataport", "erlang", "listener"], P)
        of undefined ->
            State
         ; Ls ->
            State#pn{erl_dests=Ls}
    end.

-spec(get_corba_dests(#pn{}) -> #pn{}).
get_corba_dests(#pn{prof=#conn_prof{props=P}}=State) ->
    case config:get_value(["dataport", "corba_cdr", "inport_ior"], P)
        of undefined ->
            State
         ; IOR ->
            % Resolve the IOR
            Obj = corba:string_to_object(IOR),
            ?LOG(rtl_debug, "Resolved IOR ~p to object~n~p", [IOR, Obj]),
            State#pn{corba_dest=Obj}
    end.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

