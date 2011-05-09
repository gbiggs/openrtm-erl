%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Listener for push connections.
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
-module(put_listener).
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
-record(pl, {buf :: pid(), % Owning port's buffer
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
%%      State = #pl{}
%%      Reason = any()
%% @end
-spec(init(Prof::#conn_prof{}) ->
    {ok, State::#pl{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(Prof) ->
    process_flag(trap_exit, true),
    State = #pl{buf=nil, prof=Prof},
    ?LOG(rtl_debug, "Put connector ~p initialised: ~p.", [self(), State]),
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = get_prof | get_ior | {set_corba_obj, Obj} |
%%          {set_buf, pid()} | {put, any()} | read
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #pl{}
%%      Reply = #conn_prof{} | string() | ok
%%      NewState = #pl{}
%% @end
-spec(handle_call(Request::get_prof | get_ior |
        {set_corba_obj, Obj::object_ref()} | {set_buf, Buf::pid()} |
        {put, Data::any()} | read,
        From::{pid(), Tag::any()}, State::#pl{}) ->
    {reply, Reply::#conn_prof{} | string() | ok, NewState::#pl{}}).
%%-----------------------------------------------------------------------------
handle_call(get_prof, _From, #pl{prof=Prof}=State) ->
    ?LOG(rtl_debug, "get_prof reply: ~p", [Prof]),
    {reply, Prof, State};
handle_call(get_ior, _From, State) ->
    IOR = get_ior(State),
    ?LOG(rtl_debug, "get_ior reply: ~p", [IOR]),
    {reply, IOR, State};
handle_call({set_corba_obj, Obj}, _From, State) ->
    ?LOG(rtl_debug, "Setting CORBA object to ~p", [Obj]),
    {reply, ok, State#pl{obj=Obj}};
handle_call({set_buf, Buf}, _From, State) ->
    ?LOG(rtl_debug, "Set target buffer for put listener ~p to ~p",
        [self(), Buf]),
    {reply, ok, State#pl{buf=Buf}};
handle_call({put, Data}, _From, State) ->
    ?LOG(rtl_debug, "Put listener ~p received data ~p", [self(), Data]),
    {reply, put_data(State, Data), State};
handle_call(read, _From, State) ->
    % This listener does not explicitly read.
    {reply, ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #pl{}
%%      NewState = #pl{}
%% @end
-spec(handle_cast(Request::any(), State::#pl{}) ->
    {stop, normal, NewState::#pl{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    ?LOG(rtl_debug, "Put connector ~p stopping.", [self()]),
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = #pl{}
%%      NewState = #pl{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#pl{}) ->
    {noreply, NewState::#pl{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #pl{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#pl{}) -> ok).
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
%%      State = #pl{}
%%      Extra = any()
%%      NewState = #pl{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#pl{},
        Extra::any()) -> {ok, NewState::#pl{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Get the IOR of the related CORBA interface.
%% @spec get_ior(State) -> IOR
%% where
%%      State = #pl{}
%%      IOR = string()
%% @end
-spec(get_ior(State::#pl{}) -> IOR::string()).
%%-----------------------------------------------------------------------------
get_ior(#pl{obj=nil}) ->
    "";
get_ior(#pl{obj=Obj}) ->
    corba:object_to_string(Obj).


%%-----------------------------------------------------------------------------
%% @doc Put data into the port's buffer.
%% @spec put_data(State, Data) -> port_ok | port_err | buff_full |
%%      buff_empty | buff_timeout | unknown_err
%% where
%%      State = #pl{}
%%      Data = any()
%% @end
-spec(put_data(State::#pl{}, Data::any()) -> port_ok | port_err | buff_full |
        buff_empty | buff_timeout | unknown_err).
%%-----------------------------------------------------------------------------
put_data(#pl{buf=Buf}, Data) ->
    case buffer_svr:write(Buf, Data)
        of ok ->
            port_ok
         ; overflow ->
            buff_full
         ; {error, Reason} ->
            unknown_err
    end.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

