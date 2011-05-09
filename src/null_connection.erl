%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc A null connection between two ports. No data can be exchanged.
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
-module(null_connection).
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
%%      State = #conn_prof{}
%%      Reason = any()
%% @end
-spec(init(Prof::#conn_prof{}) ->
    {ok, State::#conn_prof{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init(Prof) ->
    process_flag(trap_exit, true),
    ?LOG(rtl_debug, "Null connector ~p initialised: ~p.", [self(), Prof]),
    {ok, Prof}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = get_prof | get_ior | {set_corba_obj, Obj} | {set_buf, pid()}
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #conn_prof{}
%%      Reply = #conn_prof{} | ok
%%      NewState = #conn_prof{}
%% @end
-spec(handle_call(Request::get_prof | get_ior |
        {set_corba_obj, Obj::object_ref()} | {set_buf, Buf::pid()},
        From::{pid(), Tag::any()}, State::#conn_prof{}) ->
    {reply, Reply::#conn_prof{} | string() | ok, NewState::#conn_prof{}}).
%%-----------------------------------------------------------------------------
handle_call(get_prof, _From, State) ->
    {reply, State, State};
handle_call(get_ior, _From, State) ->
    {reply, "", State};
handle_call({set_corba_obj, Obj}, _From, State) ->
    ?LOG(rtl_debug, "Ignoring set_corba_obj(~p)", [Obj]),
    {reply, ok, State};
handle_call({set_buf, _}, _From, State) ->
    {reply, ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #conn_prof{}
%%      NewState = #conn_prof{}
%% @end
-spec(handle_cast(Request::any(), State::#conn_prof{}) ->
    {stop, normal, NewState::#conn_prof{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    ?LOG(rtl_debug, "Null connector ~p stopping.", [self()]),
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {noreply, NewState}
%% where
%%      Info = timeout | any()
%%      State = #conn_prof{}
%%      NewState = #conn_prof{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#conn_prof{}) ->
    {noreply, NewState::#conn_prof{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {noreply, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = #conn_prof{}
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#conn_prof{}) -> ok).
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
%%      State = #conn_prof{}
%%      Extra = any()
%%      NewState = #conn_prof{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#conn_prof{},
        Extra::any()) -> {ok, NewState::#conn_prof{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

