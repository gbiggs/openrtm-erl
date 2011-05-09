%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Process to manage a CORBA interface process.
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
-module(corba_obj_mgr).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Server control
-export([start_link/2, stop_server/1, send_info/2, get_obj/1]).


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
%% @spec start_link(InitFunc, Args) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      InitFunc = fun()
%%          The initialisation function of the CORBA implementation.
%%      Args = any()
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link(InitFunc::fun(), Args::any()) ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link(InitFunc, Args) ->
    gen_server:start_link(corba_obj_mgr, {InitFunc, Args}, []).


%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @spec stop_server(Svr) -> ok
%% where
%%      Svr = pid()
%%          The PID of the process running the interface.
%% @end
-spec(stop_server(Svr::pid()) -> ok).
%%-----------------------------------------------------------------------------
stop_server(Svr) ->
    ?LOG(rtl_paranoid, "CORBA object manager asked to stop."),
    gen_server:cast(Svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Send an info message to the CORBA process being managed.
%% @spec send_info(Svr, Msg) -> ok
%% where
%%      Svr = pid()
%%          The PID of the process running the interface.
%%      Msg = any()
%%      Obj = object_ref()
%% @end
-spec(send_info(Svr::pid(), Msg::any()) -> ok).
%%-----------------------------------------------------------------------------
send_info(Svr, Msg) ->
    gen_server:cast(Svr, {send_info, Msg}).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object being managed.
%% @spec get_obj(Svr) -> Obj
%% where
%%      Svr = pid()
%%          The PID of the CORBA object manager.
%%      Obj = object_ref()
%% @end
-spec(get_obj(Svr::pid()) -> Obj::object_ref()).
%%-----------------------------------------------------------------------------
get_obj(Svr) ->
    gen_server:call(Svr, get_obj).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the server.
%% @spec init({InitFunc, Args}) -> {ok, State} | {stop, Reason}
%% where
%%      InitFunc = fun()
%%          The initialisation function of the CORBA implementation.
%%      Args = any()
%%      State = {Pid, Obj}
%%      Pid = pid()
%%          The PID of the CORBA implementation process.
%%      Obj = object_ref()
%%          The CORBA object reference.
%%      Reason = any()
%% @end
-spec(init({InitFunc::fun(), Args::any()}) ->
    {ok, {Pid::pid(), Obj::object_ref()}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init({InitFunc, Args}) ->
    ?LOG(rtl_debug, "CORBA object manager initialising: ~p, ~p",
        [InitFunc, Args]),
    {ok, Pid, Obj} = InitFunc(Args),
    {ok, {Pid, Obj}}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}
%% where
%%      Request = get_obj
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = {Pid, Obj}
%%      Pid = pid()
%%      Obj = object_ref()
%%      Reply = object_ref()
%% @end
-spec(handle_call(Request::get_obj, From::{pid(), Tag::any()},
        State::{Pid::pid(), Obj::object_ref()}) ->
    {reply, Reply::object_ref(), State::{Pid::pid(), Obj::object_ref()}}).
%%-----------------------------------------------------------------------------
handle_call(get_obj, _From, {_, Obj}=State) ->
    {reply, Obj, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) ->
%%      {noreply, State} | {stop, normal, State}
%% where
%%      Request = {send_info, Msg} | stop
%%      Msg = any()
%%      State = {Pid, Obj}
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(handle_cast(Request::stop, State::{Pid::pid(), Obj::object_ref()}) ->
    {noreply, State::{Pid::pid(), Obj::object_ref()}} |
    {stop, normal, State::{Pid::pid(), Obj::object_ref()}}).
%%-----------------------------------------------------------------------------
handle_cast({send_info, Msg}, {Pid, _}=State) ->
    Pid ! Msg,
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, State}
%% where
%%      Info = not_supported
%%      State = {Pid, Obj}
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(handle_info(Info::not_supported,
        State::{Pid::pid(), Obj::object_ref()}) ->
    {ok, State::{Pid::pid(), Obj::object_ref()}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = {Pid, Obj}
%%      Pid = pid()
%%      Obj = object_ref()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::{Pid::pid(), Obj::object_ref()}) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, {Pid, _}) ->
    ?LOG(rtl_info, "Shutting down normally."),
    gen_server:cast(Pid, stop);
terminate(shutdown, {Pid, _}) ->
    ?LOG(rtl_info, "Shut down by supervisor."),
    gen_server:cast(Pid, stop);
terminate({shutdown, Reason}, {Pid, _}) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]),
    gen_server:cast(Pid, stop);
terminate(Reason, {Pid, _}) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]),
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = {Pid, Obj}
%%      Pid = pid()
%%      Obj = object_ref()
%%      Extra = any()
%%      NewState = {Pid, Obj}
%% @end
-spec(code_change(OldVsn::any() | {down, any()},
        State::{Pid::pid(), Obj::object_ref()}, Extra::any()) ->
    {ok, NewState::{Pid::pid(), Obj::object_ref()}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test starting and stopping the server.
%% @spec start_stop_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(start_stop_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
start_stop_test_() ->
    ?LET({ok, Pid}, start_link(fun dummy_corba/1, []),
        [?_assertMatch(true, is_process_alive(Pid)),
            ?_test(stop_server(Pid)),
            ?_test(timer:sleep(100)),
            ?_assertMatch(false, is_process_alive(Pid))]).


%%-----------------------------------------------------------------------------
%% @doc Test getting the object from the server.
%% @spec get_obj_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(get_obj_test_() ->
        {setup, fun(() -> {any(), pid()}),
            fun((any()) -> ok | {error, atom()}),
            fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
get_obj_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = start_link(fun dummy_corba/1, []),
            Pid
        end,
        fun(Pid) ->
            stop_server(Pid)
        end,
        fun(Pid) ->
                [?_assertMatch(obj, get_obj(Pid))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Dummy CORBA implementation function for tests.
%% @spec dummy_corba(any()) -> {ok, pid, obj}
%% @end
-spec(dummy_corba(Args::any()) -> {ok, pid, obj}).
%%-----------------------------------------------------------------------------
dummy_corba(_) ->
    {ok, pid, obj}.

-endif. % TEST

