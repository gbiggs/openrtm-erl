%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Main manager worker. Responsible for providing the implementatio of
%% manager functionality such as creating RTCs. Implemented using gen_server.
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
-module(mgr).
-behaviour(gen_server).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("mgr.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/0, stop/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the manager.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mgr}, mgr, [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the manager.
%% @spec stop() -> ok
%% @end
-spec(stop() -> ok).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:cast(mgr, stop).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the manager. Will perform the manager start-up routine. This
%% includes checking the configuration server for components to pre-create and
%% making them.
%% @spec init([]) -> {ok, State} | {stop, Reason}
%% where
%%      State = #mgr{}
%%      Reason = any()
%% @end
-spec(init([]) -> {ok, #mgr{}} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init([]) ->
    ?LOG(rtl_info, "Initialising manager worker."),
    process_flag(trap_exit, true),
    start_orber(),
    Mgr = precreate_rtcs(#mgr{pid=self()}),
    {ok, Mgr}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = #mgr{}
%%      Reply = any()
%%      NewState = #mgr{}
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()},
        State::#mgr{}) ->
    {reply, Reply::any(), NewState::#mgr{}}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, _From, State) ->
    {reply, not_supported, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = #mgr{}
%%      NewState = #mgr{}
%% @end
-spec(handle_cast(Request::any(), State::#mgr{}) ->
    {stop, normal, NewState::#mgr{}}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = timeout | any()
%%      State = #mgr{}
%%      NewState = #mgr{}
%% @end
-spec(handle_info(Info::timeout | any(), State::#mgr{}) ->
    {ok, NewState::#mgr{}}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = []
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::#mgr{}) -> ok).
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
%%      State = #mgr{}
%%      Extra = any()
%%      NewState = #mgr{}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::#mgr{},
        Extra::any()) -> {ok, NewState::#mgr{}}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Start the Orber application.
%% @spec start_orber() -> ok
%% @end
-spec(start_orber() -> ok).
%%-----------------------------------------------------------------------------
start_orber() ->
    case config_svr:get_value(["naming", "enable"])
        of true ->
            naming:start_orber()
         ; false ->
            ?LOG(rtl_info, "CORBA interfaces are disabled."),
            ok
    end.


%%-----------------------------------------------------------------------------
%% @doc Start components listed in the configuration as created on startup.
%% @spec precreate_rtcs(State) -> NewState
%% where
%%      State = #mgr{}
%%      NewState = #mgr{}
%% @end
-spec(precreate_rtcs(State::#mgr{}) -> NewState::#mgr{}).
%%-----------------------------------------------------------------------------
precreate_rtcs(State) ->
    State#mgr{}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test pre-creating an RTC.
%% @spec precreate_rtcs_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(precreate_rtcs_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
precreate_rtcs_test_() ->
    {setup,
        fun() -> start_mock_config_svr([]) end,
        fun(_) -> stop_mock_config_svr(), timer:sleep(100) end,
        fun(_) ->
            [?_assert(mgrs_equal(#mgr{pid=self()},
                        precreate_rtcs(#mgr{pid=self()})))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Start a configuration server to provide data during the unit tests.
%% @spec start_mock_config_svr(Defaults) -> {ok, pid()}
%% where
%%      Defaults = config()
%% @end
-spec(start_mock_config_svr(Defaults::config()) -> {ok, pid()}).
%%-----------------------------------------------------------------------------
start_mock_config_svr(Defaults) ->
    {ok, _} = config_svr:start_link("", Defaults).


%%-----------------------------------------------------------------------------
%% @doc Stop the configuration server.
%% @spec stop_mock_config_svr() -> ok
%% @end
-spec(stop_mock_config_svr() -> ok).
%%-----------------------------------------------------------------------------
stop_mock_config_svr() ->
    config_svr:stop().


%%-----------------------------------------------------------------------------
%% @doc Check if two manager records are (mostly) equal. The pid values are
%% not checked.
%% @spec mgrs_equal(Mgr1, Mgr2) -> boolean()
%% where
%%      Mgr1 = #mgr{}
%%      Mgr2 = #mgr{}
%% @end
-spec(mgrs_equal(Mgr1::#mgr{}, Mgr2::#mgr{}) -> boolean()).
%%-----------------------------------------------------------------------------
mgrs_equal(#mgr{mgrs=Mgrs}, #mgr{mgrs=Mgrs}) ->
    true;
mgrs_equal(_, _) ->
    false.


-endif. % TEST

