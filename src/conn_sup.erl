%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Supervisor for a single connection.
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
-module(conn_sup).
-behaviour(supervisor).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([start_link/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start a connection supervisor.
%% @spec start_link(WorkerMod, BufOpts) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      WorkerOpts = {WorkerMod, WorkerCfg}
%%      WorkerMod = atom()
%%      WorkerCfg = config()
%%      BufOpts = nil | {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(start_link(WorkerOpts::{WorkerMod::atom(), WorkerCfg::config()},
        BufOpts::nil | {BufMod::atom(), BufCfg::config()}) ->
    {ok, pid()} | ignore |
        {error, {already_started, pid()} | shutdown | any()}).
%%-----------------------------------------------------------------------------
start_link(WorkerOpts, BufOpts) ->
    supervisor:start_link(conn_sup, {WorkerOpts, BufOpts}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init({BufOpts, WorkerOpts}) -> Result
%% where
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      WorkerCfg = config()
%%      BufOpts = nil | {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(init({WorkerOpts::{WorkerMod::atom(), WorkerCfg::config()},
            BufOpts::nil | {BufMod::atom(), BufCfg::config()}}) ->
    sup_init_res()).
%%-----------------------------------------------------------------------------
init({WorkerOpts, BufOpts}) ->
    {ok, {{one_for_all, 0, 1}, child_spec(WorkerOpts, BufOpts)}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec(WorkerOpts, BufOpts) -> ChildSpec
%% where
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      WorkerCfg = config()
%%      BufOpts = nil | {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(child_spec(WorkerOpts::{WorkerMod::atom(), WorkerCfg::config()},
            BufOpts::nil | {BufMod::atom(), BufCfg::config()}) ->
    child_spec()).
%%-----------------------------------------------------------------------------
child_spec(WorkerOpts, nil) ->
    [worker_spec(WorkerOpts), corba_spec()];
child_spec(WorkerOpts, BufOpts) ->
    [worker_spec(WorkerOpts), buf_spec(BufOpts), corba_spec()].


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for the buffer.
%% @spec buf_spec(BufOpts) -> ChildSpec
%% where
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(buf_spec(BufOpts::{BufMod::atom(), BufCfg::config()}) ->
    child_spec()).
%%-----------------------------------------------------------------------------
buf_spec({BufMod, BufCfg}) ->
    {buffer, {buffer_svr, start_link, [BufMod, BufCfg]}, permanent, 30000,
        worker, [buffer_svr, BufMod]}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for the worker.
%% @spec worker_spec(BufOpts) -> ChildSpec
%% where
%%      WorkerOpts = {WorkerMod, WorkerCfg}
%%      WorkerMod = atom()
%%      WorkerCfg = config()
%% @end
-spec(worker_spec(WorkerOpts::{WorkerMod::atom(), WorkerCfg::config()}) ->
    child_spec()).
%%-----------------------------------------------------------------------------
worker_spec({WorkerMod, WorkerCfg}) ->
    {worker, {WorkerMod, start_link, [WorkerCfg]}, permanent, 30000, worker,
        [WorkerMod]}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for the CORBA service.
%% @spec corba_spec() -> ChildSpec
%% @end
-spec(corba_spec() -> child_spec()).
%%-----------------------------------------------------------------------------
corba_spec() ->
    {corba, {corba_obj_mgr, start_link,
            [fun 'OpenRTM_InPortCdr_impl':start_link/1, none]}, permanent,
        30000, worker, [corba_obj_mgr, 'OpenRTM_InPortCdr_impl']}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test the child_spec is syntactically correct.
%% @spec child_spec_test() -> ok
%% @end
-spec(child_spec_test() -> ok).
%%-----------------------------------------------------------------------------
child_spec_test() ->
    ?assertEqual(ok,
        supervisor:check_childspecs(child_spec({workermod, []}, nil))),
    ?assertEqual(ok,
        supervisor:check_childspecs(child_spec({workermod, []},
            {bufmod, []}))).

-endif. % TEST

