%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Supervisor for a single port.
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
-module(port_sup).
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
%% @doc Start a port supervisor.
%% @spec start_link(BufOpts, WorkerOpts) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      Name = string()
%%      Owner = pid() | nil
%%      WorkerCfg = config()
%% @end
-spec(start_link(BufOpts::{BufMod::atom(), BufCfg::config()},
        WorkerOpts::{WorkerMod::atom(), Name::string, Owner::pid() | nil,
            WorkerCfg::config()}) ->
    {ok, pid()} | ignore |
        {error, {already_started, pid()} | shutdown | any()}).
%%-----------------------------------------------------------------------------
start_link(BufOpts, WorkerOpts) ->
    supervisor:start_link(port_sup, {BufOpts, WorkerOpts}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init({BufOpts, WorkerOpts}) -> Result
%% where
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      Name = string()
%%      Owner = pid() | nil
%%      WorkerCfg = config()
%% @end
-spec(init({BufOpts::{BufMod::atom(), BufCfg::config()},
        WorkerOpts::{WorkerMod::atom(), Name::string, Owner::pid() | nil,
            WorkerCfg::config()}}) ->
    sup_init_res()).
%%-----------------------------------------------------------------------------
init({BufOpts, WorkerOpts}) ->
    {ok, {{one_for_all, 0, 1}, child_spec(BufOpts, WorkerOpts)}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec(BufOpts, WorkerOpts) -> ChildSpec
%% where
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      Name = string()
%%      Owner = pid() | nil
%%      WorkerCfg = config()
%% @end
-spec(child_spec(BufOpts::{BufMod::atom(), BufCfg::config()},
        WorkerOpts::{WorkerMod::atom(), Name::string, Owner::pid() | nil,
            WorkerCfg::config()}) ->
    [child_spec()]).
%%-----------------------------------------------------------------------------
child_spec(BufOpts, WorkerOpts) ->
    [{conns_sup, {conns_sup, start_link, []}, permanent, infinity, supervisor,
        dynamic},
        {corba, {corba_obj_mgr, start_link,
                [fun 'RTC_PortService_impl':start_link/1, none]}, permanent,
            30000, worker, [corba_obj_mgr, 'RTC_PortService_impl']},
        buf_spec(BufOpts),
        worker_spec(WorkerOpts)
    ].


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for the buffer.
%% @spec buf_spec(BufOpts) -> ChildSpec
%% where
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(buf_spec(BufOpts::{BufMod::atom(), BufCfg::config()}) ->
    [child_spec()]).
%%-----------------------------------------------------------------------------
buf_spec({BufMod, BufCfg}) ->
    {buffer, {buffer_svr, start_link, [BufMod, BufCfg]}, permanent, 30000,
        worker, [buffer_svr, BufMod]}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for the worker.
%% @spec worker_spec(BufOpts) -> ChildSpec
%% where
%%      WorkerOpts = {WorkerMod, Name, Owner, WorkerCfg}
%%      WorkerMod = atom()
%%      Name = string()
%%      Owner = pid() | nil
%%      WorkerCfg = config()
%% @end
-spec(worker_spec(WorkerOpts::{WorkerMod::atom(), Name::string(),
        Owner::pid() | nil, WorkerCfg::config()}) ->
    [child_spec()]).
%%-----------------------------------------------------------------------------
worker_spec({WorkerMod, Name, Owner, WorkerCfg}) ->
    {worker, {WorkerMod, start_link, [Name, Owner, WorkerCfg]}, permanent, 30000,
        worker, [WorkerMod]}.


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
        supervisor:check_childspecs(child_spec({bufmod, []},
                {workermod, "name", self(), []}))).

-endif. % TEST

