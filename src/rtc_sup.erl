%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Supervisor for a single RT Component in a manager.
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
-module(rtc_sup).
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
-export([start_link/3]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start an RTC supervisor.
%% @spec start_link(InstName, Config, Module) ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      InstName = string()
%%      Config = config()
%%      Module = atom()
%%      Error = {already_started, pid()} | shutdown | any()
%% @end
-spec(start_link(InstName::string(), Config::config(), Module::atom()) ->
        {ok, pid()} | ignore |
        {error, {already_started, pid()} | shutdown | any()}).
%%-----------------------------------------------------------------------------
start_link(InstName, Config, Module) ->
    supervisor:start_link(rtc_sup, {InstName, Config, Module}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init({RegName, Config, Module}) -> Result
%% where
%%      RegName = string()
%%      Config = config()
%%      Module = atom()
%% @end
-spec(init({RegName::string(), Config::config(), Module::atom()}) ->
    sup_init_res()).
%%-----------------------------------------------------------------------------
init({RegName, Config, Module}) ->
    {ok, {{rest_for_one, 1, 60}, child_spec(RegName, Config, Module)}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec(RegName, Config, Module) -> ChildSpec
%% where
%%      RegName = string()
%%      Config = config()
%%      Module = atom()
%% @end
-spec(child_spec(RegName::string(), Config::config(), Module::atom()) ->
    [child_spec()]).
%%-----------------------------------------------------------------------------
child_spec(RegName, Config, Module) ->
    [{config_svr, {rtc_cfg_svr, start_link, [Config]}, permanent, 30000,
            worker, [rtc_cfg_svr]},
        {fsm, {dataflow_rtc, start_link_fsm, [Config, Module]},
            permanent, 30000, worker, [dataflow_rtc]},
        {port_mgr, {port_mgr, start_link, []}, permanent,
            30000, worker, [port_mgr]},
        {cfg_corba, {corba_obj_mgr, start_link,
                [fun 'SDOPackage_Configuration_impl':start_link/1, none]},
            transient, 30000, worker, ['SDOPackage_Configuration_impl']},
        {corba, {corba_obj_mgr, start_link,
                [fun 'OpenRTM_DataFlowComponent_impl':start_link/1,
                    {RegName, Config}]},
            transient, 30000, worker, ['OpenRTM_DataFlowComponent_impl']},
        {ports_sup, {ports_sup, start_link, []}, permanent, infinity,
            supervisor, dynamic},
        {ecs_sup, {ecs_sup, start_link, []}, permanent, infinity,
            supervisor, dynamic}
    ].


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
        supervisor:check_childspecs(child_spec("Name", [], none))).

-endif. % TEST

