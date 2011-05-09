%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Supervisor for a single execution context.
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
-module(ec_sup).
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
-export([start_link/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start an EC supervisor.
%% @spec start_link(Config) -> {ok, pid()} | ignore | {error, Reason}
%% where
%%      Config = config()
%%          EC configuration.
%%      Reason = {already_started, pid()} | shutdown | any()
%% @end
-spec(start_link(Config::config()) -> {ok, pid()} | ignore |
        {error, {already_started, pid()} | shutdown | any()}).
%%-----------------------------------------------------------------------------
start_link(Config) ->
    supervisor:start_link(ec_sup, Config).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init(Config) -> Result
%% where
%%      Config = config()
%%          EC configuration.
%% @end
-spec(init(Config::config()) -> sup_init_res()).
%%-----------------------------------------------------------------------------
init(Config) ->
    {ok, {{rest_for_one, 1, 60}, child_spec(Config)}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec(Config) -> ChildSpec
%% where
%%      Config = config()
%% @end
-spec(child_spec(Config::config()) -> [child_spec()]).
%%-----------------------------------------------------------------------------
child_spec(Config) ->
    [{fsm, {periodic_ec, start_link_fsm, [Config]}, permanent, 30000, worker,
            [periodic_ec, ec_part]},
        {corba,
            {corba_obj_mgr, start_link,
                [fun 'RTC_ExecutionContextService_impl':start_link/1, []]},
            transient, 30000, worker, ['RTC_ExecutionContextService_impl']}
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
    ?assertEqual(ok, supervisor:check_childspecs(child_spec([]))).

-endif. % TEST

