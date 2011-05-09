%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Top-level supervisor for the RTC Manager.
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
-module(mgr_sup).
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
-export([]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1]).


%%=============================================================================
%% External functions
%%=============================================================================


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Supervisor initialisation callback. See the supervisor documentation
%% for more details.
%% @spec init(Args) -> Result
%% @end
-spec(init(any()) -> sup_init_res()).
%%-----------------------------------------------------------------------------
init(_Args) ->
    {ok, {{rest_for_one, 1, 60}, child_spec()}}.


%%-----------------------------------------------------------------------------
%% @doc Defines the child_spec for this supervisor.
%% @spec child_spec() -> ChildSpec
%% @end
-spec(child_spec() -> [child_spec()]).
%%-----------------------------------------------------------------------------
child_spec() ->
    [{config_svr, {config_svr, start_link,
                ["rtc.conf", defaults:defaults()]}, permanent, 30000, worker,
            [config_svr]},
        {log_mgr, {log_mgr, start_link_with_outputs, []}, permanent, 30000,
            worker, dynamic},
        {mod_svr, {mod_svr, start_link, []}, permanent, 30000,
            worker, [mod_svr]},
        {rtcs_sup, {rtcs_sup, start_link, []}, permanent, infinity,
            supervisor, dynamic},
        {mgr, {mgr, start_link, []}, permanent, 30000, worker, [mgr]},
        {mgr_corba, {'RTM_Manager_impl', start_link, []}, transient, 30000,
            worker, ['RTM_Manager_impl']}
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
    ?assertEqual(ok, supervisor:check_childspecs(child_spec())).

-endif. % TEST

