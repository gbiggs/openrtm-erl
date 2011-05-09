%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Top-level modules for the OpenRTM-aist-erlang application.
%% @reference <a href="http://www.openrtm.org/">OpenRTM.org</a>
%% @copyright 2010 Geoffrey Biggs,
%% RT-Synthesis Research Group,
%% Intelligent Systems Research Institute,
%% National Institute of Advanced Industrial Science and Technology (AIST),
%% Japan.
%% All rights reserved.
%% Licensed under the Eclipse Public License -v 1.0 (EPL),
%% http://www.opensource.org/licenses/eclipse-1.0.txt
%% @version {@version}
%% @end
%%-----------------------------------------------------------------------------
-module(openrtm_erl).

-behaviour(application).


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
-export([shutdown/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([start/2, stop/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Application start callback: starts the RTC Manager application.
%% @spec start(StartType, StartArgs) ->
%%      {ok, pid()} | {ok, pid(), State} | {error, Reason}
%% where
%%      StartType = normal
%%      StartArgs = any()
%%      State = any()
%%      Reason = any()
%% @end
-spec(start(atom(), any()) ->
        {ok, pid()} | {ok, pid(), any()} | {error, atom()}).
%%-----------------------------------------------------------------------------
start(_Type, _Args) ->
    supervisor:start_link({local, mgr_sup}, mgr_sup, []).


%%-----------------------------------------------------------------------------
%% @doc Call to shut down the RTC Manager application.
%% @spec shutdown() -> ok
%% @end
-spec(shutdown() -> ok).
%%-----------------------------------------------------------------------------
shutdown() ->
    application:stop(openrtm_erl).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Application stop callback: called when the RTC Manager application
%% stops.
%% @private
%% @spec stop(State) -> ok
%% where
%%      State = any()
%% @end
-spec(stop(any()) -> ok).
%%-----------------------------------------------------------------------------
stop(_State) ->
    ok.

