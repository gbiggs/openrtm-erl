%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Log event handler that publishes to the terminal.
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
-module(terminal_logger).
-behaviour(gen_event).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").


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
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Logger initialisation function. Creates a terminal logger with the
%% specified level threshold: all events at or below this level will be
%% published.
%% @spec init(Level) -> {ok, Level}
%% where
%%      Level = log_level()
%% @end
-spec(init(Level::log_level()) -> {ok, log_level()}).
%%-----------------------------------------------------------------------------
init(Level) ->
    ?LOG(rtl_info, "Initialising terminal logger."),
    % There is no initialisation for this logger.
    {ok, Level}.


%%-----------------------------------------------------------------------------
%% @doc Handle an event to be logged.
%% @spec handle_event(Event, State) -> {ok, NewState}
%% where
%%      Event = {Level, {Module, Line, Pid}, Data}
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      Data = any()
%%      State = log_level()
%%      NewState = log_level()
%% @end
-spec(handle_event(Event::{Level::log_level(),
            {Module::atom(), Line::pos_integer(), Pid::pid()}, Data::any()},
        StateLevel::log_level()) ->
    {ok, NewState::log_level()}).
%%-----------------------------------------------------------------------------
handle_event({Level, ModLine, Data}, StateLevel) ->
    case event_fmt:lvl_in_threshold(Level, StateLevel)
        of true ->
            io:format("~s~n", [event_fmt:format(Level, ModLine, Data)])
         ; false ->
            ok
    end,
    {ok, StateLevel}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, State) -> {ok, NewState}
%% where
%%      Request = any()
%%      State = log_level()
%%      NewState = log_level()
%% @end
-spec(handle_call(Request::any(), State::log_level()) ->
    {ok, NewState::log_level()}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = any()
%%      State = log_level()
%%      NewState = log_level()
%% @end
-spec(handle_info(Info::any(), State::log_level()) ->
    {ok, NewState::log_level()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Logger shutdown function.
%% @spec terminate(Arg, State) -> any()
%% where
%%      Arg = any() | {stop, Reason} | stop | remove_handler |
%%          {error, {'EXIT', Reason}} | {error, any()}
%%      Reason = any()
%%      State = any()
%% @end
-spec(terminate(Args::any() | {stop, Reason::any()} | stop | remove_handler |
        {error, {'EXIT', Reason::any()}} | {error, any()}, State::any()) ->
    any()).
%%-----------------------------------------------------------------------------
terminate(remove_handler, Level) ->
    % Write the removal to the terminal
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Terminal logger removed."}, Level),
    ok;
terminate({stop, Reason}, Level) ->
    % Write the stop reason to the terminal
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Terminal logger stopped unexpectedly."}, Level),
    handle_event({rtl_warn, {?MODULE, ?LINE}, Reason}, Level),
    ok;
terminate(stop, Level) ->
    % Write the clean shutdown message to the terminal
    handle_event({rtl_info, {?MODULE, ?LINE, self()},
            "Shutting down terminal logger."}, Level),
    ok;
terminate({error, {'EXIT', Reason}}, Level) ->
    % Write the exit error message to the terminal
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Terminal logger stopping due to exit:"}, Level),
    handle_event({rtl_error, {?MODULE, ?LINE}, Reason}, Level),
    ok;
terminate({error, Reason}, Level) ->
    % Write the critical error message to the terminal
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Critical error in terminal logger:"}, Level),
    handle_event({rtl_error, {?MODULE, ?LINE}, Reason}, Level),
    ok;
terminate(Arg, Level) ->
    % Write the unknown shutdown message to the terminal
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Unknown terminal logger shutdown reason:"}, Level),
    handle_event({rtl_error, {?MODULE, ?LINE}, Arg}, Level),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = log_level()
%%      Extra = any()
%%      NewState = log_level()
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::log_level(),
        Extra::any()) -> {ok, NewState::log_level()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test initialising the handler.
%% @spec init_test() -> ok
%% @end
-spec(init_test() -> ok).
%%-----------------------------------------------------------------------------
init_test() ->
    ?assertMatch({ok, rtl_info}, init(rtl_info)).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event below the threshold.
%% @spec handle_event_below_test() -> ok
%% @end
-spec(handle_event_below_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_below_test() ->
    ?assertMatch({ok, rtl_info},
        handle_event({rtl_error, {?MODULE, ?LINE, self()}, "Error"},
            rtl_info)).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event at the threshold.
%% @spec handle_event_at_test() -> ok
%% @end
-spec(handle_event_at_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_at_test() ->
    ?assertMatch({ok, rtl_info},
        handle_event({rtl_info, {?MODULE, ?LINE, self()}, "Info"}, rtl_info)).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event above the threshold.
%% @spec handle_event_above_test() -> ok
%% @end
-spec(handle_event_above_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_above_test() ->
    ?assertMatch({ok, rtl_info},
        handle_event({rtl_paranoid, {?MODULE, ?LINE, self()}, "Paranoia!"},
            rtl_info)).

-endif. % TEST

