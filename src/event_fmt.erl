%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Functions for formatting events to be logged.
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
-module(event_fmt).


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
-export([format/3, lvl_in_threshold/2]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Formats an event for printing.
%% @spec format(Level, {Module, Line, Pid}, Data) -> string()
%% where
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      Data = any()
%% @end
-spec(format(Level::log_level(),
        {Module::atom(), Line::pos_integer(), Pid::pid()}, Data::any()) ->
    string()).
%%-----------------------------------------------------------------------------
format(Level, ModLine, Data) ->
    io_lib:format("~s ~s", [timestamp(), format_event(Level, ModLine, Data)]).


%%-----------------------------------------------------------------------------
%% @doc Checks if a given log level is within the given threshold.
%% @spec lvl_in_threshold(CheckLevel, Threshold) -> true | false
%% where
%%      CheckLevel = log_level()
%%      Threshold = log_level()
%% @end
-spec(lvl_in_threshold(L::log_level(), T::log_level()) -> true | false).
%%-----------------------------------------------------------------------------
lvl_in_threshold(L, T) ->
    true = lists:member(L, threshold_members(rtl_paranoid)),
    lists:member(L, threshold_members(T)).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Creates a text timestamp.
%% @spec timestamp() -> string()
%% @end
-spec(timestamp() -> string()).
%%-----------------------------------------------------------------------------
timestamp() ->
    {Y, M, D} = date(),
    {H, Min, S} = time(),
    io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Y, M, D, H, Min, S]).


%%-----------------------------------------------------------------------------
%% @doc Pretty-print event information.
%% @spec format_event(Level, {Module, Line, Pid}, Data) -> string()
%% where
%%      Level = log_level()
%%      Module = atom()
%%      Line = pos_integer()
%%      Pid = pid()
%%      Data = any()
%% @end
-spec(format_event(Level::log_level(),
        {Module::atom(), Line::pos_integer(), Pid::pid()}, Data::any()) ->
    string()).
%%-----------------------------------------------------------------------------
format_event(Level, {Module, Line, Pid}, Data) ->
    io_lib:format("~s:~w:~p (~s): ~s", [Module, Line, Pid, Level, Data]).


%%-----------------------------------------------------------------------------
%% @doc The members of each log level.
%% @spec threshold_members(T) -> [log_level()]
%% where
%%      T = log_level()
%% @end
-spec(threshold_members(T::log_level()) -> [log_level()]).
%%-----------------------------------------------------------------------------
threshold_members(rtl_silent) ->
    [rtl_silent];
threshold_members(rtl_fatal) ->
    [rtl_silent, rtl_fatal];
threshold_members(rtl_error) ->
    [rtl_silent, rtl_fatal, rtl_error];
threshold_members(rtl_warn) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn];
threshold_members(rtl_info) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn, rtl_info];
threshold_members(rtl_debug) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn, rtl_info, rtl_debug];
threshold_members(rtl_trace) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn, rtl_info, rtl_debug,
        rtl_trace];
threshold_members(rtl_verbose) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn, rtl_info, rtl_debug,
        rtl_trace, rtl_verbose];
threshold_members(rtl_paranoid) ->
    [rtl_silent, rtl_fatal, rtl_error, rtl_warn, rtl_info, rtl_debug,
        rtl_trace, rtl_verbose, rtl_paranoid].


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test formatting events.
%% @spec format_test() -> ok
%% @end
-spec(format_test() -> ok).
%%-----------------------------------------------------------------------------
format_test() ->
    ?assertMatch("event_fmt:1:me (rtl_paranoid): blurglesplurt",
        lists:flatten(format_event(rtl_paranoid, {?MODULE, 1, me},
                "blurglesplurt"))).


%%-----------------------------------------------------------------------------
%% @doc Test log level thresholding.
%% @spec lvl_in_threshold_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(lvl_in_threshold_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
lvl_in_threshold_test_() ->
    [?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_fatal, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_error, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_warn, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_info, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_debug, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_silent)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_silent)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_fatal)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_error, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_warn, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_info, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_debug, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_fatal)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_fatal)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_error)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_error)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_warn, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_info, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_debug, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_error)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_error)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_warn)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_warn)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_warn)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_warn)),
    ?_assertMatch(false, lvl_in_threshold(rtl_info, rtl_warn)),
    ?_assertMatch(false, lvl_in_threshold(rtl_debug, rtl_warn)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_warn)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_warn)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_warn)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_info)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_info)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_info)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_info)),
    ?_assertMatch(true, lvl_in_threshold(rtl_info, rtl_info)),
    ?_assertMatch(false, lvl_in_threshold(rtl_debug, rtl_info)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_info)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_info)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_info)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_debug)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_debug)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_debug)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_debug)),
    ?_assertMatch(true, lvl_in_threshold(rtl_info, rtl_debug)),
    ?_assertMatch(true, lvl_in_threshold(rtl_debug, rtl_debug)),
    ?_assertMatch(false, lvl_in_threshold(rtl_trace, rtl_debug)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_debug)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_debug)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_info, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_debug, rtl_trace)),
    ?_assertMatch(true, lvl_in_threshold(rtl_trace, rtl_trace)),
    ?_assertMatch(false, lvl_in_threshold(rtl_verbose, rtl_trace)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_trace)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_info, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_debug, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_trace, rtl_verbose)),
    ?_assertMatch(true, lvl_in_threshold(rtl_verbose, rtl_verbose)),
    ?_assertMatch(false, lvl_in_threshold(rtl_paranoid, rtl_verbose)),

    ?_assertMatch(true, lvl_in_threshold(rtl_silent, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_fatal, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_error, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_warn, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_info, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_debug, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_trace, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_verbose, rtl_paranoid)),
    ?_assertMatch(true, lvl_in_threshold(rtl_paranoid, rtl_paranoid))].

-endif. % TEST

