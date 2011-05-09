%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Log event handler that publishes to a file.
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
-module(file_logger).
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
%% @doc Logger initialisation function. Creates a file logger with the
%% specified level threshold: all events at or below this level will be
%% published.
%% @spec init({string(), Level}) -> {ok, State}
%% where
%%      Level = log_level()
%%      State = {log_level(), Fd}
%%      Fd = pid()
%% @end
-spec(init({File::string(), Level::log_level()}) ->
        {ok, State::{log_level(), Fd::pid()}}).
%%-----------------------------------------------------------------------------
init({File, Level}) ->
    ?LOG(rtl_info, "Initialising file logger; logging to ~s.", [File]),
    case file:open(File, [write])
        of {ok, Fd} ->
            {ok, {Level, Fd}}
         ; {error, Reason} ->
            {error, Reason}
    end.


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
%%      State = {log_level(), Fd}
%%      NewState = {log_level(), Fd}
%%      Fd = pid()
%% @end
-spec(handle_event(Event::{Level::log_level(),
            {Module::atom(), Line::pos_integer(), Pid::pid()}, Data::any()},
        State::{log_level(), Fd::pid()}) ->
    {ok, NewState::{log_level(), Fd::pid()}}).
%%-----------------------------------------------------------------------------
handle_event({Level, ModLine, Data}, {StateLevel, Fd}=S) ->
    case event_fmt:lvl_in_threshold(Level, StateLevel)
        of true ->
            io:format(Fd, "~s~n", [event_fmt:format(Level, ModLine, Data)])
         ; false ->
            ok
    end,
    {ok, S}.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, State) -> {ok, NewState}
%% where
%%      Request = any()
%%      State = {log_level(), Fd}
%%      NewState = {log_level(), Fd}
%%      Fd = pid()
%% @end
-spec(handle_call(Request::any(), State::{log_level(), Fd::pid()}) ->
    {ok, NewState::{log_level(), pid()}}).
%%-----------------------------------------------------------------------------
handle_call(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = any()
%%      State = log_level()
%%      NewState = {log_level(), pid()}
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
terminate(remove_handler, {_Level, Fd}=S) ->
    handle_event({rtl_info, {?MODULE, ?LINE, self()},
            "File logger removed."}, S),
    ok = file:close(Fd);
terminate({stop, Reason}, {_Level, Fd}=S) ->
    % Write the stop reason to the file
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Unexpected stoppage of file logger."}, S),
    handle_event({rtl_warn, {?MODULE, ?LINE, self()}, Reason}, S),
    ok = file:close(Fd);
terminate(stop, {_Level, Fd}=S) ->
    % Write the clean shutdown message to the file
    handle_event({rtl_info, {?MODULE, ?LINE, self()},
            "Shutting down file logger."}, S),
    ok = file:close(Fd);
terminate({error, {'EXIT', Reason}}, {_Level, Fd}=S) ->
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "File logger shut down due to exit:"}, S),
    handle_event({rtl_error, {?MODULE, ?LINE, self()}, Reason}, S),
    ok = file:close(Fd);
terminate({error, Reason}, {_Level, Fd}=S) ->
    % Write the critical file_logger error message to the file
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Critical error in file logger."}, S),
    handle_event({rtl_error, {?MODULE, ?LINE, self()}, Reason}, S),
    ok = file:close(Fd);
terminate(Arg, {_Level, Fd}=S) ->
    % Write the unknown shutdown message to the file
    handle_event({rtl_error, {?MODULE, ?LINE, self()},
            "Unknown file logger shutdown reason"}, S),
    handle_event({rtl_error, {?MODULE, ?LINE, self()}, Arg}, S),
    ok = file:close(Fd).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = log_level()
%%      Extra = any()
%%      NewState = {log_level(), pid()}
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::log_level(),
    Extra::any()) -> {ok, NewState::{log_level(), pid()}}).
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
    File = "file_logger_unittest_init.txt",
    {ok, S} = init({File, rtl_info}),
    terminate(stop, S),
    ?assertMatch({ok, _}, file:read_file_info(File)),
    file:delete(File).


%%-----------------------------------------------------------------------------
%% @doc Test terminating the handler.
%% @spec terminate_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(terminate_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
terminate_test_() ->
    {setup,
        fun() ->
            {ok, S} = init({"file_logger_eunit_term.txt", rtl_info}),
            S
        end,
        fun(_) -> file:delete("file_logger_eunit_term.txt") end,
        fun(S) -> ?_assertMatch(ok, terminate(stop, S)) end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test handling an event below the threshold.
%% @spec handle_event_below_test() -> ok
%% @end
-spec(handle_event_below_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_below_test() ->
    File = "file_logger_eunit_he.txt",
    {ok, S} = init({File, rtl_info}),
    {ok, S} = handle_event({rtl_error, {?MODULE, ?LINE, self()}, "Error"}, S),
    {ok, Fd} = file:open(File, [read]),
    {ok, FLine} = file:read_line(Fd),
    ?assertNot(string:str(FLine, "Error") =:= 0),
    file:close(Fd),
    terminate(stop, S),
    file:delete(File).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event at the threshold.
%% @spec handle_event_at_test() -> ok
%% @end
-spec(handle_event_at_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_at_test() ->
    File = "file_logger_eunit_he.txt",
    {ok, S} = init({File, rtl_info}),
    {ok, S} = handle_event({rtl_info, {?MODULE, ?LINE, self()}, "Info"}, S),
    {ok, Fd} = file:open(File, [read]),
    {ok, FLine} = file:read_line(Fd),
    ?assertNot(string:str(FLine, "Info") =:= 0),
    file:close(Fd),
    terminate(stop, S),
    file:delete(File).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event above the threshold.
%% @spec handle_event_above_test() -> ok
%% @end
-spec(handle_event_above_test() -> ok).
%%-----------------------------------------------------------------------------
handle_event_above_test() ->
    File = "file_logger_eunit_he.txt",
    {ok, S} = init({File, rtl_info}),
    {ok, S} = handle_event({rtl_paranoid, {?MODULE, ?LINE, self()},
            "Paranoia!"}, S),
    {ok, Fd} = file:open(File, [read]),
    ?assertMatch(eof, file:read_line(Fd)),
    file:close(Fd),
    terminate(stop, S),
    file:delete(File).


%%-----------------------------------------------------------------------------
%% @doc Test handling an event using a bad level.
%% @spec handle_event_bad_level_test_() -> [{integer(), fun(() -> ok)}]
%% @end
-spec(handle_event_bad_level_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
handle_event_bad_level_test_() ->
    [?_assertError({badmatch, false},
            handle_event({5, {?MODULE, ?LINE, self()}, "E"}, {rtl_info, nil})),
        ?_assertError({badmatch, false},
            handle_event({rtl_random, {?MODULE, ?LINE, self()}, "E"},
                {rtl_info, nil}))].


%%-----------------------------------------------------------------------------
%% @doc Test handling an event using a bad level.
%% @spec handle_event_bad_state_test_() -> ok
%% @end
-spec(handle_event_bad_state_test_() -> [{integer(), fun(() -> ok)}]).
%%-----------------------------------------------------------------------------
handle_event_bad_state_test_() ->
    [?_assertError(function_clause,
            handle_event({rtl_info, {?MODULE, ?LINE, self()}, "E"}, {5, nil})),
        ?_assertError(function_clause, handle_event({rtl_info,
                    {?MODULE, ?LINE, self()}, "E"}, {rtl_random, nil}))].

-endif. % TEST

