%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Loadable module server tests.
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
-module(mod_svr_tests).


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
%% Imports
%%-----------------------------------------------------------------------------
-import(mod_svr, [init/1]).


%%=============================================================================
%% Internal functions
%%=============================================================================


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with no modules to pre-load.
%% @spec init_no_mods_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_no_mods_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_no_mods_test_() ->
    {setup,
        fun() -> start_mock_config_svr([]) end,
        fun(_) -> stop_mock_config_svr(), timer:sleep(100) end,
        fun(_) ->
            [?_assertMatch({ok, []}, init([]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with one module to pre-load but no path.
%% @spec init_one_mod_no_path_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_one_mod_no_path_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_one_mod_no_path_test_() ->
    {setup,
        fun() ->
            start_mock_config_svr([{"manager",
                        [{"modules", [{"preload", "mod_1"}]}]}])
        end,
        fun(_) -> stop_mock_config_svr() end,
        fun(_) ->
            [?_assertMatch({ok, []}, init([]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with one module to pre-load.
%% @spec init_one_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_one_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_one_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
            start_mock_config_svr([{"manager",
                        [{"modules", [{"preload", "mod_1"},
                                    {"load_path", Dir}]}]}]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir]),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch({ok, [{"mod_1", {module, mod_1}}]}, init([]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with two modules to pre-load.
%% @spec init_two_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_two_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_two_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
            start_mock_config_svr([{"manager",
                        [{"modules", [{"preload", "mod_1,mod_2"},
                                    {"load_path", Dir}]}]}]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir]),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch({ok, [_, _]}, init([]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Tests initialisation with three modules to pre-load.
%% @spec init_three_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(init_three_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
init_three_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
            start_mock_config_svr([{"manager",
                        [{"modules", [{"preload", "mod_1,mod_2,mod_3"},
                                    {"load_path", Dir}]}]}]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir]),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch({ok, [_, _, _]}, init([]))]
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
%% @doc Removes code paths.
%% @spec remove_code_paths(Paths) -> ok
%% where
%%      Paths = [string()]
%% @end
-spec(remove_code_paths(Paths::[string()]) -> ok).
%%-----------------------------------------------------------------------------
remove_code_paths(Paths) ->
    lists:foreach(fun(X) -> code:del_path(X) end, Paths).


%%-----------------------------------------------------------------------------
%% @doc Creates empty modules in the specified directory.
%% @spec make_modules(Dir, Names) -> [{Filename, module()}]
%% where
%%      Dir = string()
%%      Name = [string()]
%% @end
-spec(make_modules(Dir::string(), Name::[string()]) ->
        [{Filename::string(), module()}]).
%%-----------------------------------------------------------------------------
make_modules(Dir, Names) ->
    ok = file:make_dir(Dir),
    lists:map(fun(N) ->
            SourceName = filename:join(Dir, N) ++ ".erl",
            {ok, Fd} = file:open(SourceName, [write]),
            io:fwrite(Fd, "-module(~s).~n", [N]),
            file:close(Fd),
            compile:file(SourceName, [{outdir, Dir}]),
            BeamName = filename:join(Dir, N) ++ ".beam",
            {BeamName, {module, list_to_atom(N)}}
        end,
        Names).


%%-----------------------------------------------------------------------------
%% @doc Removes modules and the directory created for them.
%% @spec remove_modules(Dir, [{FileName, Module}]) -> ok
%% where
%%      Dir = string()
%%      FileName = string()
%%      Module = module()
%% @end
-spec(remove_modules(Dir::string(),
        [{FileName::string(), Module::module()}]) -> ok).
%%-----------------------------------------------------------------------------
remove_modules(Dir, []) ->
    ok = file:del_dir(Dir);
remove_modules(Dir, [{FileName, {module, Module}}|T]) ->
    ok = file:delete(FileName),
    ok = file:delete(filename:rootname(FileName) ++ ".erl"),
    remove_modules(Dir, T),
    code:purge(Module),
    code:delete(Module).

-endif. % TEST

