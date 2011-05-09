%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Loadable module server.
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
-module(mod_svr).
-behaviour(gen_server).


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
-export([start_link/0, stop/0, loaded_mods/0]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2,
    code_change/3]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the module manager server.
%% @spec start_link() ->
%%      {ok, pid()} | ignore | {error, Error}
%% where
%%      Error = {already_started, pid()} | any()
%% @end
-spec(start_link() ->
        {ok, pid()} | ignore | {error, {already_started, pid()} | any()}).
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mod_svr}, mod_svr, [], []).


%%-----------------------------------------------------------------------------
%% @doc Stop the module server.
%% @spec stop() -> ok
%% @end
-spec(stop() -> ok).
%%-----------------------------------------------------------------------------
stop() ->
    gen_server:cast(mod_svr, stop).


%%-----------------------------------------------------------------------------
%% @doc Get a list of the file paths of the loaded modules.
%% @spec loaded_mods() -> [string()]
%% @end
-spec(loaded_mods() -> [string()]).
%%-----------------------------------------------------------------------------
loaded_mods() ->
    gen_server:call(mod_svr, loaded_mods).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialise the module manager server.
%% @spec init([]) -> {ok, Modules} | {stop, Reason}
%% where
%%      Modules = mods_list()
%%      Reason = any()
%% @end
-spec(init([]) -> {ok, mods_list()} | {stop, Reason::any()}).
%%-----------------------------------------------------------------------------
init([]) ->
    ?LOG(rtl_info, "Initialising the module server."),
    process_flag(trap_exit, true),
    add_load_path(),
    case config_svr:get_value(["manager", "modules", "preload"])
        of undefined ->
            ?LOG(rtl_info, "No modules to preload."),
            {ok, []}
         ; Mods ->
            {ok, preload_mods(config:tokens(Mods, ","))}
    end.


%%-----------------------------------------------------------------------------
%% @doc Handle call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState}
%% where
%%      Request = any()
%%      From = {pid(), Tag}
%%      Tag = any()
%%      State = mods_list()
%%      Reply = any()
%%      NewState = mods_list()
%% @end
-spec(handle_call(Request::any(), From::{pid(), Tag::any()},
        State::mods_list()) ->
    {reply, Reply::any(), NewState::mods_list()}).
%%-----------------------------------------------------------------------------
handle_call(loaded_mods, _From, State) ->
    {reply, [M || {M, _} <- State], State}.


%%-----------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @spec handle_cast(Request, State) -> {stop, normal, NewState}
%% where
%%      Request = any()
%%      State = mods_list()
%%      NewState = mods_list()
%% @end
-spec(handle_cast(Request::any(), State::mods_list()) ->
    {stop, normal, NewState::mods_list()}).
%%-----------------------------------------------------------------------------
handle_cast(stop, State) ->
    delete_modules(State),
    {stop, normal, State}.


%%-----------------------------------------------------------------------------
%% @doc Handle info messages.
%% @spec handle_info(Info, State) -> {ok, NewState}
%% where
%%      Info = timeout | any()
%%      State = mods_list()
%%      NewState = mods_list()
%% @end
-spec(handle_info(Info::timeout | any(), State::config()) ->
    {ok, NewState::mods_list()}).
%%-----------------------------------------------------------------------------
handle_info(not_supported, State) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Termination function.
%% @spec terminate(Reason, State) -> ok
%% where
%%      Reason = normal | shutdown | {shutdown, any()} | any()
%%      State = mods_list()
%% @end
-spec(terminate(Reason::normal | shutdown | {shutdown, any()} | any(),
        State::mods_list()) -> ok).
%%-----------------------------------------------------------------------------
terminate(normal, _State) ->
    ?LOG(rtl_info, "Shutting down normally.");
terminate(shutdown, _State) ->
    ?LOG(rtl_info, "Shut down by supervisor.");
terminate({shutdown, Reason}, _State) ->
    ?LOG(rtl_info, "Shutting down: ~p", [Reason]);
terminate(Reason, _State) ->
    ?LOG(rtl_info, "Unusual shutdown: ~p", [Reason]).


%%-----------------------------------------------------------------------------
%% @doc Handle code changes.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% where
%%      OldVsn = any() | {down, any()}
%%      State = mods_list()
%%      Extra = any()
%%      NewState = mods_list()
%% @end
-spec(code_change(OldVsn::any() | {down, any()}, State::config(),
        Extra::any()) -> {ok, NewState::config()}).
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------------
%% @doc Add the value of manager.modules.load_path to the code path.
%% @spec add_load_path() -> ok
%% @end
-spec(add_load_path() -> ok).
%%-----------------------------------------------------------------------------
add_load_path() ->
    case config_svr:get_value(["manager", "modules", "load_path"])
        of undefined ->
            ok
         ; Path ->
            code:add_pathsa(config:tokens(Path, ","))
    end.


%%-----------------------------------------------------------------------------
%% @doc Preload modules listed in the manager.modules.preload property.
%% @spec preload_mods([string()]) -> Modules
%% where
%%      Modules = mods_list()
%%      Reason = any()
%% @end
-spec(preload_mods(ModNames::[string()]) -> mods_list()).
%%-----------------------------------------------------------------------------
preload_mods(ModNames) ->
    ?LOG(rtl_debug, "Pre-loading modules: ~p", [ModNames]),
    lists:foldl(fun(M, A) ->
            case preload_module(M)
                of {error, Reason} ->
                    ?LOG(rtl_warn, "Failed to preload module ~s: ~p",
                        [M, Reason]),
                    A
                 ; {notloaded, Reason} ->
                    ?LOG(rtl_info, "Did not preload module ~s: ~p",
                        [M, Reason]),
                    A
                 ; {ok, Mod} ->
                    [{M, Mod} | A]
            end
        end,
        [], ModNames).


%%-----------------------------------------------------------------------------
%% @doc Preload a single module. If the module name is an absolute path (i.e.
%% it begins with a / or with ?:\), then it is loaded using load_abs (but only
%% if manager.modules.abs_path_allowed is true). If a module name is a
%% non-absolute path, it is loaded using load_abs and must be relative to the
%% working directory. Otherwise, the name is turned into an atom and loaded
%% using the code-path-searching load_file.
%% @spec preload_module(string()) -> {ok, Module} | {error, Reason}
%% where
%%      Module = module()
%%      Reason = any()
%% @end
-spec(preload_module(Name::string()) ->
        {ok, Module::module()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
preload_module([$/|_]=Path) ->
    preload_abs_path_mod(Path);
preload_module([_,$:,$\\|_]=Path) ->
    preload_abs_path_mod(Path);
preload_module(Path) ->
    case lists:member($/, Path)
        of true ->
            preload_rel_path_mod(Path)
         ; false ->
            case lists:member($\\, Path)
                of true ->
                    preload_rel_path_mod(Path)
                 ; false ->
                    preload_atom_mod(Path)
            end
    end.

%%-----------------------------------------------------------------------------
%% @doc Preload a module from an absolute path. Will do nothing if the
%% manager.modules.abs_path_allowed property is not true.
%% @spec preload_abs_path_mod(string()) =
%%      {ok, module()} | {error, Reason} | {notloaded, Reason}
%% where
%%      Path = string()
%%      Reason = any()
%% @end
-spec(preload_abs_path_mod(Path::string()) ->
        {ok, module()} | {error, Reason::any()} | {notloaded, Reason::any()}).
%%-----------------------------------------------------------------------------
preload_abs_path_mod(Path) ->
    ?LOG(rtl_debug, "Pre-loading module ~s", [Path]),
    case config_svr:get_value(["manager", "modules", "abs_path_allowed"])
        of true ->
            case code:load_abs(filename:rootname(Path, ".beam"))
                of {module, Module} ->
                    {ok, {module, Module}}
                 ; {error, Reason} ->
                    {error, Reason}
            end
         ; false ->
            {notloaded, abs_path_disabled}
    end.


%%-----------------------------------------------------------------------------
%% @doc Preload a module from a relative path.
%% @spec preload_rel_path_mod(Path) = {ok, module()} | {error, Reason}
%% where
%%      Reason = any()
%% @end
-spec(preload_rel_path_mod(Path::string()) ->
        {ok, module()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
preload_rel_path_mod(Path) ->
    ?LOG(rtl_debug, "Pre-loading module ~s", [Path]),
    case code:load_abs(filename:rootname(Path, ".beam"))
        of {module, Module} ->
            {ok, {module, Module}}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Preload a module from an atom.
%% @spec preload_atom_mod(Mod) = {ok, module()} | {error, Reason}
%% where
%%      Mod = string()
%%      Reason = any()
%% @end
-spec(preload_atom_mod(Path::string()) ->
        {ok, module()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
preload_atom_mod(Mod) ->
    ?LOG(rtl_debug, "Pre-loading module ~s", [Mod]),
    ModAtom = list_to_atom(filename:rootname(Mod, ".beam")),
    case code:load_file(ModAtom)
        of {module, Module} ->
            {ok, {module, Module}}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Delete the code loaded for the managed modules.
%% @spec delete_modules(Modules) = ok
%% where
%%      Modules = mods_list()
%% @end
-spec(delete_modules(Modules::mods_list()) -> ok).
%%-----------------------------------------------------------------------------
delete_modules([]) ->
    ok;
delete_modules([{_Name, {module, Mod}}|T]) ->
    code:purge(Mod),
    code:delete(Mod),
    delete_modules(T).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Tests adding the load path.
%% @spec add_load_path_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(add_load_path_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
add_load_path_test_() ->
    {setup,
        fun() ->
            ok = file:make_dir("sub_dir"),
            start_mock_config_svr([{"manager",
                        [{"modules", [{"load_path", "sub_dir,/tmp"}]}]}]),
            add_load_path()
        end,
        fun(_) ->
            ok = file:del_dir("sub_dir"),
            stop_mock_config_svr(),
            remove_code_paths(["sub_dir", "tmp"])
        end,
        fun(_) ->
            [?_assert(lists:member("sub_dir", code:get_path())),
                ?_assert(lists:member("/tmp", code:get_path()))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules in the code path.
%% @spec preload_mods_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_mods_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_mods_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"load_path", Dir}]}]}]),
            add_load_path(),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir]),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch([{"mod_3.beam", {module, mod_3}},
                        {"mod_2", {module, mod_2}},
                        {"mod_1.beam", {module, mod_1}}],
                    preload_mods(["mod_1.beam", "mod_2", "mod_3.beam"]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules with absolute paths not allowed.
%% @spec preload_mods_no_abs_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_mods_no_abs_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_mods_no_abs_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir1 = filename:join(CWD, "test_mods"),
            Mods1 = make_modules(Dir1, ["mod_1"]),
            Dir2 = filename:join(CWD, "abs_mods"),
            AbsMods = make_modules(Dir2, ["abs_mod"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"load_path", Dir1},
                                {"abs_path_allowed", false}]}]}]),
            add_load_path(),
            {{Dir1, Mods1}, {Dir2, AbsMods}}
        end,
        fun({{Dir1, Mods1}, {Dir2, AbsMods}}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir1]),
            remove_modules(Dir1, Mods1),
            remove_modules(Dir2, AbsMods)
        end,
        fun({_, {_Dir2, [{FN, _}]}}) ->
            [?_assertMatch([{"mod_1.beam", {module, mod_1}}],
                    preload_mods(["mod_1.beam", FN]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules with absolute paths allowed.
%% @spec preload_mods_abs_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_mods_abs_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_mods_abs_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir1 = filename:join(CWD, "test_mods"),
            Mods1 = make_modules(Dir1, ["mod_1"]),
            Dir2 = filename:join(CWD, "abs_mods"),
            AbsMods = make_modules(Dir2, ["abs_mod"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"load_path", Dir1},
                                {"abs_path_allowed", true}]}]}]),
            add_load_path(),
            {{Dir1, Mods1}, {Dir2, AbsMods}}
        end,
        fun({{Dir1, Mods1}, {Dir2, AbsMods}}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir1]),
            remove_modules(Dir1, Mods1),
            remove_modules(Dir2, AbsMods)
        end,
        fun({_, {_Dir2, [{FN, _}]}}) ->
            [?_assertMatch([{FN, {module, abs_mod}},
                        {"mod_1.beam", {module, mod_1}}],
                    preload_mods(["mod_1.beam", FN]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules with relative paths.
%% @spec preload_mods_rel_path_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_mods_rel_path_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_mods_rel_path_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) -> remove_modules(Dir, Mods) end,
        fun(_) ->
            [?_assertMatch([{"test_mods/mod_3.beam", {module, mod_3}},
                        {"test_mods/mod_2.beam", {module, mod_2}},
                        {"test_mods/mod_1.beam", {module, mod_1}}],
                    preload_mods(["test_mods/mod_1.beam",
                            "test_mods/mod_2.beam", "test_mods/mod_3.beam"]))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading a module with absolute paths not allowed.
%% @spec preload_abs_path_mod_no_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_abs_path_mod_no_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_abs_path_mod_no_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "abs_mods"),
            Mods = make_modules(Dir, ["abs_mod"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"abs_path_allowed", false}]}]}]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_modules(Dir, Mods)
        end,
        fun({_, [{FN, _}]}) ->
            [?_assertMatch({notloaded, abs_path_disabled},
                    preload_abs_path_mod(FN)),
                ?_assertMatch({notloaded, abs_path_disabled},
                    preload_abs_path_mod("/tmp/blurgle.beam"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading a module with absolute paths allowed.
%% @spec preload_abs_path_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_abs_path_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_abs_path_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "abs_mods"),
            Mods = make_modules(Dir, ["abs_mod"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"abs_path_allowed", true}]}]}]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_modules(Dir, Mods)
        end,
        fun({_, [{FN, _}]}) ->
            [?_assertMatch({ok, {module, abs_mod}},
                    preload_abs_path_mod(FN)),
                ?_assertMatch({error, nofile},
                    preload_abs_path_mod("/tmp/blurgle.beam"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules in the code path by atom.
%% @spec preload_atom_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_atom_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_atom_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2"]),
            start_mock_config_svr([{"manager", [{"modules",
                            [{"load_path", Dir}]}]}]),
            add_load_path(),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            stop_mock_config_svr(),
            remove_code_paths([Dir]),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch({ok, {module, mod_1}}, preload_atom_mod("mod_1")),
                ?_assertMatch({ok, {module, mod_2}},
                    preload_atom_mod("mod_2.beam")),
                ?_assertMatch({error, nofile}, preload_atom_mod("blurgle"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test preloading modules by relative path.
%% @spec preload_rel_path_mod_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(preload_rel_path_mod_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
preload_rel_path_mod_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1"]),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch({ok, {module, mod_1}},
                    preload_rel_path_mod("test_mods/mod_1.beam")),
                ?_assertMatch({error, nofile},
                    preload_rel_path_mod("test_mods/mod_0.beam"))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test deleting modules.
%% @spec delete_modules_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(delete_modules_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
delete_modules_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir = filename:join(CWD, "test_mods"),
            Mods = make_modules(Dir, ["mod_1", "mod_2"]),
            code:add_patha(Dir),
            code:load_file(mod_1),
            code:load_file(mod_2),
            {Dir, Mods}
        end,
        fun({Dir, Mods}) ->
            code:del_path(Dir),
            remove_modules(Dir, Mods)
        end,
        fun(_) ->
            [?_assertMatch(ok, delete_modules([{"mod_1", {module, mod_1}},
                            {"mod_2", {module, mod_2}}])),
                ?_assertMatch(false, code:is_loaded(mod_1)),
                ?_assertMatch(false, code:is_loaded(mod_2))]
        end
    }.


%%-----------------------------------------------------------------------------
%% @doc Test retrieving a list of loaded modules.
%% @spec loaded_mods_test_() -> {setup, fun(), fun(), fun()}
%% @end
-spec(loaded_mods_test_() ->
    {setup, fun(() -> {log_level(), pid()}),
        fun((any()) -> ok | {error, atom()}),
        fun((any()) -> {integer(), fun((any()) -> any())})}).
%%-----------------------------------------------------------------------------
loaded_mods_test_() ->
    {setup,
        fun() ->
            {ok, CWD} = file:get_cwd(),
            Dir1 = filename:join(CWD, "test_mods"),
            Mods1 = make_modules(Dir1, ["mod_1", "mod_2", "mod_3"]),
            Dir2 = filename:join(CWD, "abs_mods"),
            AbsMods = make_modules(Dir2, ["abs_mod"]),
            start_mock_config_svr([{"manager", [{"modules",
                [{"load_path", Dir1},
                    {"abs_path_allowed", true},
                    {"preload",
                        "mod_1, mod_2, mod_3, " ++ Dir2 ++ "/abs_mod"}]}]}]),
            start_link(),
            {{Dir1, Mods1}, {Dir2, AbsMods}}
        end,
        fun({{Dir1, Mods1}, {Dir2, AbsMods}}) ->
            stop(),
            stop_mock_config_svr(),
            remove_code_paths([Dir1]),
            remove_modules(Dir1, Mods1),
            remove_modules(Dir2, AbsMods)
        end,
        fun({_, {Dir2, _}}) ->
            ?LET(Mods, loaded_mods(),
                [?_assert(lists:member("mod_1", Mods)),
                    ?_assert(lists:member("mod_2", Mods)),
                    ?_assert(lists:member("mod_3", Mods)),
                    ?_assert(lists:member(Dir2 ++ "/abs_mod", Mods))])
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

