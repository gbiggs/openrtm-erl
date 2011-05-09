%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for RTM_Manager_impl.erl.
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
-module(rtm_manager_impl_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include("idl/SDOPackage.hrl").
-include("type_specs.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Test server callbacks
%%-----------------------------------------------------------------------------
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
        end_per_testcase/2]).


%%-----------------------------------------------------------------------------
%% Test cases
%%-----------------------------------------------------------------------------
-export([loaded_mods/1]).


%%=============================================================================
%% Test server callbacks
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Returns a list of test groups and test cases to be executed.
%% @spec all() -> GroupsAndTestCases | {skip, Reason}
%% where
%%      GroupsAndTestCases = [{group, GroupName} | TestCase]
%%      GroupName = atom()
%%          Name of a test case group.
%%      TestCase = atom()
%%          Name of a test case.
%%      Reason = any()
%%          The reason for skipping this entire suite.
%% @end
-spec(all() -> [{group, atom()} | atom()]).
%%-----------------------------------------------------------------------------
all() ->
    [loaded_mods].


%%-----------------------------------------------------------------------------
%% @doc Initialisation to perform before executing the test suite.
%% @spec init_per_suite(Config0) -> Config1
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the suite.
%% @end
-spec(init_per_suite([tuple()]) -> [tuple()]).
%%-----------------------------------------------------------------------------
init_per_suite(Config) ->
    case whereis(orber_sup)
        of undefined ->
            orber:jump_start([{iiop_port,28090}])
         ; _ ->
            ok
    end,
    application:load(sasl),
    application:start(sasl),
    Config.


%%-----------------------------------------------------------------------------
%% @doc Clean up to perform after executing the test suite.
%% @spec end_per_suite(Config) -> ok
%% where
%%      Config = [tuple()]
%%          A list of key/value pairs configuring the suite.
%% @end
-spec(end_per_suite([tuple()]) -> ok).
%%-----------------------------------------------------------------------------
end_per_suite(_Config) ->
    orber:stop(),
    orber:uninstall(),
    mnesia:stop(),
    ok.


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Creates a configuration file for use during tests.
%% @spec create_cfg(ModsDir, Mods) -> ok
%% where
%%      ModsDir = string()
%%          The directory modules will be in.
%%      Mods = string()
%%          A list of the module names, as a single string.
%% @end
-spec(create_cfg(ModsDir::string(), Mods::string()) -> ok).
%%-----------------------------------------------------------------------------
create_cfg(ModsDir, Mods) ->
    {ok, Fd} = file:open("rtc.conf", [write]),
    {ok, CWD} = file:get_cwd(),
    io:fwrite(Fd, "manager.modules.load_path: ~s~n",
        [filename:join(CWD, ModsDir)]),
    io:fwrite(Fd, "manager.modules.preload: ~s~n", [Mods]),
    io:fwrite(Fd, "manager.naming_formats: manager.mgr~n", []),
    file:close(Fd).


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


%%-----------------------------------------------------------------------------
%% @doc Finds the specified object.
%% @spec find_obj(Path) -> object_ref()
%% where
%%      Path = string()
%% @end
-spec(find_obj(Path::string()) -> object_ref()).
%%-----------------------------------------------------------------------------
find_obj(Path) ->
    Path1 = lists:append(["corbaname:iiop:1.2@localhost:28090/NameService#",
            Path]),
    corba:string_to_object(Path1).


%%=============================================================================
%% Tests
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initialisation to perform before executing a test case.
%% @spec init_per_testcase(TestCase, Config0) ->
%%      Config1 | {skip, Reason} | {skip_and_save, Reason, Config1}
%% where
%%      TestCase = atom()
%%          Name of the test case about to be executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%% @end
-spec(init_per_testcase(atom(), [tuple()]) ->
        [tuple()] | {skip, any()} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
init_per_testcase(loaded_mods, Config) ->
    {ok, CWD} = file:get_cwd(),
    Dir = filename:join(CWD, "test_mods"),
    Mods = make_modules(Dir, ["mod_1", "mod_2", "mod_3"]),
    create_cfg("test_mods", "mod_1, mod_2, mod_3"),
    application:start(openrtm_erl),
    [{mods, {Dir, Mods}}, {cfg, "rtc.conf"} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.


%%-----------------------------------------------------------------------------
%% @doc Clean up to perform after executing a test case.
%% @spec end_per_testcase(TestCase, Config0) ->
%%      ok | {save_config, Config1} | {fail, Reason}
%% where
%%      TestCase = atom()
%%          Name of the test case that was executed.
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for failing the test case.
%% @end
-spec(end_per_testcase(atom(), [tuple()]) ->
        ok | {save_config, [tuple()]} | {fail, any()}).
%%-----------------------------------------------------------------------------
end_per_testcase(loaded_mods, Config) ->
    application:stop(openrtm_erl),
    {Dir, Mods} = ?config(mods, Config),
    remove_modules(Dir, Mods),
    FN = ?config(cfg, Config),
    file:delete(FN);
end_per_testcase(_TestCase, _Config) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Tests getting a list of the loaded modules.
%% @spec loaded_mods(Config0) ->
%%      ok | exit() | {skip, Reason} | {comment, Comment} |
%%      {save_config, Config1} | {skip_and_save, Reason, Config1}
%% where
%%      Config0 = Config1 = [tuple()]
%%          A list of key/value pairs configuring the case.
%%      Reason = any()
%%          The reason for skipping this case.
%%      Comment = any()
%%          A comment about the case to be printed in the log.
%% @end
-spec(loaded_mods([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
loaded_mods(_Config) ->
    Mods = mod_svr:loaded_mods(),
    true = lists:member("mod_1", Mods),
    true = lists:member("mod_2", Mods),
    true = lists:member("mod_3", Mods),
    Obj = find_obj("manager.mgr"),
    Mods2 = 'RTM_Manager':get_loaded_modules(Obj),
    true = lists:member(#'SDOPackage_NameValue'{name="file_path",
            value=#any{typecode={tk_string, 0}, value="mod_1"}}, Mods2),
    true = lists:member(#'SDOPackage_NameValue'{name="file_path",
            value=#any{typecode={tk_string, 0}, value="mod_2"}}, Mods2),
    true = lists:member(#'SDOPackage_NameValue'{name="file_path",
            value=#any{typecode={tk_string, 0}, value="mod_3"}}, Mods2),
    ok.

