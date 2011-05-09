%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for portsvc.erl.
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
-module(portsvc_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("type_specs.hrl").
-include("port.hrl").


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
-export([eunit_tests/1, create_destroy/1, get_port_prof/1, get_conn_profs/1,
        get_conn_prof/1, connect/1, connect_bad/1, disconnect/1,
        disconnect_bad/1, disconnect_all/1, connect_push_new/1,
        send_data_push_new/1]).


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
    [eunit_tests, create_destroy, get_port_prof, get_conn_profs, get_conn_prof,
        connect, connect_bad, disconnect, disconnect_bad, disconnect_all,
        connect_push_new, send_data_push_new].


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
%% @doc Make a simple port configuration.
%% @spec simple_cfg() -> config()
%% @end
-spec(simple_cfg() -> config()).
%%-----------------------------------------------------------------------------
simple_cfg() ->
    config:set_value("dataport", [{"subscription_type", "new,periodic,flush"},
            {"dataflow_type", "push,pull,null"},
            {"buffer", [{"size", "10"}, {"type", "queue"}]}],
        config:empty_conf()).


%%-----------------------------------------------------------------------------
%% @doc Connect two ports for testing.
%% @spec connect_ports(Port1, Port2, Intf) -> {ok, Id} | error
%% where
%%      Port1 = pid()
%%      Port2 = pid()
%%      Intf = string()
%%      Id = string()
%% @end
-spec(connect_ports(Port1::pid(), Port2::pid(), Intf::string()) ->
    {ok, Id::string()} | error).
%%-----------------------------------------------------------------------------
connect_ports(Port1, Port2, Intf) ->
    Prof = #conn_prof{name="inport_outport", id="", ports=[Port1, Port2],
        props=[{"dataport", [{"dataflow_type", "null"},
            {"data_type", portsvc:get_datatype(Port1)},
            {"interface_type", Intf},
            {"subscription_type", "flush"}]}]},
    {ok, NewProf} = portsvc:connect(Port1, Prof),
    {ok, NewProf#conn_prof.id}.


%%-----------------------------------------------------------------------------
%% @doc Connect two ports for testing with custom types.
%% @spec connect_ports(Port1, Port2, ConnType, SubType, Intf) ->
%%          {ok, Id} | error
%% where
%%      Port1 = pid()
%%      Port2 = pid()
%%      ConnType = string()
%%      SubType = string()
%%      Intf = string()
%%      Id = string()
%% @end
-spec(connect_ports(Port1::pid(), Port2::pid(), ConnType::string(),
        SubType::string(), Intf::string()) ->
    {ok, Id::string()} | error).
%%-----------------------------------------------------------------------------
connect_ports(Port1, Port2, ConnType, SubType, Intf) ->
    Prof = #conn_prof{name="inport_outport", id="", ports=[Port1, Port2],
        props=[{"dataport", [{"dataflow_type", ConnType},
            {"data_type", portsvc:get_datatype(Port1)},
            {"interface_type", Intf},
            {"subscription_type", SubType}]}]},
    {ok, NewProf} = portsvc:connect(Port1, Prof),
    {ok, NewProf#conn_prof.id}.


%%-----------------------------------------------------------------------------
%% @doc Make a default connection profile.
%% @spec make_conn_prof(Port1, Port2, Id, Intf) -> #conn_prof{}
%% where
%%      Port1 = pid()
%%      Port2 = pid()
%%      Id = string()
%%      Intf = string()
%% @end
-spec(make_conn_prof(Port1::object_ref(), Port2::object_ref(), Id::string(),
        Intf::string()) -> #conn_prof{}).
%%-----------------------------------------------------------------------------
make_conn_prof(Port1, Port2, Id, Intf) ->
    #conn_prof{name="inport_outport", id=Id, ports=[Port1, Port2],
        props=[{"dataport", [{"dataflow_type", "null"},
                {"data_type", portsvc:get_datatype(Port1)},
                {"interface_type", Intf},
                {"subscription_type", "flush"}]}]}.


%%-----------------------------------------------------------------------------
%% @doc Make a connection profile for a given connection/subscription pair.
%% @spec make_cust_conn_prof(Port1, Port2, ConnType, SubType, Intf) ->
%%          #conn_prof{}
%% where
%%      Port1 = pid()
%%      Port2 = pid()
%%      ConnType = string()
%%      SubType = string()
%%      Intf = string()
%% @end
-spec(make_cust_conn_prof(Port1::object_ref(), Port2::object_ref(),
        ConnType::string(), SubType::string(), Intf::string()) ->
    #conn_prof{}).
%%-----------------------------------------------------------------------------
make_cust_conn_prof(Port1, Port2, ConnType, SubType, Intf) ->
    #conn_prof{name="custom_conn", id="", ports=[Port1, Port2],
        props=[{"dataport", [{"dataflow_type", ConnType},
                {"data_type", portsvc:get_datatype(Port1)},
                {"interface_type", Intf},
                {"subscription_type", SubType}]}]}.


%%-----------------------------------------------------------------------------
%% @doc Check if two port profiles are equal.
%% @spec portprofs_equal(Prof1, Prof2) -> boolean()
%% where
%%      Prof1 = #port_prof{}
%%      Prof1 = #port_prof{}
%% @end
-spec(portprofs_equal(Prof1::#port_prof{}, Prof2::#port_prof{}) -> boolean()).
%%-----------------------------------------------------------------------------
portprofs_equal(#port_prof{name=N, interfaces=I, port_ref=P, conn_profs=C,
            owner=O, props=Props1},
        #port_prof{name=N, interfaces=I, port_ref=P, conn_profs=C,
            owner=O, props=Props2}) ->
    config:compare(Props1, Props2, false, false);
portprofs_equal(_Prof1, _Prof2) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Check if two port profiles are equal except for additions to the
%%      connector profile.
%% @spec portprofs_mostly_equal(Prof1, Prof2) -> boolean()
%% where
%%      Prof1 = #port_prof{}
%%      Prof1 = #port_prof{}
%% @end
-spec(portprofs_mostly_equal(Prof1::#port_prof{}, Prof2::#port_prof{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
portprofs_mostly_equal(#port_prof{name=N, interfaces=I, port_ref=P,
            conn_profs=C1, owner=O, props=Props1},
        #port_prof{name=N, interfaces=I, port_ref=P, conn_profs=C2,
            owner=O, props=Props2}) ->
    true = config:compare(Props1, Props2, false, false),
    true = compare_conn_prof_lists(C1, C2, true);
portprofs_mostly_equal(_Prof1, _Prof2) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Check if two sets of connection profiles are equal.
%% @spec compare_conn_prof_lists(C1, C2, IgMissing) -> boolean()
%% where
%%      C1 = C2 = [#conn_prof{}]
%%      IgMissing = boolean()
%% @end
-spec(compare_conn_prof_lists(C1::[#conn_prof{}], C2::[#conn_prof{}],
        IgMissing::boolean()) ->
    boolean()).
%%-----------------------------------------------------------------------------
compare_conn_prof_lists([], _, _) ->
    true;
compare_conn_prof_lists([H|T], C2, IgnoreMissing) ->
    case cp_in_list(H, C2, IgnoreMissing)
        of true ->
            compare_conn_prof_lists(T, C2, IgnoreMissing)
         ; false ->
            false
    end.

-spec(cp_in_list(CP::#conn_prof{}, L::[#conn_prof{}], IgMissing::boolean()) ->
    boolean()).
cp_in_list(_, [], _) ->
    false;
cp_in_list(CP, [H|T], IgMissing) ->
    case comp_cps(CP, H, IgMissing)
        of true ->
            true
         ; false ->
            cp_in_list(CP, T, IgMissing)
    end.

-spec(comp_cps(#conn_prof{}, #conn_prof{}, IgMissing::boolean()) -> boolean()).
comp_cps(#conn_prof{id=Id, ports=P, props=Props1},
        #conn_prof{id=Id, ports=P, props=Props2}, IgMissing) ->
    config:compare(Props1, Props2, IgMissing, false).


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
init_per_testcase(TestCase, Config) when TestCase == get_port_prof;
        TestCase == get_conn_profs; TestCase == get_conn_prof;
        TestCase == disconnect; TestCase == disconnect_bad ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "integer", nil,
        simple_cfg()),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "integer", nil,
        simple_cfg()),
    {ok, Id} = connect_ports(Port1, Port2, "erlang"),
    [{id, Id}, {port1, {Sup1, Port1}}, {port2, {Sup2, Port2}} | Config];
init_per_testcase(TestCase, Config) when TestCase == connect;
        TestCase == connect_bad; TestCase == connect_push_new ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "integer", nil,
        simple_cfg()),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "integer", nil,
        simple_cfg()),
    [{port1, {Sup1, Port1}}, {port2, {Sup2, Port2}} | Config];
init_per_testcase(TestCase, Config) when TestCase == send_data_push_new ->
    {ok, Sup1, InPort} = portsvc:create_datain("inport", "integer", nil,
        simple_cfg()),
    {ok, Sup2, OutPort} = portsvc:create_dataout("outport", "integer", nil,
        simple_cfg()),
    {ok, Id} = connect_ports(InPort, OutPort, "push", "new", "erlang"),
    [{inport, {Sup1, InPort}}, {outport, {Sup2, OutPort}} | Config];
init_per_testcase(disconnect_all, Config) ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "integer", nil,
        simple_cfg()),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "integer", nil,
        simple_cfg()),
    {ok, _Id} = connect_ports(Port1, Port2, "erlang"),
    {ok, _Id2} = connect_ports(Port1, Port2, "erlang"),
    [{port1, {Sup1, Port1}}, {port2, {Sup2, Port2}} | Config];
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
end_per_testcase(TestCase, Config) when TestCase == get_port_prof;
        TestCase == get_conn_profs; TestCase == get_conn_prof;
        TestCase == disconnect; TestCase == disconnect_bad;
        TestCase == connect; TestCase == connect_bad;
        TestCase == disconnect_all; TestCase == connect_push_new ->
    {Sup1, _Port1} = ?config(port1, Config),
    {Sup2, _Port2} = ?config(port2, Config),
    portsvc:destroy(Sup1),
    portsvc:destroy(Sup2),
    ok;
end_per_testcase(TestCase, Config) when TestCase == send_data_push_new ->
    {Sup1, _} = ?config(inport, Config),
    {Sup2, _} = ?config(outport, Config),
    portsvc:destroy(Sup1),
    portsvc:destroy(Sup2),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.


%%-----------------------------------------------------------------------------
%% @doc Executes internal EUnit tests.
%% @spec eunit_tests(Config0) ->
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
-spec(eunit_tests([tuple()]) ->
        ok | {skip, any()} | {comment, any()} |
        {save_config, [tuple()]} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
eunit_tests(_Config) ->
    ok = portsvc:test().


%%-----------------------------------------------------------------------------
%% @doc Tests creating and destroying a port.
%% @spec create_destroy(Config0) ->
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
-spec(create_destroy([tuple()]) ->
        ok | {skip, any()} | {comment, any()} |
        {save_config, [tuple()]} | {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
create_destroy(_Config) ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "integer", self(),
        simple_cfg()),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "integer", self(),
        []),
    true = is_process_alive(Port1),
    true = is_process_alive(Port2),
    ok = portsvc:destroy(Sup1),
    ok = portsvc:destroy(Sup2),
    timer:sleep(100),
    false = is_process_alive(Port1),
    false = is_process_alive(Port2).


%%-----------------------------------------------------------------------------
%% @doc Tests getting the port profile.
%% @spec get_port_prof(Config0) ->
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
-spec(get_port_prof([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_port_prof(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProfs1 = [make_conn_prof(Port1, Port2, Id, "erlang")],
    Prof1 = #port_prof{name="inport", interfaces=[], port_ref=Port1,
        conn_profs=ConnProfs1, owner=nil,
        props=[{"port", [{"port_type", "DataInPort"}]},
                {"dataport", [{"data_type", "integer"},
                {"subscription_type", "new,periodic,flush"},
                {"dataflow_type", "push,pull,null"},
                {"interface_types", "erlang,corba_cdr"},
                {"buffer", [{"size", "10"}, {"type", "queue"}]}]}]},
    true = portprofs_mostly_equal(Prof1, portsvc:get_port_profile(Port1)),
    ConnProfs2 = [make_conn_prof(Port1, Port2, Id, "erlang")],
    Prof2 = #port_prof{name="outport", interfaces=[], port_ref=Port2,
        conn_profs=ConnProfs2, owner=nil,
        props=[{"port", [{"port_type", "DataOutPort"}]},
                {"dataport", [{"data_type", "integer"},
                {"subscription_type", "new,periodic,flush"},
                {"dataflow_type", "push,pull,null"},
                {"interface_types", "erlang,corba_cdr"},
                {"buffer", [{"size", "10"}, {"type", "queue"}]}]}]},
    true = portprofs_mostly_equal(Prof2, portsvc:get_port_profile(Port2)).


%%-----------------------------------------------------------------------------
%% @doc Tests getting connector profiles.
%% @spec get_conn_profs(Config0) ->
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
-spec(get_conn_profs([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_conn_profs(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProfs1 = [make_conn_prof(Port1, Port2, Id, "erlang")],
    ConnProfs1 = portsvc:get_connector_profiles(Port1),
    ConnProfs2 = [make_conn_prof(Port1, Port2, Id, "erlang")],
    true = compare_conn_prof_lists(ConnProfs2,
        portsvc:get_connector_profiles(Port2), true).


%%-----------------------------------------------------------------------------
%% @doc Tests getting a single connector profile.
%% @spec get_conn_prof(Config0) ->
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
-spec(get_conn_prof([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
get_conn_prof(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProf1 = make_conn_prof(Port1, Port2, Id, "erlang"),
    ConnProf1 = portsvc:get_connector_profile(Port1, Id),
    ConnProf2 = make_conn_prof(Port1, Port2, Id, "erlang"),
    true = comp_cps(ConnProf2, portsvc:get_connector_profile(Port2, Id), true).


%%-----------------------------------------------------------------------------
%% @doc Tests connecting two ports.
%% @spec connect(Config0) ->
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
-spec(connect([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
connect(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Prof1 = make_conn_prof(Port1, Port2, "id", "erlang"),
    Prof2 = make_conn_prof(Port1, Port2, "", "corba_cdr"),
    Res = portsvc:connect(Port1, Prof1),
    {ok, #conn_prof{}} = Res,
    Prof1 = portsvc:get_connector_profile(Port1, "id"),
    {ok, Prof3} = portsvc:connect(Port2, Prof2),
    Prof3 = portsvc:get_connector_profile(Port2, Prof3#conn_prof.id).


%%-----------------------------------------------------------------------------
%% @doc Tests connecting two ports using a bad profile.
%% @spec connect_bad(Config0) ->
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
-spec(connect_bad([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
connect_bad(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Props1 = [{"dataport", [{"dataflow_type", "throw"},
            {"interface_type", "corba_cdr"},
            {"subscription_type", "flush"},
            {"data_type", "TimedLong"}]}],
    Props2 = [{"dataport", [{"dataflow_type", "push"},
            {"interface_type", "mr_serialiser"},
            {"subscription_type", "flush"},
            {"data_type", "TimedLong"}]}],
    Props3 = [{"dataport", [{"dataflow_type", "push"},
            {"interface_type", "corba_cdr"},
            {"subscription_type", "dump"},
            {"data_type", "TimedLong"}]}],
    Props4 = [{"dataport", [{"dataflow_type", "push"},
            {"interface_type", "corba_cdr"},
            {"subscription_type", "flush"},
            {"data_type", "TimedDouble"}]}],
    Prof = #conn_prof{name="inport_outport",
        id="id", ports=[Port1, Port2], props=Props1},
    {bad_param, _} = portsvc:connect(Port1, Prof),
    {bad_param, _} = portsvc:connect(Port1, Prof#conn_prof{props=Props2}),
    {bad_param, _} = portsvc:connect(Port1, Prof#conn_prof{props=Props3}),
    {bad_param, _} = portsvc:connect(Port1, Prof#conn_prof{props=Props4}).


%%-----------------------------------------------------------------------------
%% @doc Tests disconnecting a single connection.
%% @spec disconnect(Config0) ->
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
-spec(disconnect([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
disconnect(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Id = ?config(id, Config),
    1 = length(portsvc:get_connector_profiles(Port1)),
    1 = length(portsvc:get_connector_profiles(Port2)),
    ok = portsvc:disconnect(Port1, Id),
    0 = length(portsvc:get_connector_profiles(Port1)),
    0 = length(portsvc:get_connector_profiles(Port2)).


%%-----------------------------------------------------------------------------
%% @doc Tests disconnecting a non-existent connection.
%% @spec disconnect_bad(Config0) ->
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
-spec(disconnect_bad([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
disconnect_bad(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    1 = length(portsvc:get_connector_profiles(Port1)),
    1 = length(portsvc:get_connector_profiles(Port2)),
    bad_param = portsvc:disconnect(Port1, "bad_id"),
    1 = length(portsvc:get_connector_profiles(Port1)),
    1 = length(portsvc:get_connector_profiles(Port2)).


%%-----------------------------------------------------------------------------
%% @doc Tests disconnecting all connections.
%% @spec disconnect_all(Config0) ->
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
-spec(disconnect_all([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
disconnect_all(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    2 = length(portsvc:get_connector_profiles(Port1)),
    2 = length(portsvc:get_connector_profiles(Port2)),
    ok = portsvc:disconnect_all(Port1),
    0 = length(portsvc:get_connector_profiles(Port1)),
    0 = length(portsvc:get_connector_profiles(Port2)),
    ok = portsvc:disconnect_all(Port1).


%%-----------------------------------------------------------------------------
%% @doc Tests connecting two ports with a push/new connection.
%% @spec connect_push_new(Config0) ->
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
-spec(connect_push_new([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
connect_push_new(Config) ->
    {_Sup1, Port1} = ?config(port1, Config),
    {_Sup2, Port2} = ?config(port2, Config),
    Prof = #conn_prof{name="inport_outport", id="id", ports=[Port1, Port2],
        props=[{"dataport", [{"dataflow_type", "push"},
                {"data_type", portsvc:get_datatype(Port1)},
                {"interface_type", "erlang"},
                {"subscription_type", "new"}]}]},
    Res = portsvc:connect(Port1, Prof),
    {ok, #conn_prof{}} = Res,
    Prof = portsvc:get_connector_profile(Port1, "id").


%%-----------------------------------------------------------------------------
%% @doc Tests sending data via a push/new connection.
%% @spec send_data_push_new(Config0) ->
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
-spec(send_data_push_new([tuple()]) ->
        ok | {skip, any()} | {comment, any()} | {save_config, [tuple()]} |
        {skip_and_save, any(), [tuple()]}).
%%-----------------------------------------------------------------------------
send_data_push_new(Config) ->
    {_Sup1, InPort} = ?config(inport, Config),
    {_Sup2, OutPort} = ?config(outport, Config),
    ok = portsvc:write(OutPort, 42),
    true = portsvc:is_new(InPort),
    42 = portsvc:read(InPort).

