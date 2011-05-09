%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Test suite for RTC_PortService_impl.erl.
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
-module(rtc_portsvc_impl_SUITE).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("idl/BasicDataType.hrl").
-include("idl/RTC.hrl").
-include("idl/SDOPackage.hrl").
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
-export([get_port_prof/1, get_conn_profs/1, get_conn_prof/1, connect/1,
        connect_bad/1, disconnect/1, disconnect_bad/1, disconnect_all/1,
        connect_push_new/1, send_data_push_new/1]).


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
    [get_port_prof, get_conn_profs, get_conn_prof, connect, connect_bad,
        disconnect, disconnect_bad, disconnect_all, connect_push_new,
        send_data_push_new].


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
%% @spec connect_ports(Obj1, Obj2, Intf) -> {ok, Id} | error
%% where
%%      Obj1 = object_ref()
%%      Obj2 = object_ref()
%%      Intf = string()
%%      Id = string()
%% @end
-spec(connect_ports(Obj1::object_ref(), Obj2::object_ref(), Intf::string()) ->
    {ok, Id::string()} | error).
%%-----------------------------------------------------------------------------
connect_ports(Obj1, Obj2, Intf) ->
    Prof = make_cust_conn_prof(Obj1, Obj2, "", "null", "new", Intf),
    case 'RTC_PortService':connect(Obj1, Prof)
        of {'RTC_OK', #'RTC_ConnectorProfile'{connector_id=Id}} ->
            {ok, Id}
         ; _ ->
            error
    end.


%%-----------------------------------------------------------------------------
%% @doc Connect two ports for testing with custom types.
%% @spec connect_ports(Obj1, Obj2, ConnType, SubType, Intf) ->
%%          {ok, Id} | error
%% where
%%      Obj1 = object_ref()
%%      Obj2 = object_ref()
%%      ConnType = string()
%%      SubType = string()
%%      Intf = string()
%%      Id = string()
%% @end
-spec(connect_ports(Obj1::object_ref(), Obj2::object_ref(), ConnType::string(),
        SubType::string(), Intf::string()) ->
    {ok, Id::string()} | error).
%%-----------------------------------------------------------------------------
connect_ports(Obj1, Obj2, ConnType, SubType, Intf) ->
    Prof = make_cust_conn_prof(Obj1, Obj2, "", ConnType, SubType, Intf),
    case 'RTC_PortService':connect(Obj1, Prof)
        of {'RTC_OK', #'RTC_ConnectorProfile'{connector_id=Id}} ->
            {ok, Id}
         ; _ ->
            error
    end.


%%-----------------------------------------------------------------------------
%% @doc Make a default connection profile.
%% @spec make_conn_prof(Port1, Port2, Id, Intf) -> #'RTC_ConnectorProfile'{}
%% where
%%      Port1 = object_ref()
%%      Port2 = object_ref()
%%      Id = string()
%%      Intf = string()
%% @end
-spec(make_conn_prof(Port1::object_ref(), Port2::object_ref(), Id::string(),
        Intf::string()) -> #'RTC_ConnectorProfile'{}).
%%-----------------------------------------------------------------------------
make_conn_prof(Port1, Port2, Id, Intf) ->
    #'RTC_ConnectorProfile'{name="inport_outport", connector_id=Id,
        ports=[Port1, Port2],
        properties=nvlist:from_list([{"dataport.subscription_type", "flush"},
                {"dataport.interface_type", Intf},
                {"dataport.data_type", "RTC_TimedLong"},
                {"dataport.dataflow_type", "null"}])}.


%%-----------------------------------------------------------------------------
%% @doc Make a connection profile for a given connection/subscription pair.
%% @spec make_cust_conn_prof(Port1, Port2, Id, ConnType, SubType, Intf) ->
%%          #conn_prof{}
%% where
%%      Port1 = pid()
%%      Port2 = pid()
%%      Id = string()
%%      ConnType = string()
%%      SubType = string()
%%      Intf = string()
%% @end
-spec(make_cust_conn_prof(Port1::object_ref(), Port2::object_ref(),
        Id::string(), ConnType::string(), SubType::string(), Intf::string()) ->
    #'RTC_ConnectorProfile'{}).
%%-----------------------------------------------------------------------------
make_cust_conn_prof(Port1, Port2, Id, ConnType, SubType, Intf) ->
    #'RTC_ConnectorProfile'{name="inport_outport", connector_id=Id,
        ports=[Port1, Port2],
        properties=nvlist:from_list([{"dataport.subscription_type", SubType},
                {"dataport.interface_type", Intf},
                {"dataport.data_type", "RTC_TimedLong"},
                {"dataport.dataflow_type", ConnType}])}.


%%-----------------------------------------------------------------------------
%% @doc Check if two port profiles are equal.
%% @spec portprofs_equal(Prof1, Prof2) -> boolean()
%% where
%%      Prof1 = #'RTC_PortProfile'{}
%%      Prof1 = #'RTC_PortProfile'{}
%% @end
-spec(portprofs_equal(Prof1::#'RTC_PortProfile'{},
        Prof2::#'RTC_PortProfile'{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
portprofs_equal(#'RTC_PortProfile'{name=N, interfaces=I, port_ref=P,
            connector_profiles=C, owner=O, properties=Props1},
        #'RTC_PortProfile'{name=N, interfaces=I,
            port_ref=P, connector_profiles=C, owner=O, properties=Props2}) ->
    FGen = fun(L) -> fun(X) -> not lists:member(X, L) end end,
    case {lists:filter(FGen(Props2), Props1),
            lists:filter(FGen(Props1), Props2)}
        of {[], []} ->
            true
         ; _ ->
            false
    end;
portprofs_equal(_Prof1, _Prof2) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Check if two port profiles are equal except for additions to the
%%      connector profile.
%% @spec portprofs_mostly_equal(Prof1, Prof2) -> boolean()
%% where
%%      Prof1 = #'RTC_PortProfile'{}
%%      Prof1 = #'RTC_PortProfile'{}
%% @end
-spec(portprofs_mostly_equal(Prof1::#'RTC_PortProfile'{},
        Prof2::#'RTC_PortProfile'{}) ->
    boolean()).
%%-----------------------------------------------------------------------------
portprofs_mostly_equal(#'RTC_PortProfile'{connector_profiles=C1}=Prof1,
        #'RTC_PortProfile'{connector_profiles=C2}=Prof2) ->
    true = portprofs_equal(Prof1#'RTC_PortProfile'{connector_profiles=nil},
        Prof2#'RTC_PortProfile'{connector_profiles=nil}),
    true = compare_conn_prof_lists(C1, C2, true);
portprofs_mostly_equal(_Prof1, _Prof2) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Check if two sets of connection profiles are equal.
%% @spec compare_conn_prof_lists(C1, C2, IgMissing) -> boolean()
%% where
%%      C1 = C2 = [#'RTC_ConnectorProfile'{}{}]
%%      IgMissing = boolean()
%% @end
-spec(compare_conn_prof_lists(C1::[#'RTC_ConnectorProfile'{}],
        C2::[#'RTC_ConnectorProfile'{}], IgMissing::boolean()) ->
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

-spec(cp_in_list(CP::#'RTC_ConnectorProfile'{}, L::[#'RTC_ConnectorProfile'{}],
        IgMissing::boolean()) -> boolean()).
cp_in_list(_, [], _) ->
    false;
cp_in_list(CP, [H|T], IgMissing) ->
    case comp_cps(CP, H, IgMissing)
        of true ->
            true
         ; false ->
            cp_in_list(CP, T, IgMissing)
    end.

-spec(comp_cps(#'RTC_ConnectorProfile'{}, #'RTC_ConnectorProfile'{},
        IgMissing::boolean()) -> boolean()).
comp_cps(#'RTC_ConnectorProfile'{name=N, connector_id=Id, ports=P,
        properties=Props1}, #'RTC_ConnectorProfile'{name=N, connector_id=Id,
        ports=P, properties=Props2}, IgMissing) ->
    true = comp_props(Props1, Props2, IgMissing).

-spec(comp_props([#'SDOPackage_NameValue'{}], [#'SDOPackage_NameValue'{}],
        IgMissing::boolean()) -> boolean()).
comp_props([], [], _) ->
    true;
comp_props([], _, IgMissing) ->
    IgMissing;
comp_props([H|T], L, IgMissing) ->
    case comp_prop(H, L, IgMissing)
        of false ->
            false
         ; true ->
            comp_props(T, L, IgMissing)
    end.

-spec(comp_prop(#'SDOPackage_NameValue'{}, [#'SDOPackage_NameValue'{}],
        IgMissing::boolean()) -> boolean()).
comp_prop(_, [], IgMissing) ->
    IgMissing;
comp_prop(#'SDOPackage_NameValue'{name=N, value=V},
        [#'SDOPackage_NameValue'{name=N, value=V}|_], _IgMissing) ->
    true;
comp_prop(Prop, [_|T], IgMissing) ->
    comp_prop(Prop, T, IgMissing).


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
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "RTC_TimedLong", nil,
        simple_cfg()),
    Obj1 = portsvc:get_corba_obj(Port1),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "RTC_TimedLong",
        nil, simple_cfg()),
    Obj2 = portsvc:get_corba_obj(Port2),
    {ok, Id} = connect_ports(Obj1, Obj2, "corba_cdr"),
    [{id, Id}, {port1, {Sup1, Port1, Obj1}},
        {port2, {Sup2, Port2, Obj2}} | Config];
init_per_testcase(TestCase, Config) when TestCase == connect;
        TestCase == connect_bad; TestCase == connect_push_new ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "RTC_TimedLong", nil,
        simple_cfg()),
    Obj1 = portsvc:get_corba_obj(Port1),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "RTC_TimedLong",
        nil, simple_cfg()),
    Obj2 = portsvc:get_corba_obj(Port2),
    [{port1, {Sup1, Port1, Obj1}}, {port2, {Sup2, Port2, Obj2}} | Config];
init_per_testcase(send_data_push_new, Config) ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "RTC_TimedLong", nil,
        simple_cfg()),
    Obj1 = portsvc:get_corba_obj(Port1),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "RTC_TimedLong",
        nil, simple_cfg()),
    Obj2 = portsvc:get_corba_obj(Port2),
    {ok, _Id} = connect_ports(Obj1, Obj2, "push", "new", "corba_cdr"),
    [{inport, {Sup1, Port1, Obj1}}, {outport, {Sup2, Port2, Obj2}} | Config];
init_per_testcase(disconnect_all, Config) ->
    {ok, Sup1, Port1} = portsvc:create_datain("inport", "RTC_TimedLong", nil,
        simple_cfg()),
    Obj1 = portsvc:get_corba_obj(Port1),
    {ok, Sup2, Port2} = portsvc:create_dataout("outport", "RTC_TimedLong",
        nil, simple_cfg()),
    Obj2 = portsvc:get_corba_obj(Port2),
    {ok, _Id} = connect_ports(Obj1, Obj2, "corba_cdr"),
    {ok, _Id2} = connect_ports(Obj1, Obj2, "corba_cdr"),
    [{port1, {Sup1, Port1, Obj1}}, {port2, {Sup2, Port2, Obj2}} | Config];
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
end_per_testcase(TestCase, Config) when TestCase == send_data_push_new ->
    {Sup1, _Port1, _Obj1} = ?config(inport, Config),
    {Sup2, _Port2, _Obj2} = ?config(outport, Config),
    portsvc:destroy(Sup1),
    portsvc:destroy(Sup2),
    ok;
end_per_testcase(_TestCase, Config) ->
    {Sup1, _Port1, _Obj1} = ?config(port1, Config),
    {Sup2, _Port2, _Obj2} = ?config(port2, Config),
    portsvc:destroy(Sup1),
    portsvc:destroy(Sup2),
    ok.


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProfs1 = [make_conn_prof(Obj1, Obj2, Id, "corba_cdr")],
    Prof1 = #'RTC_PortProfile'{name="inport", interfaces=[], port_ref=Obj1,
        connector_profiles=ConnProfs1, owner=?ORBER_NIL_OBJREF,
        properties=nvlist:from_list([{"port.port_type", "DataInPort"},
                {"dataport.data_type", "RTC_TimedLong"},
                {"dataport.subscription_type", "new,periodic,flush"},
                {"dataport.dataflow_type", "push,pull,null"},
                {"dataport.interface_types", "erlang,corba_cdr"},
                {"dataport.buffer.size", "10"},
                {"dataport.buffer.type", "queue"}])},
    true = portprofs_mostly_equal(Prof1, 'RTC_PortService':get_port_profile(Obj1)),
    ConnProfs2 = [make_conn_prof(Obj1, Obj2, Id, "corba_cdr")],
    Prof2 = #'RTC_PortProfile'{name="outport", interfaces=[], port_ref=Obj2,
        connector_profiles=ConnProfs2, owner=?ORBER_NIL_OBJREF,
        properties=nvlist:from_list([{"port.port_type", "DataOutPort"},
                {"dataport.data_type", "RTC_TimedLong"},
                {"dataport.subscription_type", "new,periodic,flush"},
                {"dataport.dataflow_type", "push,pull,null"},
                {"dataport.interface_types", "erlang,corba_cdr"},
                {"dataport.buffer.size", "10"},
                {"dataport.buffer.type", "queue"}])},
    true = portprofs_mostly_equal(Prof2, 'RTC_PortService':get_port_profile(Obj2)).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProfs1 = [make_conn_prof(Obj1, Obj2, Id, "corba_cdr")],
    true = compare_conn_prof_lists(ConnProfs1,
        'RTC_PortService':get_connector_profiles(Obj1), true),
    ConnProfs2 = [make_conn_prof(Obj1, Obj2, Id, "corba_cdr")],
    true = compare_conn_prof_lists(ConnProfs2,
        'RTC_PortService':get_connector_profiles(Obj2), true).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Id = ?config(id, Config),
    ConnProf1 = make_conn_prof(Obj1, Obj2, Id, "corba_cdr"),
    true = comp_cps(ConnProf1,
        'RTC_PortService':get_connector_profile(Obj1, Id), true),
    ConnProf2 = make_conn_prof(Obj1, Obj2, Id, "corba_cdr"),
    true = comp_cps(ConnProf2,
        'RTC_PortService':get_connector_profile(Obj2, Id), true).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Prof1 = make_conn_prof(Obj1, Obj2, "id", "corba_cdr"),
    Prof2 = make_conn_prof(Obj2, Obj1, "", "corba_cdr"),
    Res = 'RTC_PortService':connect(Obj1, Prof1),
    {'RTC_OK', #'RTC_ConnectorProfile'{}=Prof3} = Res,
    ct:log("Prof1 = ~p", [Prof1]),
    ct:log("Prof3 = ~p", [Prof3]),
    true = comp_cps(Prof1, Prof3, true),
    Prof5 = 'RTC_PortService':get_connector_profile(Obj1, "id"),
    true = comp_cps(Prof3, Prof5, true),

    {'RTC_OK', Prof4} = 'RTC_PortService':connect(Obj2, Prof2),
    Prof4 = 'RTC_PortService':get_connector_profile(Obj2,
        Prof4#'RTC_ConnectorProfile'.connector_id).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Props1 = nvlist:from_list([{"dataport.dataflow_type", "throw"},
            {"dataport.interface_type", "corba_cdr"},
            {"dataport.subscription_type", "flush"},
            {"dataport.data_type", "TimedLong"}]),
    Props2 = nvlist:from_list([{"dataport.dataflow_type", "push"},
            {"dataport.interface_type", "mr_serialiser"},
            {"dataport.subscription_type", "flush"},
            {"dataport.data_type", "TimedLong"}]),
    Props3 = nvlist:from_list([{"dataport.dataflow_type", "push"},
            {"dataport.interface_type", "corba_cdr"},
            {"dataport.subscription_type", "dump"},
            {"dataport.data_type", "TimedLong"}]),
    Props4 = nvlist:from_list([{"dataport.dataflow_type", "push"},
            {"dataport.interface_type", "corba_cdr"},
            {"dataport.subscription_type", "flush"},
            {"dataport.data_type", "TimedDouble"}]),
    Prof = #'RTC_ConnectorProfile'{name="inport_outport",
        connector_id="id", ports=[Obj1, Obj2], properties=Props1},
    {'BAD_PARAMETER', _} = 'RTC_PortService':connect(Obj1, Prof),
    {'BAD_PARAMETER', _} = 'RTC_PortService':connect(Obj1,
        Prof#'RTC_ConnectorProfile'{properties=Props2}),
    {'BAD_PARAMETER', _} = 'RTC_PortService':connect(Obj1,
        Prof#'RTC_ConnectorProfile'{properties=Props3}),
    {'BAD_PARAMETER', _} = 'RTC_PortService':connect(Obj1,
        Prof#'RTC_ConnectorProfile'{properties=Props4}).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Id = ?config(id, Config),
    1 = length('RTC_PortService':get_connector_profiles(Obj1)),
    1 = length('RTC_PortService':get_connector_profiles(Obj2)),
    'RTC_OK' = 'RTC_PortService':disconnect(Obj1, Id),
    0 = length('RTC_PortService':get_connector_profiles(Obj1)),
    0 = length('RTC_PortService':get_connector_profiles(Obj2)).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    1 = length('RTC_PortService':get_connector_profiles(Obj1)),
    1 = length('RTC_PortService':get_connector_profiles(Obj2)),
    'BAD_PARAMETER' = 'RTC_PortService':disconnect(Obj1, "bad_id"),
    1 = length('RTC_PortService':get_connector_profiles(Obj1)),
    1 = length('RTC_PortService':get_connector_profiles(Obj2)).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    2 = length('RTC_PortService':get_connector_profiles(Obj1)),
    2 = length('RTC_PortService':get_connector_profiles(Obj2)),
    'RTC_OK' = 'RTC_PortService':disconnect_all(Obj1),
    0 = length('RTC_PortService':get_connector_profiles(Obj1)),
    0 = length('RTC_PortService':get_connector_profiles(Obj2)),
    'RTC_OK' = 'RTC_PortService':disconnect_all(Obj1).


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
    {_Sup1, _Port1, Obj1} = ?config(port1, Config),
    {_Sup2, _Port2, Obj2} = ?config(port2, Config),
    Prof = make_cust_conn_prof(Obj1, Obj2, "id", "push", "new", "corba_cdr"),
    Res = 'RTC_PortService':connect(Obj1, Prof),
    {'RTC_OK', #'RTC_ConnectorProfile'{}} = Res,
    true = comp_cps(Prof, 'RTC_PortService':get_connector_profile(Obj1, "id"),
        true).


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
    {_Sup1, InPort, _InPortObj} = ?config(inport, Config),
    {_Sup2, OutPort, _OutPortObj} = ?config(outport, Config),
    Data = #'RTC_TimedLong'{tm=#'RTC_Time'{sec=4, nsec=2}, data=42},
    ok = portsvc:write(OutPort, Data),
    true = portsvc:is_new(InPort),
    Data = portsvc:read(InPort).

