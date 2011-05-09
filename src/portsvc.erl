%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc PortService Erlang implementation, plus local extensions. Provides the
%% interface to the base functionality of ports.
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
-module(portsvc).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("type_specs.hrl").
-include("log.hrl").
-include("port.hrl").


%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------
% Port creation and destruction
-export([create_datain/4, create_dataout/4, create/4, destroy/1]).
% PortService functions
-export([get_port_profile/1, get_connector_profiles/1, get_connector_profile/2,
        connect/2, disconnect/2, disconnect_all/1]).
% Local extensions functions
-export([get_datatype/1]).
% Writing and reading data
-export([is_new/1, read/1, write/2]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------
% PortService functions
-export([notify_connect/2, notify_disconnect/2]).
% Extra
-export([set_conns_sup/2, set_buf/2, set_corba_obj/2, get_corba_obj/1,
    make_buf_opts/1]).


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a data in port.
%% @spec create_datain(Name, DataType, Owner, Config) -> {ok, Sup, Port}
%% where
%%      Name = string()
%%      DataType = string()
%%          The data type to be transported by the port. Used only for
%%          the introspection interfaces; Erlang itself doesn't care.
%%      Owner = pid() | nil
%%          The RTC that owns this port.
%%      Config = config()
%%      Sup = pid()
%%          PID of the supervisor process managing the port's processes.
%%      Port = pid()
%%          PID of the worker providing the core port functionality.
%% @see create/2
%% @end
-spec(create_datain(Name::string(), DataType::string(), Owner::pid() | nil,
        Config::config()) ->
    {ok, Sup::pid(), Port::pid()}).
%%-----------------------------------------------------------------------------
create_datain(Name, DataType, Owner, Config) ->
    Config1 = config:set_value(["dataport", "data_type"], DataType, Config),
    Config2 = config:set_value(["port", "port_type"], "DataInPort", Config1),
    Config3 = case config:get_value(["dataport", "interface_types"], Config2)
        of undefined ->
            config:set_value(["dataport", "interface_types"],
                "erlang,corba_cdr", Config2)
         ;  _ ->
            Config2
    end,
    create(Name, Owner, datain_portsvc, Config3).


%%-----------------------------------------------------------------------------
%% @doc Create a data out port.
%% @spec create_dataout(Name, DataType, Owner, Config) -> {ok, Sup, Port}
%% where
%%      Name = string()
%%      DataType = string()
%%          The data type to be transported by the port. Used only for
%%          the introspection interfaces; Erlang itself doesn't care.
%%      Owner = pid() | nil
%%          The RTC that owns this port.
%%      Config = config()
%%      Sup = pid()
%%          PID of the supervisor process managing the port's processes.
%%      Port = pid()
%%          PID of the worker providing the core port functionality.
%% @see create/2
%% @end
-spec(create_dataout(Name::string(), DataType::string(), Owner::pid() | nil,
        Config::config()) ->
    {ok, Sup::pid(), Port::pid()}).
%%-----------------------------------------------------------------------------
create_dataout(Name, DataType, Owner, Config) ->
    Config1 = config:set_value(["dataport", "data_type"], DataType, Config),
    Config2 = config:set_value(["port", "port_type"], "DataOutPort", Config1),
    Config3 = case config:get_value(["dataport", "interface_types"], Config2)
        of undefined ->
            config:set_value(["dataport", "interface_types"],
                "erlang,corba_cdr", Config2)
         ;  _ ->
            Config2
    end,
    create(Name, Owner, dataout_portsvc, Config3).


%%-----------------------------------------------------------------------------
%% @doc Create a port.
%% @spec create(Name, Owner, WorkerMod, Config) ->
%%      {ok, Sup, Port} | {error, Reason}
%% where
%%      Name = string()
%%      Owner = pid() | nil
%%          The RTC that owns this port.
%%      WorkerMod = atom
%%          The module providing the worker implementation.
%%      Config = config()
%%      Sup = pid()
%%          PID of the supervisor process managing the port's processes.
%%      Port = pid()
%%          PID of the worker providing the core port functionality.
%%      Reason = any()
%% @see create/2
%% @end
-spec(create(Name::string(), Owner::pid() | nil, WorkerMod::atom(),
        Config::config()) ->
    {ok, Sup::pid(), Port::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
create(Name, Owner, WorkerMod, Config) ->
    case make_buf_opts(Config)
        of {ok, BufOpts} ->
            create1(BufOpts, {WorkerMod, Name, Owner, Config})
         ; {error, Reason} ->
            {error, Reason}
    end.

create1(BufOpts, WorkerOpts) ->
    ?LOG(rtl_debug, "Creating new port: ~p ~p", [BufOpts, WorkerOpts]),
    case port_sup:start_link(BufOpts, WorkerOpts)
        of {ok, S} ->
            Children = supervisor:which_children(S),
            {conns_sup, CS, supervisor, _} = lists:keyfind(conns_sup, 1,
                Children),
            {buffer, Buf, worker, _} = lists:keyfind(buffer, 1, Children),
            {worker, Worker, worker, _} = lists:keyfind(worker, 1, Children),
            {corba, CORBA, worker, _} = lists:keyfind(corba, 1, Children),
            set_conns_sup(Worker, CS),
            set_buf(Worker, Buf),
            set_corba_obj(Worker, corba_obj_mgr:get_obj(CORBA)),
            corba_obj_mgr:send_info(CORBA, {set_target, Worker}),
            {ok, S, Worker}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Destroy a port.
%% @spec destroy(S) -> ok
%% where
%%      S = pid()
%%          The PID of the top-level supervisor of the port.
%% @end
-spec(destroy(S::pid()) -> ok).
%%-----------------------------------------------------------------------------
destroy(S) ->
    exit(S, normal),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Get the profile of a port.
%% @spec get_port_profile(P) -> Result
%% where
%%      P = pid()
%%      Result = #port_prof{}
%% @end
-spec(get_port_profile(P::pid()) -> Result::#port_prof{}).
%%-----------------------------------------------------------------------------
get_port_profile(P) ->
    gen_server:call(P, get_port_prof).


%%-----------------------------------------------------------------------------
%% @doc Get the profiles of all connectors.
%% @spec get_connector_profiles(P) -> Result
%% where
%%      P = pid()
%%      Result = [#conn_prof{}]
%% @end
-spec(get_connector_profiles(P::pid()) -> Result::[#conn_prof{}]).
%%-----------------------------------------------------------------------------
get_connector_profiles(P) ->
    gen_server:call(P, get_conn_profs).


%%-----------------------------------------------------------------------------
%% @doc Get the profile of a specific connector.
%% @spec get_connector_profile(P, Id) -> Result
%% where
%%      P = pid()
%%      Id = string()
%%      Result = [#conn_prof{}]
%% @end
-spec(get_connector_profile(P::pid(), Id::string()) -> Result::#conn_prof{}).
%%-----------------------------------------------------------------------------
get_connector_profile(P, Id) ->
    gen_server:call(P, {get_conn_prof, Id}).


%%-----------------------------------------------------------------------------
%% @doc Connect this and other ports.
%% @spec connect(P, ConnProf) -> Result
%% where
%%      P = pid()
%%      ConnProf = #conn_prof{}
%%          The connector profile of the connection to make.
%%      Result = {rtc_cb_return(), #conn_prof{}}
%% @end
-spec(connect(P::pid(), ConnProf::#conn_prof{}) ->
    Result::{rtc_cb_return(), #conn_prof{}}).
%%-----------------------------------------------------------------------------
connect(P, ConnProf) ->
    gen_server:call(P, {connect, ConnProf}).


%%-----------------------------------------------------------------------------
%% @doc Disconnect a single connection.
%% @spec disconnect(P, ID) -> Result
%% where
%%      P = pid()
%%      Id = string()
%%          The ID of the connection to disconnect.
%%      Result = rtc_cb_return()
%% @end
-spec(disconnect(P::pid(), Id::string()) -> Result::rtc_cb_return()).
%%-----------------------------------------------------------------------------
disconnect(P, Id) ->
    gen_server:call(P, {disconnect, Id}).


%%-----------------------------------------------------------------------------
%% @doc Disconnect all connections on this port.
%% @spec disconnect_all(P) -> Result
%% where
%%      P = pid()
%%      Result = rtc_cb_return()
%% @end
-spec(disconnect_all(P::pid()) -> Result::rtc_cb_return()).
%%-----------------------------------------------------------------------------
disconnect_all(P) ->
    gen_server:call(P, disconnect_all).


%%-----------------------------------------------------------------------------
%% @doc Get the datatype served by a port.
%% @spec get_datatype(P) -> Result
%% where
%%      P = pid()
%%      Result = string()
%% @end
-spec(get_datatype(P::pid()) -> Result::string()).
%%-----------------------------------------------------------------------------
get_datatype(P) ->
    gen_server:call(P, get_datatype).


%%-----------------------------------------------------------------------------
%% @doc Check if the port has data waiting to be read.
%% @spec is_new(P) -> boolean
%% where
%%      P = pid()
%% @end
-spec(is_new(P::pid()) -> boolean()).
%%-----------------------------------------------------------------------------
is_new(P) ->
    gen_server:call(P, is_new).


%%-----------------------------------------------------------------------------
%% @doc Read the first waiting piece of data from the port.
%% @spec read(P) -> {ok, D} | error
%% where
%%      P = pid()
%% @end
-spec(read(P::pid()) -> {ok, D::any()} | error).
%%-----------------------------------------------------------------------------
read(P) ->
    gen_server:call(P, read).


%%-----------------------------------------------------------------------------
%% @doc Write a piece of data to the port.
%% @spec write(P, D) -> boolean
%% where
%%      P = pid()
%% @end
-spec(write(P::pid(), D::any()) -> boolean()).
%%-----------------------------------------------------------------------------
write(P, D) ->
    gen_server:call(P, {write, D}).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Notify the PortService that it is being connected.
%% @spec notify_connect(P, ConnProf) -> Result
%% where
%%      P = pid()
%%      ConnProf = #conn_prof{}
%%      Result = {rtc_cb_return(), #conn_prof{}}
%% @end
-spec(notify_connect(P::pid(), ConnProf::#conn_prof{}) ->
    Result::{rtc_cb_return(), #conn_prof{}}).
%%-----------------------------------------------------------------------------
notify_connect(P, ConnProf) ->
    gen_server:call(P, {notify_conn, ConnProf}).


%%-----------------------------------------------------------------------------
%% @doc Notify the PortService that it is being disconnected.
%% @spec notify_disconnect(P, Id) -> Result
%% where
%%      P = pid()
%%      Id = string()
%%      Result = rtc_cb_return()
%% @end
-spec(notify_disconnect(P::pid(), Id::string()) -> Result::rtc_cb_return()).
%%-----------------------------------------------------------------------------
notify_disconnect(P, Id) ->
    gen_server:call(P, {notify_dis, Id}).


%%-----------------------------------------------------------------------------
%% @doc Find the buffer options in a config.
%% @spec make_buf_opts(Config) ->
%%      {ok, BufOpts} | {error, bad_size | bad_type}
%% where
%%      Config = config()
%%      BufOpts = {BufMod, BufCfg}
%%      BufMod = atom()
%%      BufCfg = config()
%% @end
-spec(make_buf_opts(Config::config()) ->
    {ok, BufOpts::{BufMod::atom(), BufCfg::config()}} |
        {error, bad_size | bad_type}).
%%-----------------------------------------------------------------------------
make_buf_opts(Config) ->
    BufCfg = config:get_value(["dataport", "buffer"], [], Config),
    case check_buf_config(BufCfg)
        of ok ->
            case config:get_value("type", "queue", BufCfg)
                of "queue" ->
                    {ok, {queue_buffer, BufCfg}}
                 ; _ ->
                    {error, bad_type}
            end
         ; {error, Reason} ->
            {error, Reason}
    end.

check_buf_config(Config) ->
    case config:get_value("size", Config)
        of undefined ->
            ok
         ; S ->
            try list_to_integer(S)
                of _ ->
                    ok
            catch
                error:badarg ->
                    {error, bad_size}
            end
    end.


%%-----------------------------------------------------------------------------
%% @doc Set the connections supervisor for a port.
%% @spec set_conns_sup(Port, Pid) -> ok
%% where
%%      Port = pid()
%%      Pid = pid()
%% @end
-spec(set_conns_sup(Port::pid(), Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_conns_sup(Port, Pid) ->
    gen_server:call(Port, {set_conns_sup, Pid}).


%%-----------------------------------------------------------------------------
%% @doc Set the buffer for a port.
%% @spec set_buf(Port, Pid) -> ok
%% where
%%      Port = pid()
%%      Pid = pid()
%% @end
-spec(set_buf(Port::pid(), Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_buf(Port, Pid) ->
    gen_server:call(Port, {set_buf, Pid}).


%%-----------------------------------------------------------------------------
%% @doc Set the CORBA object providing the interface for a port.
%% @spec set_corba_obj(Port, Obj) -> ok
%% where
%%      Port = pid()
%%      Obj = object_ref()
%% @end
-spec(set_corba_obj(Port::pid(), Obj::object_ref()) -> ok).
%%-----------------------------------------------------------------------------
set_corba_obj(Port, Obj) ->
    gen_server:call(Port, {set_corba_obj, Obj}).


%%-----------------------------------------------------------------------------
%% @doc Get the CORBA object providing the interface for a port.
%% @spec get_corba_obj(Port) -> Obj
%% where
%%      Port = pid()
%%      Obj = object_ref()
%% @end
-spec(get_corba_obj(Port::pid()) -> Obj::object_ref()).
%%-----------------------------------------------------------------------------
get_corba_obj(Port) ->
    gen_server:call(Port, get_corba_obj).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

%%-----------------------------------------------------------------------------
%% @doc Test making buffer options.
%% @spec make_buf_opts_test() -> ok
%% @end
-spec(make_buf_opts_test() -> ok).
%%-----------------------------------------------------------------------------
make_buf_opts_test() ->
    ?assertMatch({ok, {queue_buffer, [{"size", "10"}, {"type", "queue"}]}},
        make_buf_opts([{"dataport", [{"subscription_type", "new, periodic"},
                    {"dataflow_type", "push, pull"},
                    {"buffer", [{"size", "10"}, {"type", "queue"}]}]}])),
    ?assertMatch({error, bad_size},
        make_buf_opts([{"dataport", [{"subscription_type", "new, periodic"},
                    {"dataflow_type", "push, pull"},
                    {"buffer", [{"size", "A"}, {"type", "queue"}]}]}])),
    ?assertMatch({error, bad_type},
        make_buf_opts([{"dataport", [{"subscription_type", "new, periodic"},
                    {"dataflow_type", "push, pull"},
                    {"buffer", [{"size", "10"}, {"type", "list"}]}]}])).

-endif. % TEST

