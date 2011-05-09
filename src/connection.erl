%%-----------------------------------------------------------------------------
%% @author Geoffrey Biggs <geoffrey.biggs@aist.go.jp>
%% @doc Interface to connection processes.
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
-module(connection).


%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
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
-export([get_prof/1, get_ior/1, disconnect/1, read/1, write/2, create/3]).
% DataPort interfaces
-export([put/2, get/1]).


%%-----------------------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------------------


%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Get a connection's profile.
%% @spec get_prof(pid()) -> #conn_prof{}
%% @end
-spec(get_prof(Pid::pid()) -> Profile::#conn_prof{}).
%%-----------------------------------------------------------------------------
get_prof(Pid) ->
    gen_server:call(Pid, get_prof).


%%-----------------------------------------------------------------------------
%% @doc Get the IOR of a connection's CORBA interface (if it has one).
%% @spec get_ior(pid()) -> string() | nil
%% @end
-spec(get_ior(Pid::pid()) -> string() | nil).
%%-----------------------------------------------------------------------------
get_ior(Pid) ->
    gen_server:call(Pid, get_ior).


%%-----------------------------------------------------------------------------
%% @doc Tell a connection to disconnect and shutdown.
%% @spec disconnect(S) -> ok
%% where
%%      S = pid()
%%          The PID of the top-level supervisor of the connection.
%% @end
-spec(disconnect(S::pid()) -> ok).
%%-----------------------------------------------------------------------------
disconnect(S) ->
    exit(S, normal),
    ok.


%%-----------------------------------------------------------------------------
%% @doc Read the connection. May do nothing if the connection reads
%% automatically.
%% @spec read(Pid) -> ok | error
%% where
%%      Pid = pid()
%%          The PID of the connection worker.
%% @end
-spec(read(Pid::pid()) -> ok | error).
%%-----------------------------------------------------------------------------
read(Pid) ->
    gen_server:call(Pid, read).


%%-----------------------------------------------------------------------------
%% @doc Write data to a connection.
%% @spec write(Pid, Data) -> Result
%% where
%%      Pid = pid()
%%          PID of the connetion's worker process.
%%      Data = any()
%%      Result = ok | conn_lost | buff_full | buff_error | precon_not_met |
%%          port_err
%% @end
-spec(write(Pid::pid(), Data::any()) ->
    Result::ok | conn_lost | buff_full | buff_err | precon_not_met | port_err).
%%-----------------------------------------------------------------------------
write(Pid, Data) ->
    case gen_server:call(Pid, {write, Data})
        of port_ok ->
            ok
         ; _ ->
            port_err
    end.


%%-----------------------------------------------------------------------------
%% @doc Create a new connection.
%% @spec create(Type, Prof, Buf) -> Result
%% where
%%      Type = inport | outport
%%      Prof = #conn_prof{}
%%      Buf = pid() | nil
%%          Destination buffer for data received for inport connections.
%%      Result = {ok, Sup, Worker} | {error, Reason}
%%      Sup = pid()
%%          PID of the connection's top-level supervisor.
%%      Worker = pid()
%%          PID of the connection's worker process.
%%      Reason = any()
%% @end
-spec(create(Type::inport | outport, Prof::#conn_prof{}, Buf::pid() | nil) ->
    Result::{ok, Sup::pid(), Worker::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
create(inport, #conn_prof{props=Props}=Prof, Buf) ->
    ?LOG(rtl_debug, "Creating inport connector ~p", [Props]),
    Type = config:get_value(["dataport", "dataflow_type"], "push", Props),
    create_inport_conn(Type, Prof, Buf);
create(outport, #conn_prof{props=Props}=Prof, nil) ->
    ?LOG(rtl_debug, "Creating outport connector ~p", [Props]),
    Type = config:get_value(["dataport", "dataflow_type"], "push", Props),
    create_outport_conn(Type, Prof).


%%-----------------------------------------------------------------------------
%% @doc Put data into this connector (push connection, inport).
%% @spec put(Pid, Data) -> Result
%% where
%%      Pid = pid()
%%      Data = any()
%%      Result = port_ok | port_error | buffer_full | buffer_empty |
%%          buffer_timeout | unknown_error
%% @end
-spec(put(Pid::pid(), Data::any()) ->
    Result::port_ok | port_error | buffer_full | buffer_empty |
        buffer_timeout | unknown_error).
%%-----------------------------------------------------------------------------
put(Pid, Data) ->
    gen_server:call(Pid, {put, Data}).


%%-----------------------------------------------------------------------------
%% @doc Get data from this connector (pull connection, outport).
%% @spec get(Pid) -> Result
%% where
%%      Pid = pid()
%%      Result = {port_ok | port_error | buffer_full | buffer_empty |
%%          buffer_timeout | unknown_error, Data}
%%      Data = any()
%% @end
-spec(get(Pid::pid()) ->
    Result::{port_ok | port_error | buffer_full | buffer_empty |
        buffer_timeout | unknown_error, Data::any()}).
%%-----------------------------------------------------------------------------
get(Pid) ->
    gen_server:call(Pid, get).


%%=============================================================================
%% Internal functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create an inport connector.
%% @spec create_inport_conn(Type, Prof, Buf) -> Result
%% where
%%      Type = "null" | "push" | "pull"
%%      Prof = #conn_prof{}
%%      Buf = pid() | nil
%%          Destination buffer for data received for inport connections.
%%      Result = {ok, Sup, Worker} | {error, Reason}
%%      Sup = pid()
%%          PID of the connection's top-level supervisor.
%%      Worker = pid()
%%          PID of the connection's worker process.
%%      Reason = any()
%% @end
-spec(create_inport_conn(Type::string(), Prof::#conn_prof{},
        Buf::pid() | nil) ->
    Result::{ok, Sup::pid(), Worker::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
create_inport_conn("null", Prof, _) ->
    start_conn({null_connection, Prof}, nil, nil);
create_inport_conn("push", Prof, Buf) ->
    % Make a worker that sits and waits for put() requests.
    start_conn({put_listener, Prof}, Buf, nil);
create_inport_conn("pull", Prof, Buf) ->
    start_conn({puller, Prof}, Buf, nil).


%%-----------------------------------------------------------------------------
%% @doc Create an outport connector.
%% @spec create_outport_conn(Type, Prof) -> Result
%% where
%%      Type = "null" | "push" | "pull"
%%      Prof = #conn_prof{}
%%      Buf = pid() | nil
%%          Destination buffer for data received for inport connections.
%%      Result = {ok, Sup, Worker} | {error, Reason}
%%      Sup = pid()
%%          PID of the connection's top-level supervisor.
%%      Worker = pid()
%%          PID of the connection's worker process.
%%      Reason = any()
%% @end
-spec(create_outport_conn(Type::string(), Prof::#conn_prof{}) ->
    Result::{ok, Sup::pid(), Worker::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
create_outport_conn("null", Prof) ->
    start_conn({null_connection, Prof}, nil, nil);
create_outport_conn("push", #conn_prof{props=Props}=Prof) ->
    SubType = config:get_value(["dataport", "subscription_type"], "new",
        Props),
    ConnMod = get_conn_mod(SubType),
    case portsvc:make_buf_opts(Props)
        of {ok, BufOpts} ->
            start_conn({ConnMod, Prof}, nil, BufOpts)
         ; {error, Reason} ->
            {error, Reason}
    end;
create_outport_conn("pull", #conn_prof{props=Props}=Prof) ->
    case portsvc:make_buf_opts(Props)
        of {ok, BufOpts} ->
            start_conn({get_listener, Prof}, nil, BufOpts)
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Start a connection child.
%% @spec start_conn(WorkerOpts, DestBuf, ConnBufOpts) -> Result
%% where
%%      WorkerOpts = {WorkerMod, WorkerCfg}
%%      WorkerMod = atom()
%%      WorkerCfg = config()
%%      DestBuf = nil | pid()
%%          A buffer to place received data into.
%%      ConnBufOpts = nil | {BufMod, BufCfg}
%%          Options for the connection's internal buffer.
%%      BufMod = atom()
%%      BufCfg = config()
%%      Result = {ok, Sup, Worker} | {error, Reason}
%%      Sup = pid()
%%          PID of the connection's top-level supervisor.
%%      Worker = pid()
%%          PID of the connection's worker process.
%%      Reason = any()
%% @end
-spec(start_conn(WorkerOpts::{WorkerMod::atom(), Name::string,
            Owner::pid() | nil, WorkerCfg::config()}, DestBuf::nil | pid(),
        ConnBufOpts::{BufMod::atom(), BufCfg::config()}) ->
    Result::{ok, Sup::pid(), Worker::pid()} | {error, Reason::any()}).
%%-----------------------------------------------------------------------------
start_conn(WorkerOpts, DestBuf, ConnBufOpts) ->
    case conn_sup:start_link(WorkerOpts, ConnBufOpts)
        of {ok, S} ->
            Children = supervisor:which_children(S),
            {worker, Worker, worker, _} = lists:keyfind(worker, 1, Children),
            {corba, CORBA, worker, _} = lists:keyfind(corba, 1, Children),
            link_buffer(DestBuf, Worker),
            set_corba_obj(Worker, corba_obj_mgr:get_obj(CORBA)),
            corba_obj_mgr:send_info(CORBA, {set_target, Worker}),
            {ok, S, Worker}
         ; {error, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------------
%% @doc Link a worker to a destination buffer.
%% @spec link_buffer(Buf, pid()) -> ok
%% where
%%      Buf = nil | pid()
%% @end
-spec(link_buffer(Buf::nil | pid(), Worker::pid()) -> ok).
%%-----------------------------------------------------------------------------
link_buffer(nil, _) ->
    ok;
link_buffer(Buf, Worker) ->
    set_buf(Worker, Buf).


%%-----------------------------------------------------------------------------
%% @doc Set the CORBA object providing the interface for a connection.
%% @spec set_corba_obj(Worker, Obj) -> ok
%% where
%%      Worker = pid()
%%      Obj = object_ref()
%% @end
-spec(set_corba_obj(Worker::pid(), Obj::object_ref()) -> ok).
%%-----------------------------------------------------------------------------
set_corba_obj(Worker, Obj) ->
    gen_server:call(Worker, {set_corba_obj, Obj}).


%%-----------------------------------------------------------------------------
%% @doc Convert a subscription type string into a module name.
%% @spec get_conn_mod(SubType) -> push_new | push_flush | push_periodic
%% where
%%      SubType = "new" | "flush" | "periodic"
%% @end
-spec(get_conn_mod(string()) ->
    push_new | push_flush | push_periodic).
%%-----------------------------------------------------------------------------
get_conn_mod("new") ->
    push_new;
get_conn_mod("flush") ->
    push_flush;
get_conn_mod("periodic") ->
    push_periodic.


%%-----------------------------------------------------------------------------
%% @doc Set the buffer for a connection.
%% @spec set_buf(Conn, Pid) -> ok
%% where
%%      Conn = pid()
%%      Pid = pid()
%% @end
-spec(set_buf(Conn::pid(), Pid::pid()) -> ok).
%%-----------------------------------------------------------------------------
set_buf(Conn, Pid) ->
    gen_server:call(Conn, {set_buf, Pid}).


%%=============================================================================
%% Test functions
%%=============================================================================
-ifdef(TEST).

-endif. % TEST

