%% ===========================================================================
%% @doc        Twerl context server.
%% @since      Apr 10, 2010
%% @version    1.0
%% @copyright  2009, Sebastien Merle <s.merle@gmail.com>
%% @author     Sebastien Merle <s.merle@gmail.com>
%% @end
%%
%% Copyright (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   * Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%   * Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%   * Neither the name of "twerl" nor the names of its contributors
%%     may be used to endorse or promote products derived from this software
%%     without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% ===========================================================================

-module(twerl_context).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(context).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_context.specs.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([get_name/1,
         get_service/1,
         get_factory/1,
         register/3]).

%% Other exports
-export([start_supervisor/4,
         start_factory/3,
         start_acceptor/3,
         start_connector/3]).

%% Startup exports
-export([start_link/1, start_link/2]).

%% Overriden base_server callbacks
-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Context's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Context's state
-record(?St, {name,
              service, service_ref, serv_setup = false,
              factory, factory_ref, fact_setup = false,
              fact_sup, fact_sup_ref,
              fact_grp, fact_grp_ref,
              list_sup, list_sup_ref,
              conn_sup, conn_sup_ref,
              proto_sup, proto_sup_ref}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Retrieves the name of the specifed context.

get_name(Context) ->
    gen_server:call(Context, get_name).


%% --------------------------------------------------------------------
%% @doc Retrieves the service process of the specified context.

get_service(Context) ->
    gen_server:call(Context, get_service).


%% --------------------------------------------------------------------
%% @doc Retrieves the factory process of the spcified context.

get_factory(Context) ->
    gen_server:call(Context, get_factory).


%% --------------------------------------------------------------------
%% @doc Registers a class of process to the specified context.
%% Called by generic servers and supervisor, should not be called
%% without knowing what you are doing.
%% Process classes are: 
%%     factory_group
%%     acceptor_sup
%%     connector_sup
%%     factory_sup
%%     proto_sup
%%     service
%%     factory

register(Context, Klass, Pid) ->
    ok = gen_server:call(Context, {register, Klass, Pid}),
    {ok, Pid}.


%% ====================================================================
%% Other Exported Functions, used internally
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts a supervisor and register it to the context.

start_supervisor(Context, Klass, Module, Args) ->
    case apply(Module, start_link, Args) of
        {ok, Pid} -> register(Context, Klass, Pid);
        Any -> Any
    end.


%% --------------------------------------------------------------------
%% @doc Requests the context to start a factory process.

start_factory(Context, FactMod, FactConf) ->
    erlog:log("Starting ~w factory...", [FactMod]),
    Args = {FactMod, FactConf},
    {ok, Constructor} = call_prepare_(Context, factory, Args),
    Constructor().


%% --------------------------------------------------------------------
%% @doc Requests the context to start a acceptor process.

start_acceptor(Context, AcceptMod, AcceptConf) ->
    erlog:log("Starting ~w acceptor...", [AcceptMod]),
    Args = {AcceptMod, AcceptConf},
    {ok, Constructor} = call_prepare_(Context, acceptor, Args),
    Constructor().


%% --------------------------------------------------------------------
%% @doc Requests the context to start a connector process.

start_connector(Context, ConnMod, ConnConf) ->
    erlog:log("Starting ~w connector...", [ConnMod]),
    Args = {ConnMod, ConnConf},
    {ok, Constructor} = call_prepare_(Context, connector, Args),
    Constructor().


%% ====================================================================
%% Startup Functions
%% ====================================================================


%% --------------------------------------------------------------------
%% @doc Starts and links a context process with specified name.

start_link(Name) ->
    start_link(Name, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a context process with specified name
%%      and extra options.

start_link(Name, Options) ->
    erlog:log("Starting context for service ~w...", [Name]),
    gen_server:start_link({local, Name}, ?MODULE, {Name}, Options).


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

init({Name}) ->
    erlog:set_name(Name),
    erlog:info("Initializing context..."),
    {ok, #?St{name = Name}}.

handle_call(get_name, _From, #?St{name = Name} = State) ->
    {reply, {ok, Name}, State};
handle_call(get_service, _From, #?St{service = Service} = State) ->
    {reply, {ok, Service}, State};
handle_call(get_factory, _From, #?St{factory = Factory} = State) ->
    {reply, {ok, Factory}, State};
handle_call({prepare, Klass, Args}, _From, State) ->
    {ok, Fun, NewState} = prepare_(State, Klass, Args),
    {reply, {ok, Fun}, NewState};
handle_call({register, Klass, Ref}, _From, State) ->
    {ok, NewState} = register_(State, Klass, Ref),
    {reply, ok, NewState};
handle_call(Request, From, State) ->
    base_server:handle_call(Request, From, State).

handle_info({'DOWN', MonRef, process, _Obj, Reason}, State) ->
    {ok, NewState} = unregister_(State, MonRef, Reason),
    {noreply, NewState};
handle_info(Msg, State) ->
    base_server:handle_info(Msg, State).

terminate(Reason, _State) ->
    erlog:info("Context terminated: ~p.", [Reason]),
    ok.

code_change(OldVsn, State, _Extra) ->
    erlog:log("Updating context code from version ~w", [OldVsn]),
    {ok, State}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

call_prepare_(Context, Kind, Args) ->
    gen_server:call(Context, {prepare, Kind, Args}).


% Called from inside the context process

prepare_(State, factory, {FactMod, FactConf}) ->
    erlog:log("Preparing ~w factory startup...", [FactMod]),
    #?St{name = Name, fact_grp = FactGroup} = State,
    Context = self(),
    F = fun() ->
          {ok, Sup} = twerl_factory_group:start_factory_sup(FactGroup,
                                                            Name, Context,
                                                            FactMod, FactConf),
          twerl_factory_sup:get_factory(Sup)
        end,
    {ok, F, State};
prepare_(State, acceptor, {AcceptMod, AcceptConf}) ->
    erlog:log("Preparing ~w acceptor startup...", [AcceptMod]),
    #?St{name = Name, list_sup = AcceptorSup} = State,
    Context = self(),
    F = fun() -> twerl_acceptor_sup:start_acceptor(AcceptorSup, Name, Context,
                                                   AcceptMod, AcceptConf) end,
    {ok, F, State};
prepare_(State, connector, {ConnMod, ConnConf}) ->
    erlog:log("Preparing ~w connector startup...", [ConnMod]),
    #?St{name = Name, conn_sup = ConnSup} = State,
    Context = self(),
    F = fun() -> twerl_connector_sup:start_connector(ConnSup, Name, Context,
                                                     ConnMod, ConnConf) end,
    {ok, F, State}.

register_(State, Klass, Pid) ->
    register_(State, Klass, Pid, erlang:monitor(process, Pid)).

register_(State, factory_group, Pid, MonRef) ->
    erlog:log("Factory group ~w registered", [Pid]),
    try_service_setup_(State#?St{fact_grp = Pid, fact_grp_ref = MonRef});
register_(State, acceptor_sup, Pid, MonRef) ->
    erlog:log("Acceptor supervisor ~w registered", [Pid]),
    try_service_setup_(State#?St{list_sup = Pid, list_sup_ref = MonRef});
register_(State, connector_sup, Pid, MonRef) ->
    erlog:log("Connector supervisor ~w registered", [Pid]),
    try_service_setup_(State#?St{conn_sup = Pid, conn_sup_ref = MonRef});
register_(State, service, Pid, MonRef) ->
    erlog:log("Service ~w registered", [Pid]),
    try_service_setup_(State#?St{service = Pid, service_ref = MonRef});
register_(State, factory_sup, Pid, MonRef) ->
    erlog:log("Factory supervisor ~w registered", [Pid]),
    try_factory_setup_(State#?St{fact_sup = Pid, fact_sup_ref = MonRef});
register_(State, proto_sup, Pid, MonRef) ->
    erlog:log("Protocol supervisor ~w registered", [Pid]),
    try_factory_setup_(State#?St{proto_sup = Pid, proto_sup_ref = MonRef});
register_(State, factory, Pid, MonRef) ->
    erlog:log("Factory ~w registered", [Pid]),
    try_factory_setup_(State#?St{factory = Pid, factory_ref = MonRef}).

try_service_setup_(#?St{serv_setup = true} = State) -> {ok, State};
try_service_setup_(#?St{list_sup = undefined} = State) -> {ok, State};
try_service_setup_(#?St{conn_sup = undefined} = State) -> {ok, State};
try_service_setup_(#?St{fact_grp = undefined} = State) -> {ok, State};
try_service_setup_(#?St{service = undefined} = State) -> {ok, State};
try_service_setup_(#?St{service = Service} = State) ->
    erlog:log("Setting up service..."),
    ok = gen_server:cast(Service, setup),
    {ok, State#?St{serv_setup = true}}.

try_factory_setup_(#?St{fact_setup = true} = State) -> {ok, State};
try_factory_setup_(#?St{fact_sup = undefined} = State) -> {ok, State};
try_factory_setup_(#?St{proto_sup = undefined} = State) -> {ok, State};
try_factory_setup_(#?St{factory = undefined} = State) -> {ok, State};
try_factory_setup_(#?St{factory = Factory} = State) ->
    erlog:log("Setting up factory..."),
    ok = gen_server:cast(Factory, setup),
    {ok, State#?St{fact_setup = true}}.

unregister_(#?St{fact_grp_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering factory group process: ~w", [Reason]),
    {ok, State#?St{fact_grp = undefined, fact_grp_ref = undefined}};
unregister_(#?St{list_sup_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering acceptor supervisor process: ~w", [Reason]),
    {ok, State#?St{list_sup = undefined, list_sup_ref = undefined}};
unregister_(#?St{conn_sup_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering connector supervisor process: ~w", [Reason]),
    {ok, State#?St{conn_sup = undefined, conn_sup_ref = undefined}};
unregister_(#?St{service_ref = MonRef} = State, MonRef, normal) ->
    erlog:info("Service process terminated normally, stopping service..."),
    ok = twerl_controller:stop_service_async(State#?St.name),
    {ok, State#?St{service = undefined, service_ref = undefined,
                   serv_setup = false}};
unregister_(#?St{service_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering terminated service process: ~w", [Reason]),
    {ok, State#?St{service = undefined, service_ref = undefined,
                   serv_setup = false}};
unregister_(#?St{fact_sup_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering factory supervisor process: ~w", [Reason]),
    {ok, State#?St{fact_sup = undefined, fact_sup_ref = undefined}};
unregister_(#?St{proto_sup_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering protocol supervisor process: ~w", [Reason]),
    {ok, State#?St{proto_sup = undefined, proto_sup_ref = undefined}};
unregister_(#?St{factory_ref = MonRef} = State, MonRef, normal) ->
    erlog:info("Factory process terminated normally, stopping service..."),
    ok = twerl_controller:stop_service_async(State#?St.name),
    {ok, State#?St{factory = undefined, factory_ref = undefined,
                   fact_setup = false}};
unregister_(#?St{factory_ref = MonRef} = State, MonRef, Reason) ->
    erlog:log("Unregistering terminated factory process: ~w", [Reason]),
    {ok, State#?St{factory = undefined, factory_ref = undefined,
                   fact_setup = false}}.
