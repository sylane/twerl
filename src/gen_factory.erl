%% ===========================================================================
%% @doc        Behaviour module for implementing twerl factories.
%% @since      Mar 23, 2010
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

-module(gen_factory).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(factory).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("gen_factory.specs.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([prepare/1,
         create/3,
         create_protocol/2, create_protocol/4,
         connection_failed/3]).

%% Startup exports
-export([start_link/5, start_link/6]).

%% Inner exports
-export([inner_start_protocol/4]).

%% Behaviour registration export
-export([behaviour_info/1]).

%% Overriden base_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Factory's state record name
-define(St, ?MODULE).

% Factory's sub-state record name
-define(SSt, sub).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Factory's state
-record(?St, {name, state, context, fact_sup, proto_sup, sub_mod, sub_state}).

%% Factory's sub-state when used as a generic factory without sub-module
-record(?SSt, {protocol}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares a factory definition.

prepare(ProtoDef) ->
    {fun ?MODULE:create/3, ?MODULE, [{protocol, ProtoDef}]}.


%% --------------------------------------------------------------------
%% @doc Creates a factory process.
%%      Config is a list of tuples that can contains:
%%        {protocol, twerl_def()}

create(Context, Module, Config) ->
    create_factory_(twerl_utils:as_pid(Context), Module, Config).


%% --------------------------------------------------------------------
%% @doc Requests a factory to create an instance of the default protocol.

create_protocol(Factory, Args) ->
    gen_server:call(Factory, {create_protocol, Args}).


%% --------------------------------------------------------------------
%% @doc Requests a factory to create the specified protocol.

create_protocol(Factory, ProtoDef, Parent, Args) ->
    Msg = {create_protocol, ProtoDef, Parent, Args},
    gen_server:call(Factory, Msg).


%% --------------------------------------------------------------------
%% @doc Informs the factory the connection intent failed.

connection_failed(Factory, Peer, Reason) ->
    gen_server:cast(Factory, {connection_failed, Peer, Reason}).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a factory process.

start_link(ServName, Context, Module, Config, FactSup) ->
    start_link(ServName, Context, Module, Config, FactSup, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a factory process with extra options.

start_link(ServName, Context, Module, Config, FactSup, Options) ->
    erlog:log("Starting ~w factory for service ~w...", [Module, ServName]),
    Params = {ServName, twerl_utils:as_pid(Context), Module, Config, FactSup},
    gen_server:start_link(?MODULE, Params, Options).


%% ====================================================================
%% Behaviour Registration Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Specifies the gen_factory behaviour.

behaviour_info(callbacks) -> [{init, 1},
                              {create_protocol, 3},
                              {create_protocol, 5},
                              {connection_failed, 4},
                              {terminate, 3},
                              {code_change, 3}];
behaviour_info(_Other)    -> undefined.


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

init({ServName, Context, Module, Config, FactSup}) ->
    % Can't call context during initialization, it would dead-lock.
    erlog:set_name(ServName),
    BaseState = #?St{state = init, name = ServName, context = Context,
                     fact_sup = FactSup, sub_mod = Module},
    {ok, State, RemArgs} = init_state_(BaseState, Config),
    case init_factory_(State, RemArgs) of
        {ok, NewState} ->
            {ok, _} = twerl_context:register(Context, factory, self()),
            {ok, NewState#?St{state = setup}};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call({create_protocol, Args}, _From, State) ->
    {ok, Proto, NewState} = create_protocol_(State, Args),
    {reply, {ok, Proto}, NewState};
handle_call({create_protocol, ProtoDef, Parent, Args}, _From, State) ->
    {ok, Proto, NewState} = create_protocol_(State, ProtoDef, Parent, Args),
    {reply, {ok, Proto}, NewState};
handle_call(Request, From, State) ->
    base_server:handle_call(Request, From, State).

handle_cast(setup, #?St{state = setup} = State) ->
    {ok, NewState} = setup_factory_(State),
    {noreply, NewState};
handle_cast({connection_failed, Peer, Reason}, State) ->
    case connection_failed_(State, Peer, Reason) of
        {ok, NewState} -> {noreply, NewState};
        {stop, NewReason, NewState} -> {stop, NewReason, NewState}
    end;
handle_cast(Msg, State) ->
    base_server:handle_cast(Msg, State).

terminate(Reason, State) ->
    terminate_factory_(State, Reason).

code_change(OldVsn, State, Extra) ->
    upgrade_factory_(State, OldVsn, Extra).


%% ====================================================================
%% Inner Functions, only to be called by sub-modules
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts a new protocol process.

inner_start_protocol(ProtoDef, Parent, Args, State) ->
    #?St{context = Ctx, proto_sup = Sup} = State,
    {Constructor, ProtoMod, ProtoConf} = ProtoDef,
    Constructor(Ctx, self(), Sup, Parent, Args, ProtoMod, ProtoConf).    


%% ====================================================================
%% Internal Functions
%% ====================================================================

% Called by any process

create_factory_(Context, Module, Config) ->
    erlog:log("Creating ~w factory...", [Module]),
    {ok, _FactSup} = twerl_context:start_factory(Context, Module, Config),
    twerl_context:get_factory(Context).

% Called from inside the acceptor process

init_state_(#?St{sub_mod = ?MODULE} = St, Config) ->
    erlog:debug("Configuring generic factory with ~p...", [Config]),
    init_state_(St#?St{sub_state = #?SSt{}}, [], Config);
init_state_(St, Config) ->
    erlog:log("Configuring generic factory with ~p...", [Config]),
    init_state_(St, [], Config).

init_state_(St, Acc, []) ->
   {ok, St, lists:reverse(Acc)};
init_state_(#?St{sub_mod = ?MODULE, sub_state = SSt} = St, Acc,
           [{protocol, ProtoDef} |T]) ->
    NewSSt = SSt#?SSt{protocol = ProtoDef},
    init_state_(St#?St{sub_state = NewSSt}, Acc, T);
init_state_(St, Acc, [Other |T]) ->
    init_state_(St, [Other |Acc], T).

init_factory_(#?St{sub_mod = ?MODULE} = St, []) ->
    erlog:info("Initializing generic factory..."),
    {ok, St};
init_factory_(#?St{sub_mod = SMod} = St, RemArgs) ->
    erlog:debug("Initializing generic factory..."),
    case SMod:init(RemArgs) of
        {ok, ModSt} ->
            {ok, St#?St{sub_state = ModSt}};
        {stop, _} = Result -> Result
    end.

setup_factory_(#?St{fact_sup = Sup, sub_mod = ?MODULE} = St) ->
    erlog:debug("Setting up generic factory..."),
    {ok, ProtoSup} = twerl_factory_sup:get_protocol_sup(Sup),
    {ok, St#?St{state = normal, proto_sup = ProtoSup}};
setup_factory_(#?St{sub_mod = SMod, sub_state = SSt} = St) ->
    erlog:log("Setting up generic service..."),
    {ok, SSt2} = SMod:setup(SSt, St),
    {ok, St#?St{state = normal, sub_state = SSt2}}.

terminate_factory_(#?St{sub_mod = ?MODULE}, shutdown) ->
    erlog:info("Generic factory shutdown."),
    ok;
terminate_factory_(#?St{sub_mod = ?MODULE}, normal) ->
    erlog:info("Generic factory terminated normaly."),
    ok;
terminate_factory_(#?St{sub_mod = ?MODULE}, Reason) ->
    erlog:warn("Generic factory terminated: ~p.", [Reason]),
    ok;
terminate_factory_(#?St{sub_mod = SMod, sub_state = SSt} = St, Reason) ->
    erlog:debug("Generic factory terminated: ~p.", [Reason]),
    SMod:terminate(Reason, SSt, St).

upgrade_factory_(#?St{sub_mod = ?MODULE} = St, OldVsn, _Extra) ->
    erlog:info("Updating generic factory code from version ~w...", [OldVsn]),
    {ok, St};
upgrade_factory_(#?St{sub_mod = SMod} = St, OldVsn, Extra) ->
    erlog:debug("Updating generic factory code from version ~w...", [OldVsn]),
    {ok, SSt} = SMod:code_change(OldVsn, St#?St.sub_state, Extra),
    {ok, St#?St{sub_state = SSt}}.

create_protocol_(#?St{sub_mod = ?MODULE} = St, Args) ->
    #?SSt{protocol = ProtoDef} = St#?St.sub_state,
    {ok, Proto} = inner_start_protocol(ProtoDef, undefined, Args, St),
    {ok, Proto, St};
create_protocol_(#?St{sub_mod = SMod} = St, Args) ->
    erlog:log("Delegating protocol creation to ~w...", [SMod]),
    case SMod:create_protocol(Args, St#?St.sub_state, St) of
        {ok, Proto, SSt} -> {ok, Proto, St#?St{sub_state = SSt}}
    end.

create_protocol_(#?St{sub_mod = ?MODULE} = St, ProtoDef, Parent, Args) ->
    {ok, Proto} = inner_start_protocol(ProtoDef, Parent, Args, St),
    {ok, Proto, St};
create_protocol_(#?St{sub_mod = SMod} = St, ProtoDef, Parent, Args) ->
    erlog:log("Delegating protocol creation to ~w...", [SMod]),
    case SMod:create_protocol(ProtoDef, Parent, Args, St#?St.sub_state, St) of
        {ok, Proto, SSt} -> {ok, Proto, St#?St{sub_state = SSt}}
    end.

connection_failed_(#?St{sub_mod = ?MODULE} = St, Peer, Reason) ->
    erlog:warn("Failed to connect to ~s: ~w", [format:peer(Peer), Reason]),
    {stop, normal, St};
connection_failed_(#?St{sub_mod = SMod} = St, Peer, Reason) ->
    erlog:log("Delegating conection failure to ~w...", [SMod]),
    case SMod:connection_failed(Peer, Reason, St#?St.sub_state, St) of
        {ok, SSt} -> {ok, St#?St{sub_state = SSt}};
        {stop, NewReason, SSt} -> {stop, NewReason, St#?St{sub_state = SSt}}
    end.
