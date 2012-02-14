%% ===========================================================================
%% @doc        Behaviour module for implementing twerl services.
%% @since      Apr 11, 2010
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

-module(gen_service).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(service).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("gen_service.specs.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([prepare_server/2,
         prepare_client/2,
         create/3,
         launch/3,
         get_service/1]).

%% Startup exports
-export([start_link/4, start_link/5]).

%% Behaviour registration export
-export([behaviour_info/1]).

%% Overridden base_server callbacks
-export([init/1,
         handle_cast/2,
         terminate/2,
         code_change/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Service's state record name
-define(St, ?MODULE).

% Generic service's sub-state record name
-define(SSt, sub).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Service's state
-record(?St, {state, context, sub_mod, sub_state}).

%% Service's sub-state when used as a generic service without sub-module
-record(?SSt, {factory, acceptors = [], connectors = []}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares a generic server service definition.

prepare_server(LisDef, FactDef) ->
    {fun gen_service:create/3, ?MODULE,
     [{factory, FactDef}, {acceptor, LisDef}]}.


%% --------------------------------------------------------------------
%% @doc Prepares a generic client service definition.

prepare_client(ConnDef, FactDef) ->
    {fun gen_service:create/3, ?MODULE,
     [{factory, FactDef}, {connector, ConnDef}]}.


%% --------------------------------------------------------------------
%% @doc Launch a service supervisor detached from twerl supervisation tree.
%% Could be called by an application custom supervisor.
%% Config is a list of tuples that can contains:
%%     {factory, twerl_def()}
%%     {acceptor, twerl_def()}
%%     {connector, twerl_def()}}

launch(Name, ServMod, ServConf) ->
    case whereis(Name) of
        undefined ->
            erlog:log("Launching ~w service ~w...", [ServMod, Name]),
            twerl_context_sup:start_link(Name, ServMod, ServConf);
        Pid -> {error, {already_started, Pid}}
    end.


%% --------------------------------------------------------------------
%% @doc Create a service supervisor for the specified callback module
%% and configuration.
%% Config is a list of tuples that can contains:
%%     {factory, twerl_def()}
%%     {acceptor, twerl_def()}
%%     {connector, twerl_def()}}

create(Name, Module, Config) ->
    case whereis(Name) of
        undefined ->
            erlog:log("Creating ~w service ~w...", [Module, Name]),
            twerl_context_group:start_context_sup(Name, Module, Config);
        Pid ->  {error, {already_started, Pid}}
    end.


%% --------------------------------------------------------------------
%% @doc Returns the service process from the context supervisor.

get_service(CtxSup) ->
    {ok, ServSup} = twerl_context_sup:get_service_sup(CtxSup),
    twerl_service_sup:get_service(ServSup).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a service process.

start_link(Name, Context, Module, Config) ->
    start_link(Name, Context, Module, Config, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a service process with extra options.

start_link(Name, Context, Module, Config, Options) ->
    erlog:log("Starting ~w service...", [Module]),
    Params = {Name, Context, Module, Config},
    gen_server:start_link(?MODULE, Params, Options).


%% ====================================================================
%% Behaviour Registration Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Specifies the gen_service behaviour.

behaviour_info(callbacks) -> [{init, 1},
                              {setup, 2},
                              {terminate, 3},
                              {code_change, 3}];
behaviour_info(_Other)    -> undefined.


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

init({Name, Context, Module, Config}) ->
    % Can't call context during initialization, it would dead-lock.
    erlog:set_name(Name),
    BaseState = #?St{context = Context, state = init, sub_mod = Module},
    {ok, State, RemArgs} = init_state_(BaseState, Config),
    case init_service_(State, RemArgs) of
        {error, Reason} -> {stop, Reason};
        {ok, NewState} ->
            {ok, _} = twerl_context:register(Context, service, self()),
            {ok, NewState#?St{state = setup}}
    end.

handle_cast(setup, #?St{state = setup} = State) ->
    case setup_service_(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState}
    end;
handle_cast(Msg, State) ->
    base_server:handle_cast(Msg, State).

terminate(Reason, State) ->
    terminate_service_(State, Reason).

code_change(OldVsn, State, Extra) ->
    update_service_(State, OldVsn, Extra).


%% ====================================================================
%% Internal Functions
%% ====================================================================

% Called from inside the service process

init_state_(#?St{sub_mod = ?MODULE} = St, Config) ->
    erlog:debug("Configuring generic service with ~p...", [Config]),
    init_state_(St#?St{sub_state = #?SSt{}}, [], Config);
init_state_(St, Config) ->
    erlog:log("Configuring generic service with ~p...", [Config]),
    init_state_(St, [], Config).

init_state_(St, Acc, []) ->
    {ok, St, lists:reverse(Acc)};
init_state_(#?St{sub_mod = ?MODULE, sub_state = SSt} = St, Acc,
           [{factory, FactDef} |T]) ->
    SSt2 = SSt#?SSt{factory = FactDef},
    init_state_(St#?St{sub_state = SSt2}, Acc, T);
init_state_(#?St{sub_mod = ?MODULE, sub_state = SSt} = St, Acc,
           [{acceptor, LisDef} |T]) ->
    Acceptors = [LisDef |SSt#?SSt.acceptors],
    init_state_(St#?St{sub_state = SSt#?SSt{acceptors = Acceptors}}, Acc, T);
init_state_(#?St{sub_mod = ?MODULE, sub_state = SSt} = St, Acc,
           [{connector, ConnDef} |T]) ->
    Connectors = [ConnDef |SSt#?SSt.connectors],
    init_state_(St#?St{sub_state = SSt#?SSt{connectors = Connectors}}, Acc, T);
init_state_(St, Acc, [Other |T]) ->
    init_state_(St, [Other |Acc], T).

init_service_(#?St{sub_mod = ?MODULE} = St, []) ->
    erlog:info("Initializing generic service..."),
    {ok, St};
init_service_(#?St{sub_mod = ?MODULE}, Unexpected) ->
    erlang:error(unexpected_arguments, [Unexpected]);
init_service_(#?St{sub_mod = SMod} = St, RemArgs) ->
    erlog:debug("Initializing generic service..."),
    case SMod:init(RemArgs) of
        {ok, ModSt} ->
            {ok, St#?St{sub_state = ModSt}};
        {stop, Reason} -> {error, Reason}
    end.

setup_service_(#?St{sub_mod = ?MODULE, sub_state = SSt} = St) ->
    erlog:debug("Setting up generic service..."),
    #?SSt{factory = FactDef,
          acceptors = LisDefs,
          connectors = ConnDefs} = SSt,
    Funs = [fun(FSt) -> setup_factory_(FSt, FactDef) end,
            fun(FSt) -> setup_acceptors_(FSt, LisDefs) end,
            fun(FSt) -> setup_connectors_(FSt, ConnDefs) end],
    exec_setup_(St, Funs);
setup_service_(#?St{sub_mod = SMod, sub_state = SSt} = St) ->
    erlog:log("Setting up generic service..."),
    case SMod:setup(SSt, St) of
        {ok, SSt2} -> setup_succeed_(St, SSt2);
        {error, Reason} -> setup_failed_(St, Reason)
    end.

exec_setup_(St, []) ->
    setup_succeed_(St, undefined);
exec_setup_(St, [Fun |Funs]) ->
    case Fun(St) of
        {ok, NewSt} -> exec_setup_(NewSt, Funs);
        {error, Reason} -> setup_failed_(St, Reason)
    end.

setup_factory_(St, undefined) ->
    {ok, St};
setup_factory_(#?St{context = Ctx} = St, FactDef) ->
    {Constructor, Module, Config} = FactDef,
    case Constructor(Ctx, Module, Config) of
        {ok, _Fact} -> {ok, St};
        {error, _} = Error -> Error
    end.

setup_acceptors_(St, []) ->
    {ok, St};
setup_acceptors_(#?St{context = Ctx} = St, [LisDef |LisDefs]) ->
    {Constructor, Module, Config} = LisDef,
    case Constructor(Ctx, Module, Config) of
        {ok, _Acceptor} -> setup_acceptors_(St, LisDefs);
        {error, _} = Error -> Error
    end.

setup_connectors_(St, []) ->
    {ok, St};
setup_connectors_(#?St{context = Ctx} = St, [ConnDef |ConnDefs]) ->
    {Constructor, Module, Config} = ConnDef,
    case Constructor(Ctx, Module, Config) of
        {ok, _Connector} -> setup_connectors_(St, ConnDefs);
        {error, _} = Error -> Error
    end.

setup_succeed_(St, SSt) ->
    {ok, St#?St{state = normal, sub_state = SSt}}.

setup_failed_(St, Reason) ->
    {error, Reason, St#?St{state = error}}.

terminate_service_(#?St{sub_mod = ?MODULE}, shutdown) ->
    erlog:info("Generic service shutdown."),
    ok;
terminate_service_(#?St{sub_mod = ?MODULE}, normal) ->
    erlog:info("Generic service terminated normaly."),
    ok;
terminate_service_(#?St{sub_mod = ?MODULE}, Reason) ->
    erlog:warn("Generic service terminated: ~p.", [Reason]),
    ok;
terminate_service_(#?St{sub_mod = SMod, sub_state = SSt} = St, Reason) ->
    erlog:debug("Generic service terminated: ~p.", [Reason]),
    SMod:terminate(Reason, SSt, St).

update_service_(#?St{sub_mod = ?MODULE} = St, OldVsn, _Extra) ->
    erlog:info("Updating generic service code from version ~w...", [OldVsn]),
    {ok, St};
update_service_(#?St{sub_mod = SMod} = St, OldVsn, Extra) ->
    erlog:debug("Updating generic service code from version ~w...", [OldVsn]),
    {ok, SSt} = SMod:code_change(OldVsn, St#?St.sub_state, Extra),
    {ok, St#?St{sub_state = SSt}}.
