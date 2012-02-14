%% ===========================================================================
%% @doc        Behaviour module for implementing twerl connectors.
%% @since      May 28, 2010
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

-module(gen_connector).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(connector).

%TODO: Remove duplicated code between gen_acceptor and gen_connector


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_types.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([create/3]).

%% Startup exports
-export([start_link/4, start_link/5]).

%% Inner exports
-export([inner_create_protocol/1,
         inner_connect_protocol/5,
         inner_connection_failed/3]).

%% Behaviour registration export
-export([behaviour_info/1]).

%% Overridden base_server callbacks
-export([init/1,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Acceptor's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Acceptor's state
-record(?St, {context, args, sub_mod, sub_state}).


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type state() :: #gen_connector{}.


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Requests the context to start a connector process with specified
%%      arguments using specified callback module and configuration.
%%      Config is a list of tuples that can contains:
%%        {args, any()} -> Arguments passed to the factory

create(Context, Module, Config) ->
    create_connector_(twerl_utils:as_pid(Context), Module, Config).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a connector process.

start_link(ServName, Context, Module, Config) ->
    start_link(ServName, Context, Module, Config, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a connector process with extra options.

start_link(ServName, Context, Module, Config, Options) ->
    erlog:log("Starting ~w connector for service ~w...", [Module, ServName]),
    Params = {ServName, twerl_utils:as_pid(Context), Module, Config},
    gen_server:start_link(?MODULE, Params, Options).


%% ====================================================================
%% Behaviour Registration Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Specifies the gen_connector behaviour.

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) -> [{init, 1},
                              {setup, 2},
                              {handle_message, 3},
                              {connect, 2},
                              {terminate, 3},
                              {code_change, 3}];
behaviour_info(_Other)    -> undefined.


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc gen_server beaviour init callback.

-spec init({ServName::twerl_service_name(), Context::pid(),
            Module::module(), Config::list()}) ->
          {ok, State::state()}
        | {stop, Reason::stop_reason()}.

init({ServName, Context, Module, Config}) ->
    % Can't call context during initialization, it would dead-lock.
    erlog:set_name(ServName),
    BaseState = #?St{context = Context, sub_mod = Module},
    {ok, State, RemArgs} = initialize_(BaseState, Config),
    init_connector_(State, RemArgs).


%% --------------------------------------------------------------------
%% @doc gen_server beaviour handle_cast callback.

-spec handle_cast(Msg::term(), State::state()) ->
          {noreply, State::state()}
        | {stop, Reason::stop_reason(), NewState::state()}.

handle_cast(setup, State) ->
    case setup_connector_(State) of
        {ok, NewState} -> {noreply, NewState};
        {stop, _Reason, _NewState} = Response -> Response
    end;
handle_cast(Msg, State) ->
    base_server:handle_cast(Msg, State).


%% --------------------------------------------------------------------
%% @doc gen_server beaviour handle_info callback.

-spec handle_info(Msg::term(), State::state()) ->
          {noreply, State::state()}.

handle_info(Msg, State) ->
    delegate_info_(State, Msg).


%% --------------------------------------------------------------------
%% @doc gen_server beaviour terminate callback.

-spec terminate(Reason::terminate_reason(), State::state()) -> any().

terminate(Reason, State) ->
    terminate_connector_(State, Reason).


%% --------------------------------------------------------------------
%% @doc gen_server beaviour code_change callback.

-spec code_change(OldVsn::term(), State::state(), Extra::term()) ->
        {ok, NewState::state()}.

code_change(OldVsn, State, Extra) ->
    upgrade_connector_(State, OldVsn, Extra).


%% ====================================================================
%% Inner Functions, Only called by sub-modules
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Request the factory to create a protocol.

inner_create_protocol(#?St{context = Ctx} = State) ->
    erlog:log("Building ~w connector protocol...", [State#?St.sub_mod]),
    %TODO: do not request the factory every time.
    {ok, Fact} = twerl_context:get_factory(Ctx),
    erlog:log("Delegating protocol creation to factory..."),
    {ok, Proto} = gen_factory:create_protocol(Fact, State#?St.args),
    {ok, Proto}.


%% --------------------------------------------------------------------
%% @doc Connects a previously created protocol.

inner_connect_protocol(Peer, Proto, Prod, Cons, State) ->
    erlog:log("Connecting ~w connector protocol to ~s...",
              [State#?St.sub_mod, format:peer(Peer)]),
    gen_protocol:connect(Proto, Peer, Prod, Cons).


%% --------------------------------------------------------------------
%% @doc Informs the factory that the connection failed.

inner_connection_failed(Peer, Reason, #?St{context = Ctx} = State) ->
    erlog:log("Connector ~w failed to connect to ~s: ~p...",
              [State#?St.sub_mod, format:peer(Peer), Reason]),
    %TODO: do not request the factory every time.
    {ok, Fact} = twerl_context:get_factory(Ctx),
    ok = gen_factory:connection_failed(Fact, Peer, Reason),
    %TODO: What should it return ?
    ok.


%% ====================================================================
%% Internal Functions
%% ====================================================================

% Called by any process

create_connector_(Context, Module, Config) ->
    erlog:log("Creating ~w connector...", [Module]),
    twerl_context:start_connector(Context, Module, Config).

% Called from inside the connector process

initialize_(St, Config) ->
    erlog:log("Configuring generic connector with ~p...", [Config]),
    initialize_(St, [], Config).

initialize_(St, Acc, []) ->
    {ok, St, lists:reverse(Acc)};
initialize_(St, Acc, [{args, Args} |Rest]) ->
    initialize_(St#?St{args = Args}, Acc, Rest);
initialize_(St, Acc, [Opt |Rest]) ->
    initialize_(St, [Opt |Acc], Rest).

init_connector_(#?St{sub_mod = SMod} = St, RemArgs) ->
    erlog:debug("Initializing generic connector"),
    case SMod:init(RemArgs) of
        {ok, SSt} ->
            ok = gen_server:cast(self(), setup),
            {ok, St#?St{sub_state = SSt}};
        {stop, _} = Result -> Result
    end.

setup_connector_(#?St{sub_mod = SMod, sub_state = SSt} = St) ->
    {ok, NewSSt} = SMod:setup(SSt, St),
    delegate_connect_(St#?St{sub_state = NewSSt}).

delegate_connect_(#?St{sub_mod = SMod} = St) ->
    case SMod:connect(St#?St.sub_state, St) of
        {ok, SSt} ->
            {ok, St#?St{sub_state = SSt}};
        {stop, Reason, SSt} ->
            {stop, Reason, St#?St{sub_state = SSt}}
    end.

delegate_info_(#?St{sub_mod = SMod, sub_state = SSt} = St, Msg) ->
    {ok, NewSSt} = SMod:handle_message(Msg, SSt, St),
    {noreply, St#?St{sub_state =  NewSSt}}.

terminate_connector_(#?St{sub_mod = SMod, sub_state = SSt} = St, Reason) ->
    SMod:terminate(Reason, SSt, St).

upgrade_connector_(#?St{sub_mod = SMod} = St, OldVsn, Extra) ->
    erlog:debug("Updating generic connector code from version ~w...", [OldVsn]),
    {ok, SSt} = SMod:code_change(OldVsn, St#?St.sub_state, Extra),
    {ok, St#?St{sub_state = SSt}}.
