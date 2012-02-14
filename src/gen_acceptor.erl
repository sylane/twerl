%% ===========================================================================
%% @doc        Behaviour module for implementing twerl acceptors.
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

-module(gen_acceptor).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(acceptor).


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
         inner_connect_protocol/5]).

%% Behaviour registration export
-export([behaviour_info/1]).

%% Overridden base_server callbacks
-export([init/1,
         handle_call/3,
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
-record(?St, {state, context, args, sub_mod, sub_state}).


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type state() :: #gen_acceptor{}.


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Requests the context to start a acceptor process with specified
%%      arguments using specified callback module and configuration.
%%      Config is a list of tuples that can contains:
%%        {args, any()} -> Arguments passed to the factory
%% --------------------------------------------------------------------
create(Context, Module, Config) ->
    create_acceptor_(twerl_utils:as_pid(Context), Module, Config).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a acceptor process.

start_link(ServName, Context, Module, Config) ->
    start_link(ServName, Context, Module, Config, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a acceptor process with extra options.

start_link(ServName, Context, Module, Config, Options) ->
    erlog:log("Starting ~w acceptor for service ~w...", [Module, ServName]),
    Params = {ServName, twerl_utils:as_pid(Context), Module, Config},
    gen_server:start_link(?MODULE, Params, Options).


%% ====================================================================
%% Behaviour Registration Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Specifies the gen_acceptor behaviour.

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) -> [{init, 1},
                              {handle_message, 3},
                              {start_accepting, 2},
                              {pause_accepting, 2},
                              {resume_accepting, 2},
                              {stop_accepting, 2},
                              {terminate, 3},
                              {code_change, 3}];
behaviour_info(_Other)    -> undefined.


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================


%% --------------------------------------------------------------------
%% @doc gen_server init callback.

-spec init({ServName::twerl_service_name(), Context::pid(),
            Module::module(), Config::list()}) ->
        {ok, State::state()}
        | {stop, Reason::stop_reason()}.

init({ServName, Context, Module, Config}) ->
    % Can't call context during initialization, it would dead-lock.
    erlog:set_name(ServName),
    BaseState = #?St{context = Context, state = init, sub_mod = Module},
    {ok, State, RemArgs} = initialize_(BaseState, Config),
    init_acceptor_(State, RemArgs).


%% --------------------------------------------------------------------
%% @doc gen_server handle_call callback.

-spec handle_call(Request::term(), From::term(), State::state()) ->
          {reply, Reply::term(), State::state()}
        | {stop, Reason::stop_reason(), Reply::term(), State::state()}.

handle_call(pause_accepting, _From, State) ->
    case pause_accepting_(State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason, NewState} ->
            {stop, Reason, {error, Reason}, NewState}
    end;
handle_call(resume_accepting, _From, State) ->
    case resume_accepting_(State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason, NewState} ->
            {stop, Reason, {error, Reason}, NewState}
    end;
handle_call(Request, From, State) ->
    base_server:handle_call(Request, From, State).


%% --------------------------------------------------------------------
%% @doc gen_server handle_cast callback.

-spec handle_cast(Msg::term(), State::state()) ->
        {noreply, State::state()}
        | {stop, Reason::stop_reason(), State::state()}.

handle_cast(setup, #?St{state = setup} = State) ->
    case setup_acceptor_(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState}
    end;
handle_cast(Msg, State) ->
    base_server:handle_cast(Msg, State).


%% --------------------------------------------------------------------
%% @doc gen_server handle_info callback.

-spec handle_info(Msg::term(), State::state()) ->
        {noreply, State::state()}
        | {stop, Reason::stop_reason(), State::state()}.

handle_info(Msg, State) ->
    delegate_info_(State, Msg).


%% --------------------------------------------------------------------
%% @doc gen_server terminate callback.

-spec terminate(Reason::terminate_reason(), State::state()) -> any().

terminate(Reason, State) ->
    terminate_acceptor_(State, Reason).


%% --------------------------------------------------------------------
%% @doc gen_server code_change callback.

-spec code_change(OldVsn::term(), State::state(), Extra::term()) ->
        {ok, NewState::state()}.

code_change(OldVsn, State, Extra) ->
    upgrade_acceptor_(State, OldVsn, Extra).


%% ====================================================================
%% Inner Functions, Only called by sub-modules
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc TODO: Add description of function gen_acceptor:inner_create_protocol/1

inner_create_protocol(#?St{context = Ctx} = State) ->
    erlog:log("Building ~w acceptor protocol...", [State#?St.sub_mod]),
    %TODO: do not request the factory every time.
    {ok, Fact} = twerl_context:get_factory(Ctx),
    erlog:log("Delegating protocol creation to factory..."),
    {ok, Proto} = gen_factory:create_protocol(Fact, State#?St.args),
    {ok, Proto}.


%% --------------------------------------------------------------------
%% @doc TODO: Add description of function gen_acceptor:inner_connect_protocol/5

inner_connect_protocol(Peer, Proto, Prod, Cons, State) ->
    erlog:log("Connecting ~w acceptor protocol to ~s...",
              [State#?St.sub_mod, format:peer(Peer)]),
    gen_protocol:connect(Proto, Peer, Prod, Cons).


%% ====================================================================
%% Internal Functions
%% ====================================================================

% Called by any process

create_acceptor_(Context, Module, Config) ->
    erlog:log("Creating ~w acceptor...", [Module]),
    twerl_context:start_acceptor(Context, Module, Config).

% Called from inside the acceptor process

initialize_(St, Config) ->
    erlog:log("Configuring generic acceptor with ~p...", [Config]),
    initialize_(St, [], Config).

initialize_(St, Acc, []) ->
    {ok, St, lists:reverse(Acc)};
initialize_(St, Acc, [{args, Args} |Rest]) ->
    initialize_(St#?St{args = Args}, Acc, Rest);
initialize_(St, Acc, [Opt |Rest]) ->
    initialize_(St, [Opt |Acc], Rest).

init_acceptor_(#?St{sub_mod = SMod} = St, RemArgs) ->
    process_flag(trap_exit, true),
    erlog:debug("Initializing generic acceptor"),
    case SMod:init(RemArgs) of
        {ok, SSt} ->
            ok = gen_server:cast(self(), setup),
            {ok, St#?St{state = setup, sub_state = SSt}};
        {stop, _} = Result -> Result
    end.

setup_acceptor_(#?St{sub_mod = SMod, sub_state = SSt} = St) ->
    case SMod:setup(SSt, St) of
        {ok, NewSSt} -> start_accepting_(St#?St{sub_state = NewSSt});
        {error, Reason} -> {error, Reason, St#?St{state = error}}
    end.

start_accepting_(#?St{state = setup, sub_mod = SMod} = St) ->
    case SMod:start_accepting(St#?St.sub_state, St) of
        {ok, SSt} ->
            {ok, St#?St{state = accepting, sub_state = SSt}};
        {error, Reason} ->
            {error, Reason, St#?St{state = error}}
    end.

pause_accepting_(#?St{state = accepting, sub_mod = SMod} = St) ->
    case SMod:pause_accepting(St#?St.sub_state, St) of
        {ok, SSt} ->
            {ok, St#?St{state = paused, sub_state = SSt}};
        {error, Reason} ->
            {error, Reason, St#?St{state = error}}
    end.

resume_accepting_(#?St{state = paused, sub_mod = SMod} = St) ->
    case SMod:resume_accepting(St#?St.sub_state, St) of
        {ok, SSt} ->
            {ok, St#?St{state = accepting, sub_state = SSt}};
        {error, Reason} ->
            {error, Reason, St#?St{state = error}}
    end.

stop_accepting_(#?St{sub_mod = SMod} = St) ->
    case SMod:stop_accepting(St#?St.sub_state, St) of
        {ok, SSt} ->
            {ok, St#?St{state = stopped, sub_state = SSt}};
        {error, Reason} ->
            {error, Reason, St#?St{state = error}}
    end.

delegate_info_(#?St{sub_mod = SMod, sub_state = SSt} = St, Msg) ->
    case SMod:handle_message(Msg, SSt, St) of
        {ok, NewSSt} -> {noreply, St#?St{sub_state =  NewSSt}};
        {error, Reason} -> {stop, Reason, St#?St{state = error}}
    end.

terminate_acceptor_(St, Reason) ->
    erlog:debug("Generic acceptor terminated: ~p.", [Reason]),
    case stop_accepting_(St) of
        {ok, NewSt} ->
            delegate_terminate_(NewSt, Reason);
        {error, OtherReason, NewSt} ->
            % Don't fail here, sub-module's terminate must be called
            erlog:warn("Stopping -W acceptor failed: ~p.",
                       [NewSt#?St.sub_mod, OtherReason]),
            delegate_terminate_(NewSt, Reason)
    end.

delegate_terminate_(#?St{sub_mod = SMod, sub_state = SSt} = St, Reason) ->
    SMod:terminate(Reason, SSt, St).

upgrade_acceptor_(#?St{sub_mod = SMod} = St, OldVsn, Extra) ->
    erlog:debug("Updating generic acceptor code from version ~w...", [OldVsn]),
    {ok, SSt} = SMod:code_change(OldVsn, St#?St.sub_state, Extra),
    {ok, St#?St{sub_state = SSt}}.
