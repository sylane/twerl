%% ===========================================================================
%% @doc        Behaviour module for implementing twerl protocols.
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

-module(gen_protocol).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(protocol).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("gen_protocol.hrl").

-include("twerl_pipeline.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([create/7,
         connect/4,
         detach/1]).

%% Startup exports
-export([start_link/6, start_link/7]).

%% Behaviour registration export
-export([behaviour_info/1]).

%% Overriden base_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a protocol process using specified supervisor.

create(Context, Factory, Supervisor, Parent, Args, Module, Config) ->
    {ok, _} = twerl_protocol_sup:start_protocol(Supervisor,
                                                Context, Factory,
                                                Parent, Args,
                                                Module, Config).


%% --------------------------------------------------------------------
%% @doc Informs protocol the connection has been established.

connect(Protocol, Peer, InStages, OutStages) ->
    gen_server:cast(Protocol, {connect, {self(), Peer, InStages, OutStages}}).


%% --------------------------------------------------------------------
%% @doc Informs protocol it got detached from its parent.

detach(Protocol) ->
    gen_server:cast(Protocol, {detached, self()}).

%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a protocol with specified context and factory
%% using specified module for callbacks.

start_link(Context, Factory, Parent, Args, Module, Config) ->
    start_link(Context, Factory, Parent, Args, Module, Config, []).


%% --------------------------------------------------------------------
%% @doc Starts and links a protocol with specified context and factory
%% using specified module for callbacks with extra start_link options.

start_link(Context, Factory, Parent, Args, Module, Config, Options) ->
    erlog:log("Starting ~w protocol...", [Module]),
    Params = {Context, Factory, Parent, Args, Module, Config},
    gen_server:start_link(?MODULE, Params, Options).


%% ====================================================================
%% Behaviour Registration Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Defines gen_protocol behaviours.

behaviour_info(callbacks) -> [{init, 2},
                              {setup, 2},
                              {connection_made, 3},
                              {prepare_input, 1},
                              {prepare_output, 1},
                              {input_bound, 3},
                              {output_bound, 3},
                              {handle_call, 4},
                              {handle_cast, 3},
                              {handle_info, 3},
                              {parent_data, 3},
                              {parent_call, 3},
                              {parent_cast, 3},
                              {child_data, 4},
                              {child_call, 4},
                              {child_cast, 4},
                              {terminate, 2},
                              {code_change, 3}];
behaviour_info(_Other)    -> undefined.


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc gen_server behaviour init callback.

init({Context, Factory, Parent, Args, Module, Config}) ->
    %TODO: Maybe passing the name in parameter to prevent a call every time.
    {ok, Name} = twerl_context:get_name(Context),
    erlog:set_name(Name),
    BaseState = #?St{context = Context, parent = Parent,
                     factory = Factory, sub_mod = Module},
    {ok, State, RemConfig} = configure_(BaseState, Config),
    initialize_(State, RemConfig, Args).


%% --------------------------------------------------------------------
%% @doc gen_server behaviour handle_call callback.

handle_call({parent_call, {Parent, Request}}, Parent, State) ->
    #?St{parent = Parent} = State,
    delegate_parent_call_(State, Request);
handle_call({child_call, {Child, Request}}, Child, State) ->
    {ok, Key, _} = lookup_protocol_(State, Child),
    delegate_child_call_(State, Request, Key);
handle_call(Request, From, State) ->
    delegate_call_(State, Request, From).


%% --------------------------------------------------------------------
%% @doc gen_server behaviour handle_cast callback.

handle_cast({parent_data, {Parent, Data}}, State) ->
    #?St{parent = Parent} = State,
    push_input_data_(State, Data);
handle_cast({parent_packets, {Parent, Packets}}, State) ->
    #?St{parent = Parent} = State,
    push_input_packets_(State, Packets);
handle_cast({child_data, {Child, Data}}, State) ->
    {ok, Key, _} = lookup_protocol_(State, Child),
    delegate_child_data_(State, Data, Key);
handle_cast({child_packets, {Child, Packets}}, State) ->
    {ok, Key, _} = lookup_protocol_(State, Child),
    delegate_child_packets_(State, Packets, Key);
handle_cast({parent_cast, {Parent, Msg}}, State) ->
    #?St{parent = Parent} = State,
    delegate_parent_cast_(State, Msg);
handle_cast({child_cast, {Child, Msg}}, State) ->
    {ok, Key, _} = lookup_protocol_(State, Child),
    delegate_child_cast_(State, Msg, Key);
handle_cast(setup, State) ->
    setup_(State);
handle_cast({connect, {From, Peer, InStages, OutStages}},
            #?St{parent = Parent} = State)
  when Parent =:= undefined; Parent =:= From ->
    connect_(State, Peer, InStages, OutStages);
handle_cast({detached, Parent}, State) ->
    #?St{parent = Parent} = State,
    detached_(State);
handle_cast(Msg, State) ->
    delegate_cast_(State, Msg).


%% --------------------------------------------------------------------
%% @doc gen_server behaviour handle_info callback.

handle_info(timeout, State) ->
    pull_input_data_(State);
handle_info(Msg, #?St{input = ?PIPELINE_MATCH_INPUT(message)} = State) ->
    push_input_data_(State, Msg);    
handle_info(Msg, State) ->
    delegate_info_(State, Msg).


%% --------------------------------------------------------------------
%% @doc gen_server behaviour terminate callback.

terminate(Reason, State) ->
    terminate_(State, Reason).


%% --------------------------------------------------------------------
%% @doc gen_server behaviour code_change callback.

code_change(OldVsn, State, Extra) ->
    upgrade_(State, OldVsn, Extra).


%% ====================================================================
%% Internal Functions
%% ====================================================================

-include("gen_protocol_common.hrl").

configure_(State, Config) ->
    erlog:log("Configuring generic protocol with ~p...", [Config]),
    configure_(State, [], Config).

configure_(State, Acc, []) ->
    {ok, State, lists:reverse(Acc)};
configure_(State, Acc, [Opt |Rest]) ->
    configure_(State, [Opt |Acc], Rest).

initialize_(State, Config, Args) ->
    erlog:debug("Initializing generic protocol..."),
    delegate_init_(State, Config, Args).

setup_(State) ->
    erlog:debug("Setting up generic protocol..."),
    delegate_setup_(State).

connect_(State, Peer, InStages, OutStages) ->
    erlog:debug("Generic protocol connected to ~s.", [format:peer(Peer)]),
    case delegate_connection_made_(State, Peer) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        {noreply, NewState, _} ->
            create_pipelines_(NewState, InStages, OutStages)
    end.

detached_(State) ->
    {stop, normal, State}.

create_pipelines_(State, InStages, OutStages) ->
    {ok, InFmts, ExtraInStages} = delegate_prepare_input_(State),
    AllInStages = lists:flatten([InStages, ExtraInStages]),
    {ok, OutFmts, ExtraOutStages} = delegate_prepare_output_(State),
    AllOutStages = lists:flatten([ExtraOutStages, OutStages]),
    bind_input_(State, InFmts, AllInStages, OutFmts, AllOutStages).

bind_input_(State, _InFmts, [], OutFmts, OutStages) ->
    %TODO: What about the input format ?
    input_bound_(State, undefined, OutFmts, OutStages);
bind_input_(State, InFmts, InStages, OutFmts, OutStages) ->
    case twerl_pipeline:new(any, InFmts, InStages) of
        {error, Reason} ->
            {stop, Reason, State};
        {ok, InPipe} ->
            InFmt = ?PIPELINE_GET_OUTPUT(InPipe),
            State2 = State#?St{input = InPipe},
            input_bound_(State2, InFmt, OutFmts, OutStages)
    end.

input_bound_(State, InFmt, OutFmts, OutStages) ->
    case delegate_input_bound_(State, InFmt) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        {noreply, NewState, _} ->
            bind_output_(NewState, OutFmts, OutStages)
    end.

bind_output_(State, _OutFmts, []) ->
    %TODO: What about the output format ?
    delegate_output_bound_(State, undefined);
bind_output_(State, OutFmts, OutStages) ->
    case twerl_pipeline:new(OutFmts, any, OutStages) of
        {error, Reason} ->
            {stop, Reason, State};
        {ok, OutPipe} ->
            OutFmt = ?PIPELINE_GET_INPUT(OutPipe),
            State2 = State#?St{output = OutPipe},
            delegate_output_bound_(State2, OutFmt)
    end.

push_input_data_(#?St{input = undefined} = State, Data) ->
    % No input pipeline so no need to timouet to pull any more data
    delegate_parent_data_(State#?St{timeout = infinity}, Data);
push_input_data_(#?St{input = Pipe} = State, Data) ->
    % Input pipeline may contains more data packet,
    % use timeout to pull more packet later.
    input_result_(State#?St{timeout = 0}, twerl_pipeline:push(Data, Pipe)).

push_input_packets_(#?St{input = undefined} = State, Packets) ->
    delegate_parent_packets_(State, Packets);
push_input_packets_(State, Packets) ->
    push_input_data_packet_(State, Packets).

push_input_data_packet_(State, [Data, Packets]) ->
    #?St{input = Pipe} = State,
    case input_result_(State, twerl_pipeline:push(Data, Pipe)) of
        {stop, _, _} = Response -> Response;
        {noreply, NewState, _} -> push_input_packets_(NewState, Packets)
    end.

pull_input_data_(#?St{input = undefined} = State) ->
    % No input pipeline, nothing to pull
    {noreply, State};
pull_input_data_(#?St{input = Pipe} = State) ->
    input_result_(State, twerl_pipeline:pull(next, Pipe)).

input_result_(State, {data, Data, _Fmt, Pipe}) ->
    delegate_parent_data_(State#?St{input = Pipe}, Data);
input_result_(State, {consumed, Pipe}) ->
    {noreply, State#?St{input = Pipe}, State#?St.timeout};
input_result_(State, {more, next, message, Pipe}) ->
    % No more data in imput pipeline, cancel the timeout
    {noreply, State#?St{input = Pipe, timeout = infinity}, infinity};
input_result_(#?St{output = ?PIPELINE_MATCH_INPUT(message)},
              {ignore, _Data, _Pipe}) ->
    erlang:error(not_implemented);
input_result_(State, {ignore, Data, Pipe}) ->
    delegate_info_(State#?St{input = Pipe}, Data);
input_result_(State, {eos, Pipe}) ->
    {stop, normal, State#?St{input = Pipe}}.

terminate_(State, Reason) ->
    erlog:debug("Generic protocol terminated: ~p.", [Reason]),
    delegate_terminate_(State, Reason).

upgrade_(State, OldVsn, Extra) ->
    erlog:debug("Upgrading generic protocol code from version ~w...", [OldVsn]),
    delegate_code_change_(State, OldVsn, Extra).

delegate_init_(State, Config, Args) ->
    #?St{sub_mod = SubMod, sub_state = undefined} = State,
    case SubMod:init(Config, Args) of
        {ok, SubState} ->
            ok = gen_server:cast(self(), setup),
            {ok, State#?St{sub_state = SubState}};
        {stop, _} = Response -> Response
    end.

delegate_setup_(State) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:setup(State, SubState)).

delegate_connection_made_(State, Peer) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:connection_made(Peer, State, SubState)).

delegate_prepare_input_(State) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    SubMod:prepare_input(SubState).

delegate_prepare_output_(State) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    SubMod:prepare_output(SubState).

delegate_input_bound_(State, InFmt) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:input_bound(InFmt, State, SubState)).

delegate_output_bound_(State, OutFmt) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:output_bound(OutFmt, State, SubState)).

delegate_parent_data_(State, Data) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    case SubMod:parent_data(Data, State, SubState) of
        {ok, #?St{} = NewState} ->
            % The pipe may contain more data we need to pull
            {noreply, NewState, NewState#?St.timeout};
        {stop, _, #?St{}} = Response -> Response
    end.

delegate_parent_packets_(State, []) ->
    % Only called when there is no input pipeline so a timeout is not needed
    {noreply, State, State#?St.timeout};
delegate_parent_packets_(State, [Data |Packets]) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    case SubMod:parent_data(Data, State, SubState) of
        {ok, #?St{} = NewState} ->
            delegate_parent_packets_(NewState, Packets);
        {stop, _, #?St{}} = Response -> Response
    end.

delegate_child_data_(State, Data, Key) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:child_data(Data, Key, State, SubState)).

delegate_child_packets_(State, [], _Key) ->
    {noreply, State, State#?St.timeout};
delegate_child_packets_(State, [Data |Packets], Key) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    case check_result_(SubMod:child_data(Data, Key, State, SubState)) of
        {stop, _, _} = Response ->
            Response;
        {noreply, NewState, _} ->
            delegate_child_packets_(NewState, Packets, Key)
    end.

delegate_call_(State, Request, From) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_call_result_(SubMod:handle_call(Request, From, State, SubState)).

delegate_cast_(State, Msg) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:handle_cast(Msg, State, SubState)).

delegate_info_(State, Msg) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:handle_info(Msg, State, SubState)).

delegate_parent_call_(State, Request) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_call_result_(SubMod:parent_call(Request, State, SubState)).

delegate_parent_cast_(State, Msg) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:parent_cast(Msg, State, SubState)).

delegate_child_call_(State, Request, Key) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_call_result_(SubMod:child_call(Request, Key, State, SubState)).

delegate_child_cast_(State, Msg, Key) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    check_result_(SubMod:child_cast(Msg, Key, State, SubState)).

delegate_code_change_(State, OldVsn, Extra) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    case SubMod:code_change(OldVsn, SubState, Extra) of
        {ok, NewSubState} ->
            {ok, State#?St{sub_state = NewSubState}};
        {error, Reason} ->
            {error, Reason}
    end.

delegate_terminate_(State, Reason) ->
    #?St{sub_mod = SubMod, sub_state = SubState} = State,
    SubMod:terminate(Reason, SubState).

check_result_({ok, #?St{} = NewState}) ->
    {noreply, NewState, NewState#?St.timeout};
check_result_({noreply, #?St{} = NewState}) ->
    {noreply, NewState, NewState#?St.timeout};
check_result_({stop, _, #?St{}} = Response) ->
    Response;
check_result_({error, Reason, #?St{} = NewState}) ->
    {stop, Reason, NewState}.

check_call_result_({ok, #?St{} = NewState}) ->
    {noreply, NewState, NewState#?St.timeout};
check_call_result_({noreply, #?St{} = NewState}) ->
    {noreply, NewState, NewState#?St.timeout};
check_call_result_({reply, Reply, #?St{} = NewState}) ->
    {reply, Reply, NewState, NewState#?St.timeout};
check_call_result_({stop, _, #?St{}} = Response) ->
    Response;
check_call_result_({stop, _, _, #?St{}} = Response) ->
    Response.
