%% ===========================================================================
%% @doc        Set of functions called from inside a twerl protocols
%%             that could alter gen_protocol behaviour internal state.
%% @since      Dec 30, 2011
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

-module(super_protocol).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(protocol).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_types.hrl").

-include("gen_protocol.hrl").

-include("twerl_pipeline.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% Tail callbacks exports
-export([continue/2,
         terminate/2,
         fail/3,
         reply/3,
         reply_and_stop/3,
         reply_and_fail/4]).

%% Super immutable callback exports
-export([call_parent/2, call_parent/3,
         cast_parent/2,
         call_child/3, call_child/4,
         cast_child/3]).

%% Super mutable callback exports
-export([negotiate_input/3,
         negotiate_output/3,
         parent_data/3,
         parent_packets/3,
         parent_query/3,
         child_start/5,
         child_connect/4,
         child_detach/3,
         child_data/4,
         child_packets/4,
         bind_input/4,
         bind_output/4]).


%% ====================================================================
%% Tail functions, protocols last statment must always
%% be a call to one of these function. Note that some make sens only
%% in specific context, like reply* functions that make sens only
%% when used from inside a handle_call function.
%% ====================================================================

%% --------------------------------------------------------------------
%% @spec continue(Super::term(), State::term()) -> any()
%%
%% @doc Protocol's tail function to continue protocol operation normally.

-spec continue(Super::super(), State::term()) ->
          {ok, NewSuper::super()}.

continue(Super, State) ->
    {ok, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @spec terminate(Super::term(), State::term()) -> any()
%%
%% @doc Protocol's tail function to terminate the protocol normaly.

-spec terminate(Super::super(), State::term()) ->
          {stop, Reason::term(), NewSuper::super()}.

terminate(Super, State) ->
    erlog:debug("Protocol terminating normaly"),
    {stop, normal, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @spec fail(Super::term(), State::term(), Reason::term()) -> any()
%%
%% @doc Protocol's tail function to make the protocol fail.

-spec fail(Super::super(), State::term(), Reason::term()) ->
          {stop, Reason::term(), NewSuper::super()}.

fail(Super, State, Reason) ->
    erlog:debug("Protocol failure: ~p", [Reason]),
    {stop, Reason, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @spec reply(Super::term(), State::term(), Response::term()) -> any()
%%
%% @doc Protocol's tail function to reply to a message call.

-spec reply(Super::super(), State::term(), Response::term()) ->
          {reply, Response::term(), NewSuper::super()}.

reply(Super, State, Response) ->
    {reply, Response, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @spec reply_and_terminate(Super::term(), State::term(),
%%                           Response::term()) -> any()
%%
%% @doc Tail function to reply to a message call
%% and then terminate the protocol normaly.6

-spec reply_and_stop(Super::super(), State::term(), Response::term()) ->
          {stop, normal, Response::term(), NewSuper::super()}.

reply_and_stop(Super, State, Response) ->
    erlog:debug("Protocol terminating normaly"),
    {stop, normal, Response, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @spec reply_and_fail(Super::term(), State::term(),
%%                      Reason::term(), Response::term()) -> any()
%%
%% @doc Protocol's tail function to reply to a message call
%% and then fail for the specified reason.

-spec reply_and_fail(Super::super(), State::term(),
                     Reason::stop_reason(), Response::term()) ->
          {stop, Reason::stop_reason(), Response::term(), NewSuper::super()}.

reply_and_fail(Super, State, Reason, Response) ->
    erlog:debug("Protocol failure: ~p", [Reason]),
    {stop, Reason, Response, Super#?St{sub_state = State}}.


%% ====================================================================
%% Immutable Callback Functions
%% They take Super but do not modify it.
%% ====================================================================

%% --------------------------------------------------------------------
%% @spec call_parent(Super::term(), Msg::term()) -> term()
%%
%% @doc Makes a synchronous call to parent protocol.

-spec call_parent(Super::super(), Msg::term()) -> any().

call_parent(#?St{parent = Parent}, Request)
  when Parent =/= undefined ->
    gen_server:call(Parent, {child_call, {self(), Request}}).

%% --------------------------------------------------------------------
%% @spec call_parent(Super::term(), Msg::term(), Timeout) -> term()
%%       Timeout = integer() | infinity
%%
%% @doc Makes a synchronous call to parent protocol protected by a timeout.

-spec call_parent(Super::super(), Msg::term(), Timeout::timeout()) -> any().

call_parent(#?St{parent = Parent}, Request, Timeout)
  when Parent =/= undefined ->
    gen_server:call(Parent, {child_call, {self(), Request}}, Timeout).


%% --------------------------------------------------------------------
%% @spec cast_parent(Super::term(), Msg::term()) -> term()
%%
%% @doc Sends an asynchronous message to parent protocol.

-spec cast_parent(Super::super(), Msg::term()) -> ok.

cast_parent(#?St{parent = Parent}, Msg) when Parent =/= undefined ->
    gen_server:cast(Parent, {child_cast, {self(), Msg}}).


%% --------------------------------------------------------------------
%% @spec call_child(Super::term(), Key::term(), Msg::term()) -> term()
%%
%% @doc Makes a synchronous call to the child protocol with specified key.

-spec call_child(Super::super(), Proto::twerl_proto_key(), Msg::term()) ->
          term().

call_child(Super, Key, Request) ->
    {ok, _, Proto} = lookup_protocol_(Super, Key),
    gen_server:call(Proto, {parent_call, {self(), Request}}).


%% --------------------------------------------------------------------
%% @spec call_child(Super::term(), Key::term(), Msg::term(), Timeout) -> term()
%%       Timeout = integer() | infinity
%%
%% @doc Makes a synchronous call to the child protocol with specified key
%% protected by specified timeout.

-spec call_child(Super::super(), Proto::twerl_proto_key(),
                 Msg::term(), Timeout::timeout()) ->
          term().

call_child(Super, Key, Request, Timeout) ->
    {ok, _, Proto} = lookup_protocol_(Super, Key),
    gen_server:call(Proto, {parent_call, {self(), Request}}, Timeout).


%% --------------------------------------------------------------------
%% @spec cast_child(Super::term(), Key::term(), Msg::term()) -> term()
%%
%% @doc Sends and asynchronous message to the child protocol
%% with specified key.

-spec cast_child(Super::super(), Proto::twerl_proto_key(), Msg::term()) -> ok.

cast_child(Super, Key, Msg) ->
    {ok, _, Proto} = lookup_protocol_(Super, Key),
    gen_server:cast(Proto, {parent_cast, {self(), Msg}}).


%% ====================================================================
%% Mutable Callback Functions
%% Thet takes Super and State as parameter and return an updated Super.
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Negotiate input pipeline format.

-spec negotiate_input(Super::super(), State::term(),
                      Formats::twerl_stage_formats()) ->
          {ok, Format::twerl_stage_format(), NewSuper::super()}
        | {error, Reason::term(), NewSuper::super()}.

negotiate_input(Super, State, Formats) ->
    negotiate_input_(Super#?St{sub_state = State}, Formats).


%% --------------------------------------------------------------------
%% @doc Negotiate output pipeline format.

-spec negotiate_output(Super::super(), State::term(),
                      Formats::twerl_stage_formats()) ->
%%           {ok, Format::twerl_stage_format(), NewSuper::super()}
%%         | {error, Reason::term(), NewSuper::super()}.
          no_return().

negotiate_output(_Super, _State, _Formats) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Sends data for the parent protocol to collect.
%% Can be use as a protocol tail function.

-spec parent_data(Super::super(), State::term(), Data::twerl_stage_data()) ->
          {ok, NewSuper::super()}
        | {eos, NewSuper::super()}.

parent_data(#?St{output = undefined} = Super, State, Data) ->
    ok = cast_child_data_(Super, Data),
    {ok, Super#?St{sub_state = State}};
parent_data(Super, State, Data) ->
    send_data_(Super#?St{sub_state = State}, Data).


%% --------------------------------------------------------------------
%% @doc Sends multiple data packets for the parent protocol to collect.
%% Can be use as a protocol tail function.

-spec parent_packets(Super::super(), State::term(),
                     Packets::twerl_stage_packets()) ->
          {ok, NewSuper::super()}
        | {eos, NewSuper::super()}.

parent_packets(#?St{output = undefined} = Super, State, Packets) ->
    ok = cast_child_packets_(Super, Packets),
    {ok, Super#?St{sub_state = State}};
parent_packets(Super, State, Packets) ->
    send_packets_(Super#?St{sub_state = State}, Packets).


%% --------------------------------------------------------------------
%% @doc Queries parent protocol for data.

-spec parent_query(Super::super(), State::term(),
                   Query::twerl_stage_query()) ->
          {data, Data::twerl_stage_data(), Super::super()}
        | {eos, Super::super()}.

parent_query(Super, State, Query) ->
    query_data_(Super#?St{sub_state = State}, Query).


%% --------------------------------------------------------------------
%% @doc Starts a child-protocol with the calling protocol as parent.

-spec child_start(Super::super(), State::term(), Key::twerl_proto_key(),
                  ProtoDef::twerl_def(), Args::list()) ->
          {ok, NewSuper::super()}.

child_start(Super, State, Key, ProtoDef, Args) ->
    #?St{factory = Factory} = Super,
    {ok, Proto} = gen_factory:create_protocol(Factory, ProtoDef, self(), Args),
    true = erlang:link(Proto),
    store_protocol_(Super#?St{sub_state = State}, Key, Proto).


%% --------------------------------------------------------------------
%% @doc Connect current protocol to a child one previously started
%% using start_protocol.
%% Can be use as a protocol tail function.

-spec child_connect(Super::super(), State::term(),
                    Key::twerl_proto_key(), Peer::twerl_peer()) ->
          {ok, NewSuper::super()}.

child_connect(Super, State, Proto, Peer) when is_pid(Proto) ->
    ok = gen_protocol:connect(Proto, Peer, [], []),
    {ok, Super#?St{sub_state = State}};
child_connect(Super, State, Key, Peer) ->
    {ok, _, Proto} = lookup_protocol_(Super, Key),
    ok = gen_protocol:connect(Proto, Peer, [], []),
    {ok, Super#?St{sub_state = State}}.


%% --------------------------------------------------------------------
%% @doc Detach a child-protocol.
%% The protocol is informed and should terminate itself.
%% Can be use as a protocol tail function.

-spec child_detach(Super::super(), State::term(), Key::twerl_proto_key()) ->
          {ok, NewSuper::super()}.

child_detach(Super, State, Key) ->
    {ok, Proto, Super1} = take_protocol_(Super, Key),
    ok = gen_protocol:detach(Proto),
    {ok, Super1#?St{sub_state = State}}.

%% --------------------------------------------------------------------
%% @doc Push data to a child-protocol.
%% Can be use as a protocol tail function.

-spec child_data(Super::super(), State::term(),
                 Key::twerl_proto_key(), Data::twerl_stage_data()) ->
          {ok, NewSuper::super()}.

child_data(Super, State, Key, Data) ->
    ok = cast_parent_data_(Super, Key, Data),
    {ok, Super#?St{sub_state = State}}.

%% --------------------------------------------------------------------
%% @doc Push packets to a child-protocol.
%% Can be use as a protocol tail function.

-spec child_packets(Super::super(), State::term(),
                    Key::twerl_proto_key(), Packets::twerl_stage_packets()) ->
          {ok, NewSuper::super()}.

child_packets(Super, State, Key, Packets) ->
    ok = cast_parent_packets_(Super, Key, Packets),
    {ok, Super#?St{sub_state = State}}.

%% --------------------------------------------------------------------
%% @doc Binds the input pipeline to a sub-protocol,
%%      data_received will not be called anymore afterward.

-spec bind_input(Super::super(), State::term(), Key::twerl_proto_key(),
                 Formats::twerl_stage_formats()) ->
%%           {ok, Format::twerl_stage_format(), NewSuper::super()}
%%         | {error, Reason::term(), NewSuper::super()}.
          no_return().

bind_input(_Super, _State, _Key, _Formats) ->
    erlang:error(not_implemented).

%% --------------------------------------------------------------------
%% @doc Binds the ouput pipeline to a sub-protocol,
%%      data_collected will not be called anymore afterward.

-spec bind_output(Super::super(), State::term(), Key::twerl_proto_key(),
                  Formats::twerl_stage_formats()) ->
%%           {ok, Format::twerl_stage_format(), NewSuper::super()}
%%         | {error, Reason::term(), NewSuper::super()}.
          no_return().

bind_output(_Super, _State, _Key, _Formats) ->
    erlang:error(not_implemented).


%% ====================================================================
%% Internal Functions
%% ====================================================================

-include("gen_protocol_common.hrl").

store_protocol_(#?St{children = Children} = Super, Key, Proto) ->
    case lists:keymember(Key, 1, Children) of
        false ->
            {ok, Super#?St{children = [{Key, Proto} |Children]}}
    end.

take_protocol_(#?St{children = Children} = Super, Key) when is_pid(Key) ->
    {value, {_, Proto}, NewChildren} = lists:keytake(Key, 2, Children),
    {ok, Proto, Super#?St{children = NewChildren}};
take_protocol_(#?St{children = Children} = Super, Key) ->
    {value, {_, Proto}, NewChildren} = lists:keytake(Key, 1, Children),
    {ok, Proto, Super#?St{children = NewChildren}}.

send_data_(State, Data) ->
    #?St{output = Pipe} = State,
    send_loop_(State, twerl_pipeline:push(Data, Pipe)).

send_loop_(State, {consumed, Pipe}) ->
    send_loop_(State, twerl_pipeline:pull(next, Pipe));
send_loop_(State, {data, Data, _Fmt, Pipe}) ->
    ok = cast_child_data_(State, Data),
    {ok, State#?St{output = Pipe}};
send_loop_(State, {more, next, _, Pipe}) ->
    {ok, State#?St{output = Pipe}};
send_loop_(State, {eos, Pipe}) ->
    {eos, State#?St{output = Pipe}}.

send_packets_(State, []) ->
    {ok, State};
send_packets_(State, [Data |Packets]) ->
    #?St{output = Pipe} = State,
    case send_loop_(State, twerl_pipeline:push(Data, Pipe)) of
        {ok, NewState} ->
            send_packets_(NewState, Packets);
        {eos, NewState} ->
            {eos, NewState}
    end.

query_data_(State, Query) ->
    %FIXME: What about pulling some more after a query ?
    %       If a protocol query data and no message is ever received
    %       some packets could get stuck in the pipeline.
    #?St{input = Pipe} = State,
    case twerl_pipeline:pull(Query, Pipe) of
        {data, Data, _Fmt, NewPipe} ->
            NewState = State#?St{output = NewPipe},
            {data, Data, NewState};
        {eos, NewPipe} ->
            NewState = State#?St{output = NewPipe},
            {eos, NewState}
    end.

negotiate_input_(State, Formats) ->
    #?St{input = Pipe} = State,
    case twerl_pipeline:negotiate(any, Formats, Pipe) of
        {error, Reason} ->
            {error, Reason, State};
        {ok, NewPipe} ->
            NewState = State#?St{input = NewPipe},
            {ok, ?PIPELINE_GET_OUTPUT(NewPipe), NewState}
    end.

cast_child_data_(State, Data) ->
    gen_server:cast(State#?St.parent, {child_data, {self(), Data}}).

cast_child_packets_(State, Packets) ->
    gen_server:cast(State#?St.parent, {child_packets, {self(), Packets}}).

cast_parent_data_(State, Key, Data) ->
    {ok, _, Proto} = lookup_protocol_(State, Key),
    gen_server:cast(Proto, {parent_data, {self(), Data}}).

cast_parent_packets_(State, Key, Packets) ->
    {ok, _, Proto} = lookup_protocol_(State, Key),
    gen_server:cast(Proto, {parent_packets, {self(), Packets}}).
