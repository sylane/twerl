%% ===========================================================================
%% @doc        Base module for twerl protocols declaring all required
%%             callbacks and implementing some when meaningful.
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

-module(base_protocol).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(gen_protocol).

-erlog_category(protocol).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_protocol.hrl").

-include("twerl_types.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([]).

%% Behaviour gen_protocol callbacks
-export([init/2,
         setup/2,
         connection_made/3,
         prepare_input/1,
         prepare_output/1,
         input_bound/3,
         output_bound/3,
         parent_data/3,
         child_data/4,
         handle_call/4,
         handle_cast/3,
         handle_info/3,
         parent_call/3,
         parent_cast/3,
         child_call/4,
         child_cast/4,
         terminate/2,
         code_change/3]).


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type state() :: term().


%% ====================================================================
%% Behaviour gen_protocol Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Called to initialise protocol's process state.
%% It recieve arguments form it's parent and some configuration.

-spec init(Config::list(), Args::list()) -> no_return().

init(_Config, _Args) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called when protocol's process running

-spec setup(Super::super(), State::state()) -> {ok, Super::super()}.

setup(Super, State) ->
    super_protocol:continue(Super, State).


%% --------------------------------------------------------------------
%% @doc Called when a protocol got connected to a parent

-spec connection_made(Peer::twerl_peer(), Super::super(), State::state()) ->
          no_return().

connection_made(_Peer, Super, State) ->
    super_protocol:continue(Super, State).


%% --------------------------------------------------------------------
%% @doc Called to prepare the protocol's input pipeline,
%% input format and additional pipeline stages can be specified.

-spec prepare_input(State::state()) ->
          {ok, InFormat::twerl_stage_format(), InStages::twerl_stages()}.

prepare_input(_State) -> {ok, any, []}.


%% --------------------------------------------------------------------
%% @doc Called to prepare the protocol's output pipeline,
%% output format and additional pipeline stages can be specified.

-spec prepare_output(State::state()) ->
          {ok, OutFormat::twerl_stage_format(), OutStages::twerl_stages()}.

prepare_output(_State) -> {ok, any, []}.


%% --------------------------------------------------------------------
%% @doc Called when the input pipeline has been bound.

-spec input_bound(Format::twerl_stage_format(),
                  Super::super(), State::state()) ->
          {ok, NewSuper::super()}.

input_bound(_Format, Super, State) ->
    super_protocol:continue(Super, State).


%% --------------------------------------------------------------------
%% @doc Called when the output pipeline has been bound.

-spec output_bound(Format::twerl_stage_format(),
                   Super::super(), State::state()) ->
          {ok, NewSuper::super()}.

output_bound(_Format, Super, State) ->
    super_protocol:continue(Super, State).


%% --------------------------------------------------------------------
%% @doc Called when receiving an unknown gen_server call.

-spec handle_call(Request::term(), From::term(),
                  Super::super(), State::state()) ->
          no_return().

handle_call(Request, _From, _Super, _State) ->
    erlang:error({unexpected_call, Request}).


%% --------------------------------------------------------------------
%% @doc Called when receiving an unknown gen_server cast.

-spec handle_cast(Msg::term(), Super::super(), State::state()) ->
          no_return().

handle_cast(Msg, _Super, _State) ->
    erlang:error({unexpected_cast, Msg}).


%% --------------------------------------------------------------------
%% @doc Called when receiving an unknown message.

-spec handle_info(Msg::term(), Super::super(), State::state()) ->
          no_return().

handle_info(Msg, _Super, _State) ->
    erlang:error({unexpected_message, Msg}).


%% --------------------------------------------------------------------
%% @doc Called when data has been received from parent protocol
%%      through the input pipeline.

-spec parent_data(Data::twerl_stage_data(), Super::super(), State::state()) ->
          no_return().

parent_data(Data, _Super, _State) ->
    erlang:error({unexpected_parent_data, Data}).


%% --------------------------------------------------------------------
%% @doc Called when a parent protocol is calling.

-spec parent_call(Request::term(), Super::super(), State::state()) ->
          no_return().

parent_call(Request, _Super, _State) ->
    erlang:error({unexpected_parent_call, Request}).


%% --------------------------------------------------------------------
%% @doc Called when a parent protocol is casting a message.

-spec parent_cast(Msg::term(), Super::super(), State::state()) ->
          no_return().

parent_cast(Msg, _Super, _State) ->
    erlang:error({unexpected_parent_cast, Msg}).


%% --------------------------------------------------------------------
%% @doc Called when data has been received from a child protocol.

-spec child_data(Data::twerl_stage_data(), Key::twerl_proto_key(),
                 Super::super(), State::state()) ->
          no_return().

child_data(Data, _Key, _Super, _State) ->
    erlang:error({unexpected_child_data, Data}).


%% --------------------------------------------------------------------
%% @doc Called when a child protocol is calling.

-spec child_call(Request::term(), Key::twerl_proto_key(),
                 Super::super(), State::state()) ->
          no_return().

child_call(Request, _Key, _Super, _State) ->
    erlang:error({unexpected_child_call, Request}).


%% --------------------------------------------------------------------
%% @doc Called when a child protocol is casting a message.

-spec child_cast(Msg::term(), Key::twerl_proto_key(),
                 Super::super(), State::state()) ->
          no_return().

child_cast(Msg, _Key, _Super, _State) ->
    erlang:error({unexpected_child_cast, Msg}).


%% --------------------------------------------------------------------
%% @doc Called when the protocol got terminated.

-spec terminate(Reason::terminate_reason(), State::state()) ->
          ok.

terminate(shutdown, _State) ->
    erlog:info("Protocol shutdown."),
    ok;
terminate(normal, _State) ->
    erlog:info("Protocol terminated normaly."),
    ok;
terminate(Reason, _State) ->
    erlog:warn("Protocol terminated: ~p.", [Reason]),
    ok.


%% --------------------------------------------------------------------
%% @doc Called to upgrade protocol's process state.

-spec code_change(OldVsn::term(), State::state(), Extra::term()) ->
        {ok, NewState::state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
