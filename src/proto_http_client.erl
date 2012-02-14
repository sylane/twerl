%% ===========================================================================
%% @doc Simplistic proof-of-concept HTTP client protocol delegating
%% response processing to a sub-protocol using http_req idiom.
%%
%% @since      Jul 30, 2010
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

-module(proto_http_client).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(http_client).

-extends(base_protocol).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_protocol.hrl").

-include("http_req.hrl").

-include("http_res.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([prepare/1]).

%% Overriden base_protocol callbacks
-export([init/2,
         prepare_input/1,
         prepare_output/1,
         connection_made/3,
         output_bound/3,
         parent_data/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Stage's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {child_def, peer, request, response}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares an HTTP client protocol definition.

prepare(ResponseHandler) ->
    {fun gen_protocol:create/7, ?MODULE, [ResponseHandler]}.


%% ====================================================================
%% Overriden base_protocol Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour init callback.

init([ResponseHandler], _Args) ->
    erlog:log("Initializing HTTP client protocol..."),
    {ok, #?St{child_def = ResponseHandler}}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_input callback.

prepare_input(_State) ->
    erlog:log("Preparing HTTP client protocol input pipeline..."),
    {ok, HTTPDec} = stage_http_pkt_decoder:new(),
    {ok, {http_pkt, binary}, [HTTPDec]}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_output callback.

prepare_output(_State) ->
    erlog:log("Preparing HTTP client protocol output pipeline..."),
    {ok, ReqEnc} = stage_http_req_encoder:new(),
    {ok, PktEnc} = stage_http_pkt_encoder:new(),
    {ok, {http_req, binary}, [ReqEnc, PktEnc]}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour output_bound callback.

output_bound(_Format, Super, State) ->
    initiate_request_(Super, State).


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour connection_made callback.

connection_made(Peer, Super, State) ->
    erlog:debug("HTTP connection to ~s", [format:peer(Peer)]),
    ?super:continue(Super, State#?St{peer = Peer}).


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour parent_data callback.

parent_data({response, Ver, Status, Msg}, Super, State) ->
    decode_response_(Super, State, Ver, Status, Msg);
parent_data({header, Name, Value}, Super, State) ->
    decode_header_(Super, State, Name, Value);
parent_data(eoh, Super, State) ->
    delegate_response_(Super, State);
parent_data({body, Data}, Super, State) ->
    delegate_body_(Super, State, Data);
parent_data(eob, Super, State) ->
    finish_response_(Super, State);
parent_data(Packet, Super, State) ->
    erlog:debug("Unhandled data: ~p", [Packet]),
    ?super:continue(Super, State).


%% ====================================================================
%% Internal Functions
%% ====================================================================

initiate_request_(Super, State) ->
    #?St{peer = Peer} = State,
    Url = #url{scheme = http},
    Ver = {1, 0},
    Method = 'GET',
    Req = http_req:new(Peer, Method, Url, Ver),
    Packets = [{request, Req}, {done, Req}],
    erlog:debug("Requesting to ~w ~s", [Method, "/"]),
    %FIXME: send_packets() can return {eos, _} when {ok, _} is expected
    ?super:parent_packets(Super, State#?St{request = Req}, Packets).

decode_response_(Super, State, Ver, Status, Msg) ->
    #?St{peer = Peer} = State,
    erlog:debug("Got HTTP ~s response: ~s (~w)",
                [format:version(Ver), Msg, Status]),
    Res = http_res:new(Peer, Ver, Status, Msg),
	?super:continue(Super, State#?St{response = Res}).

decode_header_(Super, State, Name, Value) ->
    #?St{response = Res} = State,
    NewRes = http_res:add_header(Name, Value, Res),
    process_header_(Super, State#?St{response = NewRes}, Name, Value).

process_header_(Super, State, _Name, _Value) ->
    ?super:continue(Super, State).

delegate_response_(Super, State) ->
    #?St{peer = Peer, child_def = Def, request = Req, response = Res} = State,
    erlog:log("Delegating response ~s to protocol ~s...",
              [Res:format(), ?def_mod(Def)]),
    {ok, Super1} = ?super:child_start(Super, State, Req, Def, []),
    {ok, Super2} = ?super:child_connect(Super1, State, Req, Peer),
    Msg = {response, Req, Res},
    {ok, Super3} = ?super:child_data(Super2, State, Req, Msg),
    ?super:continue(Super3, State).

delegate_body_(Super, #?St{request = Req} = State, Data) ->
    ?super:child_data(Super, State, Req, {body, Req, Data}).

finish_response_(Super, #?St{request = Req} = State) ->
    {ok, Super1} = ?super:child_data(Super, State, Req, {done, Req}),
    ?super:child_detach(Super1, State, Req).
