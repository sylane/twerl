%% ===========================================================================
%% @doc Simplistic proof-of-concept protocol handling a pipeline
%% of http requests, delegating request processing to a sub-protocol
%% using http_req idiom.
%%
%% There is important missing parts:
%%
%%  - Concurent pipelined request limitation.
%%  - Timeouts to prevent staled connections.
%%
%% @since      May 17, 2010
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

-module(proto_http_server).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(http_server).

% Set to log for more logging
-erlog_max_level(debug).

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
         parent_data/3,
         child_data/4,
         connection_made/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Protocol's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Protocol's state
-record(?St, {child_def,
              peer,
              close,
              current,
              active,
              queue = queue:new()}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares an HTTP server protocol definition.

prepare(RequestHandlerDef) ->
    {fun gen_protocol:create/7, ?MODULE, [RequestHandlerDef]}.


%% ====================================================================
%% Overriden base_protocol Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour init callback.

init([RequestHandlerDef], _Args) ->
    erlog:log("Initializing HTTP server protocol..."),
    {ok, #?St{child_def = RequestHandlerDef}}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_input callback.

prepare_input(_State) ->
    erlog:log("Preparing HTTP server protocol input pipeline..."),
    {ok, HTTPDec} = stage_http_pkt_decoder:new(),
    {ok, {http_pkt, binary}, [HTTPDec]}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_output callback.

prepare_output(_State) ->
    erlog:log("Preparing HTTP server protocol output pipeline..."),
    {ok, ReqEnc} = stage_http_req_encoder:new(),
    {ok, PktEnc} = stage_http_pkt_encoder:new(),
    {ok, {http_req, binary}, [ReqEnc, PktEnc]}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour parent_data callback.

parent_data({request, Ver, Method, Url}, Super, State) ->
    decode_request_(Super, State, Ver, Method, Url);
parent_data({header, Name, Value}, Super, State) ->
    decode_header_(Super, State, Name, Value);
parent_data(eoh, Super, State) ->
    initiate_request_(Super, State);
parent_data({body, Data}, Super, State) ->
    request_body_(Super, State, Data);
parent_data(eob, Super, State) ->
    finish_request_(Super, State);
parent_data({error, {unexpected, <<"\r\n">>}}, Super, State) ->
    % Ignore extra new lines
    ?super:continue(Super, State);
parent_data(Data, Super, State) ->
    base_protocol:parent_data(Data, Super, State).


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour child_data callback.

child_data({response, Req, Res}, Req, Super, #?St{active = Req} = State) ->
    {ok, NewState} = process_response_(State, Req, Res),
    ?super:parent_data(Super, NewState, {response, Req, Res});
child_data({body, Req, Data}, Req, Super, #?St{active = Req} = State) ->
    ?super:parent_data(Super, State, {body, Req, Data});
child_data({done, Req}, Req, Super, #?St{active = Req} = State) ->
    finish_response_(Super, State, Req);
child_data(Data, Key, Super, State) ->
    base_protocol:child_data(Data, Key, Super, State).


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour connection_made callback.

connection_made(Peer, Super, State) ->
    erlog:debug("HTTP connection from ~s", [format:peer(Peer)]),
    ?super:continue(Super, State#?St{peer = Peer}).


%% ====================================================================
%% Internal Functions
%% ====================================================================

decode_request_(Super, #?St{peer = Peer, current = undefined} = State,
                Ver, Method, Url) when Peer =/= undefined ->
    Req = http_req:new(Peer, Method, Url#url{scheme = http}, Ver),
    {ok, NewState} = init_request_(State#?St{current = Req}, Ver),
    ?super:continue(Super, NewState).

init_request_(State, {1, 0}) ->
    {ok, State#?St{close = true}};
init_request_(State, {1, 1}) ->
    {ok, State#?St{close = false}}.

decode_header_(Super, #?St{current = Req} = State, Name, Value) ->
    NewReq = http_req:add_header(Name, Value, Req),
    process_header_(Super, State#?St{current = NewReq}, Name, Value).

process_header_(Super, State, 'Connection', Value) ->
    #?St{current = #http_req{version = Ver}} = State,
    {ok, NewState} = process_req_conn_(State, Ver, Value),
    ?super:continue(Super, NewState);
process_header_(Super, State, 'Host', Value) ->
    #?St{current = #http_req{url = Url} = Req} = State,
    {Host, Port} = httplib:parse_host(erlang:binary_to_list(Value)),
    NewUrl = Url#url{domain = Host, port = Port},
    NewState = State#?St{current = Req#http_req{url = NewUrl}},
    ?super:continue(Super, NewState);
process_header_(Super, State, _Name, _Value) ->
    ?super:continue(Super, State).

process_req_conn_(State, {1, 0}, Values) ->
    {ok, State#?St{close = not lists:member(<<"Keep-Alive">>, Values)}};
process_req_conn_(State, {1, 1}, Values) ->
    {ok, State#?St{close = lists:member(<<"close">>, Values)}}.

initiate_request_(Super, State) ->
    #?St{peer = P, child_def = D, current = R} = State,
    erlog:log("Delegating request ~s to protocol ~s...",
              [R:format(), ?def_mod(D)]),
    {ok, Super1} = ?super:child_start(Super, State, R, D, []),
    {ok, Super2} = ?super:child_connect(Super1, State, R, P),
    {ok, Super3} = ?super:child_data(Super2, State, R, {request, R}),
    ?super:continue(Super3, State).

request_body_(Super, #?St{current = R} = State, D) when R =/= undefined ->
    ?super:child_data(Super, State, R, {body, R, D}).

finish_request_(Super, #?St{current = R} = State) when R =/= undefined ->
    {ok, Super1} = ?super:child_data(Super, State, R, {done, R}),
    queue_request_(Super1, State#?St{current = undefined}, R).

queue_request_(Super, #?St{active = undefined} = State, R) ->
    activate_request_(Super, State, R);
queue_request_(Super, State, R) ->
    erlog:log("Queuing request for ~s", [http_req:format(R)]),
    ?super:continue(Super, State#?St{queue = queue:in(R, State#?St.queue)}).

activate_request_(Super, #?St{active = undefined} = State, R) ->
    erlog:log("Activating request ~s...", [R:format()]),
     {ok, Super1} = ?super:child_data(Super, State, R, {activate, R}),
     ?super:continue(Super1, State#?St{active = R}).

process_response_(State, _Req, Res) ->
    ConnHeadervalue = http_res:get_header_value('Connection', Res),   
    process_res_conn_(State, Res#http_res.version, ConnHeadervalue).

process_res_conn_(#?St{close = ReqClose} = State, {1, 0}, Values) ->
    ResClose = not lists:member(<<"Keep-Alive">>, Values),
    {ok, State#?St{close = ResClose and ReqClose}};
process_res_conn_(#?St{close = ReqClose} = State, {1, 1}, Values) ->
    ResClose = lists:member(<<"close">>, Values),
    {ok, State#?St{close = ReqClose or ResClose}}.

finish_response_(Super, State, Req) ->
    erlog:log("Response finished for request ~s...", [Req:format()]),
    {ok, Super1} = ?super:child_detach(Super, State, Req),
    {ok, Super2} = ?super:parent_data(Super1, State, {done, Req}),
    request_finished(Super2, State#?St{active = undefined}).

request_finished(Super, #?St{close = false} = State) ->
    activate_next_request_(Super, State);
request_finished(Super, #?St{close = true} = State) ->
    ?super:terminate(Super, State).
       
activate_next_request_(Super, #?St{active = undefined, queue = Q} = State) ->
    case queue:out(Q) of
        {{value, Req}, Q2} ->
            activate_request_(Super, State#?St{queue = Q2}, Req);
        {empty, Q2} ->
            ?super:continue(Super, State#?St{queue = Q2})
    end.
