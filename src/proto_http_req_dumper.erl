%% ===========================================================================
%% @doc Protocol dumping the received requests to the console.
%% It expects messages following http_req idiom definition and use
%% the exact same pipeline stages than the HTTP server to encode
%% the HTTP requests and response.
%% Can be used as a client protocol too using prepare_client/0,
%% but it will only dump the response.
%%
%% @since      Jan 9, 2012
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

-module(proto_http_req_dumper).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(http_req_dumper).

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
-export([prepare/0,
         prepare_client/0]).

%% Overriden base_protocol callbacks
-export([init/2,
         prepare_input/1,
         prepare_output/1,
         parent_data/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Protocol's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Protocol's state
-record(?St, {side, state}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares an HTTP request dumper definition for server protocols.

prepare() ->
    {fun gen_protocol:create/7, ?MODULE, [server]}.


%% --------------------------------------------------------------------
%% @doc Prepares an HTTP request dumper definition for client protocols.

prepare_client() ->
    {fun gen_protocol:create/7, ?MODULE, [client]}.


%% ====================================================================
%% Overriden base_protocol Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour init callback.

init([Side], _Args) ->
    erlog:log("Initializing HTTP request dumper protocol..."),
    {ok, #?St{side = Side}}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_input callback.

prepare_input(_State) ->
    erlog:log("Preparing HTTP request dumper protocol input pipeline..."),
    {ok, {http_req, binary}, []}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour prepare_output callback.

prepare_output(_State) ->
    erlog:log("Preparing HTTP request dumper protocol output pipeline..."),
    {ok, {http_req, binary}, []}.


%% --------------------------------------------------------------------
%% @doc gen_protocol behaviour parent_data callback.

parent_data({request, Request} = Msg,
            Super, #?St{side = Side} = State) ->
    print_(Request, Side, request, [Msg]),
    ?super:continue(Super, State#?St{state = request});
parent_data({response, Request, _Response} = Msg,
            Super, #?St{side = Side} = State) ->
    print_(Request, Side, response, [Msg]),
    ?super:continue(Super, State#?St{state = response});
parent_data({activate, Request} = Msg, Super, #?St{side = server} = State) ->
    print_(Request, server, request, [Msg]),

    Headers = [{'Content-Type', <<"text/plain">>},
               {'Content-Length', <<"3">>}],    
    Response = http_res:new(Request, ok, Headers),
    Packets = [{response, Request, Response},
               {body, Request, <<"OK\n">>},
               {done, Request}],

    print_(Request, server, response, Packets),
    
    ?super:parent_packets(Super, State#?St{state = undefined}, Packets);
parent_data({body, Request, _Data} = Msg,
            Super, #?St{side = Side, state = Type} = State) ->
    print_(Request, Side, Type, [Msg]),
    ?super:continue(Super, State);
parent_data({done, Request} = Msg,
            Super, #?St{side = Side, state = Type} = State) ->
    print_(Request, Side, Type, [Msg]),
    ?super:continue(Super, State);
parent_data({error, Request, _Reason} = Msg,
            Super, #?St{side = Side, state = Type} = State) ->
    print_(Request, Side, Type, [Msg]),
    ?super:continue(Super, State).


%% ====================================================================
%% Internal Functions
%% ====================================================================

print_(Req, Side, Type, Packets) ->
    Prefix = get_prefix_(Req, Side, Type),
    {ok, Pipe} = make_printer_(Prefix),
    twerl_pipeline:consume_packets(Packets, Pipe).

get_prefix_(Req, Side, Type) ->
    io_lib:format("~w ~s ", [Req#http_req.id, get_direction_(Side, Type)]).

get_direction_(client, request) -> "<<<"; 
get_direction_(client, response) -> ">>>";
get_direction_(server, request) -> ">>>"; 
get_direction_(server, response) -> "<<<".

make_printer_(Prefix) ->
    {ok, ReqEnc} = stage_http_req_encoder:new(),
    {ok, PktEnc} = stage_http_pkt_encoder:new(),
    {ok, PktDec} = stage_packet_decoder:new(),
    {ok, StdOut} = stage_stdio_consumer:new(Prefix),
    twerl_pipeline:new([ReqEnc, PktEnc, PktDec, StdOut]).
