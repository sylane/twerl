%% ===========================================================================
%% @doc Stage encoding http_req idiom to http_pkt idiom.
%%
%% Supported output formats:
%%
%%  - {http_pkt, list} -> NOT SUPPORTED YET
%%  - {http_pkt, binary}
%%
%% Supported input formats:
%%
%%  - {http_req, list} -> NOT SUPPORTED YET
%%  - {http_req, binary}
%%
%% Supported input queries:
%%
%%  - next
%%
%% Supported pull queries:
%%
%%  - next
%%
%% @since      Jan 22, 2012
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

-module(stage_http_req_encoder).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(http_dec).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_stage.hrl").

-include("http_req.hrl").

-include("http_res.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0]).

%% Pipeline callbacks exports
-export([init/1,
         negotiate/2,
         agree/3,
         setup/3,
         process/3,
         continue/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Stage's state record name
-define(St, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {in = [], out = []}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates an HTTP encoder stage converting http_pkt idiom to raw data.

new() -> {ok, #?St{}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)                -> {http_req, binary};
negotiate(http_pkt, _)           -> {http_req, binary};
negotiate({http_pkt, binary}, _) -> {http_req, binary}.


agree({http_req, binary}, any, _)                -> {http_pkt, binary};
agree({http_req, binary}, http_pkt, _)           -> {http_pkt, binary};
agree({http_req, binary}, {http_pkt, binary}, _) -> {http_pkt, binary};
agree(_, _, _)                                   -> rejected.


setup(_, _, State) ->
    {ok, State}.


process(Data, Super, #?St{in = In, out = [O |Os]} = State) ->
    NewState = State#?St{in = [Data |In], out = Os},
    ?super:produce(Super, NewState, O);    
process(Data, Super, #?St{in = In, out = []} = State) ->
    case encode_http_req_(lists:reverse([Data |In]), []) of
        [O |Os] ->
            NewState = State#?St{in = [], out = Os},
            ?super:produce(Super, NewState, O);
        [] ->
            NewState = State#?St{in = [], out = []},
            ?super:need_more(Super, NewState, next)
    end.


continue(next, Super, #?St{out = [O |Os]} = State) ->
    NewState = State#?St{out = Os},
    ?super:produce(Super, NewState, O);    
continue(next, Super, State) ->
    ?super:need_more(Super, State, next);
continue(Query, Super, State) ->
    Reason = {query_not_supported, Query, ?MODULE},
    ?super:failed(Super, State, Reason).


%% ====================================================================
%% Internal Functions
%% ====================================================================

encode_http_req_([], Out) -> Out;
encode_http_req_([{body, _Req, Body} |Data], Out) ->
    encode_http_req_(Data, [{body, Body} |Out]);
encode_http_req_([{activate, _Req} |Data], Out) ->
    encode_http_req_(Data, Out);
encode_http_req_([{done, _Req} |Data], Out) ->
    encode_http_req_(Data, [eob |Out]);
encode_http_req_([{error, _Req, Reason} |Data], Out) ->
    encode_http_req_(Data, [{error, Reason} |Out]);
encode_http_req_([{request, Req} |Data], Out) ->
    Names = lists:reverse(http_req:get_headers(Req)),
    Out1 = encode_request_headers_(Req, Names, [eoh |Out]),
    #http_req{version = V, method = M, url = U} = Req,
    encode_http_req_(Data, [{request, V, M, U} |Out1]);
encode_http_req_([{response, _Req, Res} |Data], Out) ->
    Names = lists:reverse(http_res:get_headers(Res)),
    Out1 = encode_response_headers_(Res, Names, [eoh |Out]),
    #http_res{version = V, status = S, message = M} = Res,
    encode_http_req_(Data, [{response, V, S, M} |Out1]).

encode_request_headers_(_Req, [], Out) -> Out;
encode_request_headers_(Req, [N |Names], Out) ->
    NewOut = [{header, N, http_req:get_header_value(N, Req)} |Out],
    encode_request_headers_(Req, Names, NewOut).

encode_response_headers_(_Res, [], Out) -> Out;
encode_response_headers_(Res, [N |Names], Out) ->
    NewOut = [{header, N, http_res:get_header_value(N, Res)} |Out],
    encode_response_headers_(Res, Names, NewOut).
