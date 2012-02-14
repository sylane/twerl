%% ===========================================================================
%% @doc        Module stage_http_pkt_encoder unit tests.
%% @since      Aug 06, 2010
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
-module(test_stage_http_pkt_encoder).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_max_level(debug).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("pipeline_test_utils.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([coverage/0]).

%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(idiom_http_pkt, [request/5,
                         response/4,
                         header/3,
                         eoh/1,
                         eob/1,
                         error_pkt/3,
                         make_tests/2]).


%% ====================================================================
%% Coverage
%% ====================================================================

coverage() ->
    ?MODULE_COVERAGE("lib/pipeline", stage_http_pkt_encoder),
    ok.


%% ====================================================================
%% Unit Tests
%% ====================================================================

binary_encoding_test_() ->
    {ok, E} = stage_http_pkt_encoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, iodata, [E]),
    Check = fun(Exp, Val) ->
                    ?assertEqual(Exp, [erlang:iolist_to_binary(Val)]) end,
    MkFun = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, Check, Dec, Enc) end,
    make_tests(binary, MkFun).

alternative_encoding_test_() ->
    V = binary,
    {ok, E} = stage_http_pkt_encoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, iodata, [E]),
    Check = fun(Exp, Val) ->
                    ?assertEqual(Exp, [erlang:iolist_to_binary(Val)]) end,
    [?MK_TEST_CONSUME(P, Check,
                      [response(V, ok, 1, 0), eoh(V), eob(V)],
                      [<<"HTTP/1.0 200 OK\r\n\r\n">>]),
     ?MK_TEST_CONSUME(P, Check,
                      [response(V, not_found, 1, 0), eoh(V), eob(V)],
                      [<<"HTTP/1.0 404 Not Found\r\n\r\n">>])
     ].

error_encoding_test_() ->
    V = binary,
    {ok, E} = stage_http_pkt_encoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, iodata, [E]),
    Check = fun(Exp, Val) ->
                    ?assertEqual(Exp, [erlang:iolist_to_binary(Val)]) end,
    [?MK_TEST_CONSUME(P, Check,
                      [error_pkt(V, unexpected, "spam"),
                       request(V, 'GET', "/", 1, 0),
                       error_pkt(V, unexpected, "bacon"),
                       eoh(V), eob(V)],
                      [<<"GET / HTTP/1.0\r\n\r\n">>]),
     ?MK_TEST_CONSUME(P, Check,
                      [request(V, 'GET', "/", 1, 0),
                       error_pkt(V, bad_request, {bad_header,
                                              'Content-Length', "X"}),
                       header(V, 'Content-Type', "text/html"),
                       eoh(V), eob(V)],
                      [<<"GET / HTTP/1.0\r\n">>])
     ].
