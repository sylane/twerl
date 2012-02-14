%% ===========================================================================
%% @doc        Module stage_http_pkt_decoder unit tests.
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
-module(test_stage_http_pkt_decoder).

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
                         header/3,
                         eoh/1,
                         body/2,
                         eob/1,
                         error_pkt/3,
                         make_tests/2]).


%% ====================================================================
%% Coverage
%% ====================================================================

coverage() ->
    ?MODULE_COVERAGE("lib/pipeline", stage_http_pkt_decoder),
    ok.


%% ====================================================================
%% Unit Tests
%% ====================================================================

binary_decoding_test_() ->
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    MkFun = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, Check, Enc, Dec) end,
    make_tests(binary, MkFun).

list_decoding_test_() ->
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, list}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    MkFun = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, Check, Enc, Dec) end,
    make_tests(list, MkFun).

header_case_decoding_test_() ->
    V = binary,
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    [?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "user-agent: foo\r\n",
                         "referer: http://dummy/url\r\n\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'User-Agent', <<"foo">>),
                       header(V, 'Referer', <<"http://dummy/url">>),
                       eoh(V), eob(V)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "user-Agent: foo\r\n",
                         "reFereR: dummy\r\n\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'User-Agent', <<"foo">>),
                       header(V, 'Referer', <<"dummy">>),
                       eoh(V), eob(V)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "UsEr-AgEnT: foo\r\n bar\r\n",
                         "Referer:    dummy   \r\n\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'User-Agent', <<"foo\r\n bar">>),
                       header(V, 'Referer', <<"dummy   ">>),
                       eoh(V), eob(V)])
    ].

http9_decoding_test_() ->
    V = binary,
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    [?MK_TEST_CONSUME(P, Check, [<<"GET /\r\n\r\n">>],
                      [request(V, 'GET', "/", 0, 9), eoh(V), eob(V)])
    ].

fragmented_decoding_test_() ->
    V = binary,
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    [?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "Content-Length: 16\r\n\r\n">>,
                       <<"AAAA">>, <<"BBBB">>, <<"CCCC">>, <<"DDDD">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'Content-Length', <<"16">>),
                       eoh(V),
                       body(V, "AAAA"), body(V, "BBBB"), body(V, "CCCC"), body(V, "DDDD"),
                       eob(V)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HT">>, <<"TP/1.0\r\n",
                                         "Content-">>, <<"Length: 4\r\n\r\n",
                                                         "AAAA">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'Content-Length', <<"4">>),
                       eoh(V), body(V, "AAAA"), eob(V)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0">>, <<"\r">>, <<"\n",
                                                         "Content-Length: 4\r\n">>, <<"\r\n",
                                                                                      "AAAA">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'Content-Length', <<"4">>),
                       eoh(V), body(V, "AAAA"), eob(V)])
    ].

error_decoding_test_() ->
    V = binary,
    {ok, D} = stage_http_pkt_decoder:new(),
    {ok, P} = twerl_pipeline:new({http_pkt, binary}, [D]),
    Check = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    [?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "Content-Length: foo\r\n\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       error_pkt(V, bad_request,
                             {bad_header, 'Content-Length', <<"foo">>})]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n\r\n\r\n",
                         "GET / HTTP/1.0\r\n\r\nbar\r\n">>],
                      [request(V, 'GET', "/", 1, 0), eoh(V), eob(V),
                       error_pkt(V, unexpected, <<"\r\n">>),
                       request(V, 'GET', "/", 1, 0), eoh(V), eob(V),
                       error_pkt(V, unexpected, <<"bar\r\n">>)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\nbaz\r\n\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       error_pkt(V, unexpected, <<"baz\r\n">>),
                       eoh(V), eob(V)]),
     ?MK_TEST_CONSUME(P, Check,
                      [<<"GET / HTTP/1.0\r\n",
                         "Content-Length: 0\r\n",
                         "foobar\r\n",
                         "User-Agent: spam\r\n",
                         "\r\n">>],
                      [request(V, 'GET', "/", 1, 0),
                       header(V, 'Content-Length', <<"0">>),
                       error_pkt(V, unexpected, <<"foobar\r\n">>),
                       header(V, 'User-Agent', <<"spam">>),
                       eoh(V), eob(V)])
    ].
