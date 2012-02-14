%% ===========================================================================
%% @doc        Module stage_packet_decoder unit tests.
%% @since      Jan 26, 2012
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
-module(test_stage_packet_decoder).

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
    ?MODULE_COVERAGE("", stage_http_pkt_decoder),
    ok.


%% ====================================================================
%% Unit Tests
%% ====================================================================

line_decoding_test_() ->
    make_line_tests(binary, binary)
    ++ make_line_tests(binary, list)
    ++ make_line_tests(list, binary)
    ++ make_line_tests(list, list)
    ++ make_iodata_line_tests(binary)
    ++ make_iodata_line_tests(list)
    .

make_line_tests(In, Out) ->
    P = make_pipe_(In, {Out, line}),
    C = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    F = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, C, Enc, Dec) end,
    I = fun(V) -> mk_var_(In, V) end,
    O = fun(V) -> mk_var_(Out, V) end,
    [F([I("")], []),
     F([I("\n")], [O("\n")]),
     F([I("\n\n\n")], [O("\n"), O("\n"), O("\n")]),
     F([I("toto")], []),
     F([I("toto\n")], [O("toto\n")]),
     F([I("toto\ntata")], [O("toto\n")]),
     F([I("toto\ntata\n")], [O("toto\n"), O("tata\n")]),
     F([I("A\nB\n\nC\nD")], [O("A\n"), O("B\n"), O("\n"), O("C\n")]),
     F([I(""), I(""), I("")], []),
     F([I(""), I("\n"), I("")], [O("\n")]),
     F([I("\n"), I("\n\n")], [O("\n"), O("\n"), O("\n")]),
     F([I("to"), I("to")], []),
     F([I("to"), I("to\n")], [O("toto\n")]),
     F([I("to"), I("to\nta"), I("ta")], [O("toto\n")]),
     F([I("to"), I("to\nta"), I("ta"), I("\n")],
       [O("toto\n"), O("tata\n")]),
     F([I("A"), I("\nB\n"), I("\nC"), I("\n"), I("D")],
       [O("A\n"), O("B\n"), O("\n"), O("C\n")])
    ].

make_iodata_line_tests(Out) ->
    P = make_pipe_(iodata, {Out, line}),
    C = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    F = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, C, Enc, Dec) end,
    O = fun(V) -> mk_var_(Out, V) end,
    [F([""], []),
     F([[""]], []),
     F([[""], <<"">>], []),
     F([<<"\n">>, ["\n", <<"\n">>]],
       [O("\n"), O("\n"), O("\n")]),
     F([[<<"A\nB">>, ["\n\n"]], "C\nD"],
       [O("A\n"), O("B\n"), O("\n"), O("C\n")])
    ].

block_decoding_test_() ->
    make_block_tests(binary, binary)
    ++ make_block_tests(binary, list)
    ++ make_block_tests(list, binary)
    ++ make_block_tests(list, list)
    .

make_block_tests(In, Out) ->
    P = make_pipe_(In, {Out, {block, 3}}),
    C = fun(Exp, Val) -> ?assertEqual(Exp, Val) end,
    F = fun(Enc, Dec) -> ?MK_TEST_CONSUME(P, C, Enc, Dec) end,
    I = fun(V) -> mk_var_(In, V) end,
    O = fun(V) -> mk_var_(Out, V) end,
    [F([I("")], []),
     F([I("X")], []),
     F([I("XX")], []),
     F([I("XXX")], [O("XXX")]),
     F([I("XXXY")], [O("XXX")]),
     F([I("XXXYY")], [O("XXX")]),
     F([I("XXXYYY")], [O("XXX"), O("YYY")]),
     F([I("XXXYYYZ")], [O("XXX"), O("YYY")]),
     F([I("X"), I("X"), I("X"), I("Y")], [O("XXX")]),
     F([I("X"), I("X"), I("X"), I("Y"), I("Y"), I("Y"), I("Z")],
       [O("XXX"), O("YYY")])
    ].


negotiation_test() ->
    N = fun negotiate_/1,

    % test passthrough negotiations
    ?assertMatch([{binary, block}, {binary, block}], N(any)),
    ?assertMatch([{packet, 1}, {packet, 1}], N({packet, 1})),
    ?assertMatch([{packet, 2}, {packet, 2}], N({packet, 2})),
    ?assertMatch([{packet, 4}, {packet, 4}], N({packet, 4})),
    ?assertMatch([{packet, asN}, {packet, asN}], N({packet, asN})),
    ?assertMatch([{packet, cdr}, {packet, cdr}], N({packet, cdr})),
    ?assertMatch([{packet, sunrm}, {packet, sunrm}], N({packet, sunrm})),
    ?assertMatch([{packet, fcgi}, {packet, fcgi}], N({packet, fcgi})),
    ?assertMatch([{packet, tpkt}, {packet, tpkt}], N({packet, tpkt})),
    ?assertMatch([{binary, line}, {binary, line}], N({binary, line})),
    ?assertMatch([{http, list}, {http, list}], N({http, list})),
    ?assertMatch([{http, binary}, {http, binary}], N({http, binary})),
    ?assertMatch([{binary, block}, {binary, block}], N(binary)),
    ?assertMatch([{list, block}, {list, block}], N(list)),
    ?assertMatch([{iodata, block}, {iodata, block}], N(iodata)),
    ?assertMatch([{binary, block}, {binary, block}], N({binary, block})),
    ?assertMatch([{list, block}, {list, block}], N({list, block})),
    ?assertMatch([{iodata, block}, {iodata, block}], N({iodata, block})),
    ?assertMatch([{binary, {block, 2}}, {binary, {block, 2}}],
                 N({binary, {block, 2}})),
    ?assertMatch([{list, {block, 2}}, {list, {block, 2}}],
                 N({list, {block, 2}})),
    ?assertMatch([{iodata, {block, 2}}, {iodata, {block, 2}}],
                 N({iodata, {block, 2}})),

     check_negotiation(fun negotiate_/2, binary),
     check_negotiation(fun negotiate_/2, list),
     check_negotiation(fun negotiate_/2, iodata),

    ok.

check_negotiation(N, F) ->
    ?assertMatch([{F, block}, {binary, block}],
                 N(F, binary)),
    ?assertMatch([{F, block}, {list, block}],
                 N(F, list)),
    ?assertMatch([{F, block}, {iodata, block}],
                 N(F, iodata)),
    ?assertMatch([{F, block}, {binary, block}],
                 N(F, {binary, block})),
    ?assertMatch([{F, block}, {list, block}],
                 N(F, {list, block})),
    ?assertMatch([{F, block}, {iodata, block}],
                 N(F, {iodata, block})),
    ?assertMatch([{F, block}, {binary, line}],
                 N(F, {binary, line})),
    ?assertMatch([{F, block}, {list, line}],
                 N(F, {list, line})),
    ?assertMatch([{F, block}, {iodata, line}],
                 N(F, {iodata, line})),
    ?assertMatch([{F, block}, {binary, {packet, 1}}],
                 N(F, {binary, {packet, 1}})),
    ?assertMatch([{F, block}, {list, {packet, 1}}],
                 N(F, {list, {packet, 1}})),
    ?assertMatch([{F, block}, {iodata, {packet, 1}}],
                 N(F, {iodata, {packet, 1}})),
    ?assertMatch([{F, block}, {http, binary}],
                 N(F, {http, binary})),
    ?assertMatch([{F, block}, {http, list}],
                 N(F, {http, list})),

    ?assertMatch([{F, block}, {binary, block}],
                 N({F, block}, binary)),
    ?assertMatch([{F, block}, {list, block}],
                 N({F, block}, list)),
    ?assertMatch([{F, block}, {iodata, block}],
                 N({F, block}, iodata)),
    ?assertMatch([{F, block}, {binary, block}],
                 N({F, block}, {binary, block})),
    ?assertMatch([{F, block}, {list, block}],
                 N({F, block}, {list, block})),
    ?assertMatch([{F, block}, {iodata, block}],
                 N({F, block}, {iodata, block})),
    ?assertMatch([{F, block}, {binary, line}],
                 N({F, block}, {binary, line})),
    ?assertMatch([{F, block}, {list, line}],
                 N({F, block}, {list, line})),
    ?assertMatch([{F, block}, {iodata, line}],
                 N({F, block}, {iodata, line})),
    ?assertMatch([{F, block}, {binary, {packet, 1}}],
                 N({F, block}, {binary, {packet, 1}})),
    ?assertMatch([{F, block}, {list, {packet, 1}}],
                 N({F, block}, {list, {packet, 1}})),
    ?assertMatch([{F, block}, {iodata, {packet, 1}}],
                 N({F, block}, {iodata, {packet, 1}})),
    ?assertMatch([{F, block}, {http, binary}],
                 N({F, block}, {http, binary})),
    ?assertMatch([{F, block}, {http, list}],
                 N({F, block}, {http, list})),

    ?assertMatch([{F, {block, 2}}, {binary, {block, 2}}],
                 N({F, {block, 2}}, binary)),
    ?assertMatch([{F, {block, 2}}, {list, {block, 2}}],
                 N({F, {block, 2}}, list)),
    ?assertMatch([{F, {block, 2}}, {iodata, {block, 2}}],
                 N({F, {block, 2}}, iodata)),
    ?assertMatch([{F, {block, 2}}, {binary, {block, 2}}],
                 N({F, {block, 2}}, {binary, block})),
    ?assertMatch([{F, {block, 2}}, {list, {block, 2}}],
                 N({F, {block, 2}}, {list, block})),
    ?assertMatch([{F, {block, 2}}, {iodata, {block, 2}}],
                 N({F, {block, 2}}, {iodata, block})),
    ?assertMatch([{F, {block, 2}}, {binary, {block, 2}}],
                 N({F, {block, 2}}, {binary, {block, 2}})),
    ?assertMatch([{F, {block, 2}}, {list, {block, 2}}],
                 N({F, {block, 2}}, {list, {block, 2}})),
    ?assertMatch([{F, {block, 2}}, {iodata, {block, 2}}],
                 N({F, {block, 2}}, {iodata, {block, 2}})),
    ?assertMatch([{F, {block, 2}}, {binary, line}],
                 N({F, {block, 2}}, {binary, line})),
    ?assertMatch([{F, {block, 2}}, {list, line}],
                 N({F, {block, 2}}, {list, line})),
    ?assertMatch([{F, {block, 2}}, {iodata, line}],
                 N({F, {block, 2}}, {iodata, line})),
    ?assertMatch([{F, {block, 2}}, {binary, {packet, 1}}],
                 N({F, {block, 2}}, {binary, {packet, 1}})),
    ?assertMatch([{F, {block, 2}}, {list, {packet, 1}}],
                 N({F, {block, 2}}, {list, {packet, 1}})),
    ?assertMatch([{F, {block, 2}}, {iodata, {packet, 1}}],
                 N({F, {block, 2}}, {iodata, {packet, 1}})),
    ?assertMatch([{F, {block, 2}}, {http, binary}],
                 N({F, {block, 2}}, {http, binary})),
    ?assertMatch([{F, {block, 2}}, {http, list}],
                 N({F, {block, 2}}, {http, list})),
    
    ?assertMatch([{F, block}, {binary, {block, 10}}],
                 N(F, {binary, {block, 10}})),
    ?assertMatch([{F, block}, {binary, {block, 10}}],
                 N({F, block}, {binary, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {binary, {block, 10}}],
                 N({F, {block, 10}}, {binary, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {binary, {block, 20}}],
                 N({F, {block, 10}}, {binary, {block, 20}})),
    ?assertMatch([{F, {block, 10}}, {binary, {block, 5}}],
                 N({F, {block, 10}}, {binary, {block, 5}})),

    ?assertMatch([{F, block}, {list, {block, 10}}],
                 N(F, {list, {block, 10}})),
    ?assertMatch([{F, block}, {list, {block, 10}}],
                 N({F, block}, {list, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {list, {block, 10}}],
                 N({F, {block, 10}}, {list, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {list, {block, 20}}],
                 N({F, {block, 10}}, {list, {block, 20}})),
    ?assertMatch([{F, {block, 10}}, {list, {block, 5}}],
                 N({F, {block, 10}}, {list, {block, 5}})),

    ?assertMatch([{F, block}, {iodata, {block, 10}}],
                 N(F, {iodata, {block, 10}})),
    ?assertMatch([{F, block}, {iodata, {block, 10}}],
                 N({F, block}, {iodata, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {iodata, {block, 10}}],
                 N({F, {block, 10}}, {iodata, {block, 10}})),
    ?assertMatch([{F, {block, 10}}, {iodata, {block, 20}}],
                 N({F, {block, 10}}, {iodata, {block, 20}})),
    ?assertMatch([{F, {block, 10}}, {iodata, {block, 5}}],
                 N({F, {block, 10}}, {iodata, {block, 5}})),

    ok.


%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

negotiate_(OutFmts) ->
    P = make_pipe_(OutFmts),
    {ok, Fmts} = P:get_formats(),
    Fmts.

negotiate_(InFmts, OutFmts) ->
    P = make_pipe_(InFmts, OutFmts),
    {ok, Fmts} = P:get_formats(),
    Fmts.

make_pipe_(InFmts, OutFmts) ->
    {ok, D} = stage_packet_decoder:new(),
    {ok, P} = twerl_pipeline:new(InFmts, OutFmts, [D]),
    P.

make_pipe_(OutFmts) ->
    {ok, D} = stage_packet_decoder:new(),
    {ok, P} = twerl_pipeline:new(OutFmts, [D]),
    P.

mk_var_(list, Value) -> Value;
mk_var_(binary, Value) -> erlang:list_to_binary(Value).
