%% ===========================================================================
%% @doc        Module twerl_pipeline and twerl_stage unit tests.
%% @since      Apr 24, 2010
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
-module(test_twerl_pipeline).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_max_level(debug).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("pipeline_test_utils.hrl").


%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(dummy_stage, [new_basic/2,
                      new_negotiator/2,
                      new_flattener/2,
                      new_pairmaker/2,
                      new_adder/2,
                      new_moreorlesser/2,
                      new_negator/0,
                      new_upstream_format_switcher/3,
                      new_downstream_format_switcher/3,
                      new_basic_producer/2,
                      new_number_producer/2]).


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([coverage/0]).


%% ====================================================================
%% Coverage
%% ====================================================================

coverage() ->
    ?MODULE_COVERAGE("lib/pipeline", twerl_pipeline),
    ok.


%% ====================================================================
%% Unit Tests
%% ====================================================================

downstream_format_switching_test_() ->
    {ok, BP} = new_basic_producer(pos, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
    {ok, NG} = new_negator(),
    Switches = [{1, neg}, {3, pos}, {5, neg}, {7, pos}, {11, neg}],
    {ok, DS} = new_downstream_format_switcher(pos, pos, Switches),
    {ok, AD} = new_adder([pos, neg], pos),

    [
     test_producing([BP], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
     test_producing([BP, NG], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
     test_producing_chain([BP, DS, NG],
                          ['query', pos, pos, pos],
                          ['query', pos, neg, pos],
                          [-1, -2, 3, 4, -5, -6, 7, 8, 9, 10, -11, -12]),
     test_producing_chain([BP, DS, NG, AD],
                          ['query', pos, pos, pos, pos],
                          ['query', pos, neg, pos, pos],
                          [-1-2, 3+4, -5-6, 7+8, 9+10, -11-12])
    ].

upstream_format_switching_test_() ->
    {ok, NP} = new_number_producer(pos, 12),
    {ok, NG} = new_negator(),
    Switches = [{1, neg}, {3, neg}, {5, neg}, {7, neg}, {11, neg},
                {-1, pos}, {-3, pos}, {-5, pos}, {-7, pos}, {-11, pos}],
    {ok, US} = new_upstream_format_switcher(pos, pos, Switches),
    {ok, AD} = new_adder([pos, neg], int),

    [
     test_producing([NP], [12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]),
     test_producing([NP, NG], [12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]),
     test_producing_chain([NP, US], ['query', pos, pos], ['query', neg, pos],
                          [12, 11, -10, -9, -8, -7, 6, 5, -4, -3, 2, 1]),
     test_producing_chain([NP, US, AD],
                          ['query', pos, pos, int],
                          ['query', neg, pos, int],
                          [12+11, -10-9, -8-7, 6+5, -4-3, 2+1]),
     test_producing_chain([NP, NG, US],
                          ['query', pos, pos, pos],
                          ['query', pos, neg, pos],
                          [12, 11, -10, -9, -8, -7, 6, 5, -4, -3, 2, 1]),
     test_producing_chain([NP, NG, US, AD],
                          ['query', pos, pos, pos, int],
                          ['query', pos, neg, pos, int],
                          [12+11, -10-9, -8-7, 6+5, -4-3, 2+1])
    ].

normal_production_test_() ->
    %% Dummy stages:
    %% A: [M, N], [O, P, Q] -> M, N, O, P, Q
    {ok, A} = new_flattener(none, none),
    %% B: M, N, O, P, Q -> M+N, O+P
    {ok, B} = new_adder(none, none),
    %% C: M, N, O -> M+7, M-3, N+7, N-3, O+7, O-3
    {ok, C} = new_moreorlesser(none, none),
    %% D: M, N, O, P, Q -> [M, N], [O, P]
    {ok, D} = new_pairmaker(none, none),

    [
     test_producing([A],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
     test_producing([B],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [1+2, 3+4, 5+6, 7+8, 9+10]),
     test_producing([C],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [1+7, 1-3, 2+7, 2-3, 3+7, 3-3, 4+7, 4-3, 5+7, 5-3,
                      6+7, 6-3, 7+7, 7-3, 8+7, 8-3, 9+7, 9-3, 10+7, 10-3]),
     test_producing([D],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]),

     test_producing([A, B],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [1+2, 3+4, 5+6, 7+8, 9+10]),
     test_producing([A, C],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [1+7, 1-3, 2+7, 2-3, 3+7, 3-3, 4+7, 4-3, 5+7, 5-3,
                      6+7, 6-3, 7+7, 7-3, 8+7, 8-3, 9+7, 9-3, 10+7, 10-3]),
     test_producing([A, D],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]),
     test_producing([B, C],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [(1+2+7), (1+2-3), (3+4+7), (3+4-3),
                      (5+6+7), (5+6-3), (7+8+7), (7+8-3),
                      (9+10+7), (9+10-3)]),
     test_producing([B, D],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [[1+2, 3+4], [5+6, 7+8]]),
     test_producing([C, D],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [[1+7, 1-3], [2+7, 2-3], [3+7, 3-3], [4+7, 4-3],
                      [5+7, 5-3], [6+7, 6-3], [7+7, 7-3], [8+7, 8-3],
                      [9+7, 9-3], [10+7, 10-3]]),
     test_producing([D, A],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                     [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),

     test_producing([A, B, C],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [(1+2+7), (1+2-3), (3+4+7), (3+4-3),
                      (5+6+7), (5+6-3), (7+8+7), (7+8-3),
                      (9+10+7), (9+10-3)]),

     test_producing([A, B, C, D],
                     [[1, 2, 3], [4], [5, 6, 7, 8], [9, 10]],
                     [[(1+2+7), (1+2-3)], [(3+4+7), (3+4-3)],
                      [(5+6+7), (5+6-3)], [(7+8+7), (7+8-3)],
                      [(9+10+7), (9+10-3)]])

     ].

simple_negotiation_test_() ->
    {ok, A} = new_basic('query', list),
    {ok, B} = new_basic(list, binary),
    {ok, C} = new_basic(binary, none),

    [
     test_basic_negotiation_success([A, B, C], 'query', none,
                                    [binary, list], [binary, list]),

     test_basic_negotiation_success([A, B], 'query', binary,
                                    [binary, list], [none, list]),

     test_basic_negotiation_success([B, C], list, none,
                                    [binary, 'query'], [binary, list]),

     test_basic_negotiation_failure([A, B, C], [binary, list], [binary, list]),

     test_basic_negotiation_failure([A, B], [binary, list], [none, list]),

     test_basic_negotiation_failure([B, C], [binary, 'query'], [binary, list])
    ].


multiple_negotiation_test_() ->
    %% Stage formats:
    %% A: x -> x, y -> z, z -> y
    %% B: x -> x, y -> y or z, z -> x
    %% C: x -> x or y, y -> z, z -> y
    {ok, A} = new_negotiator([{any, [x, y, z]}, {x, x}, {y, z}, {z, y}],
                             [{{x, any}, x}, {{y, any}, z}, {{z, any}, y},
                              {{x, x}, x}, {{y, z}, z}, {{z, y}, y}]),
    {ok, B} = new_negotiator([{any, [x, y, z]}, {x, [x, z]}, {y, y}, {z, y}],
                             [{{x, any}, x}, {{y, any}, y}, {{z, any}, x},
                              {{x, x}, x}, {{y, y}, y}, {{y, z}, z}, {{z, x}, x}]),
    {ok, C} = new_negotiator([{any, [x, y, z]}, {x, x}, {y, [x, z]}, {z, y}],
                             [{{x, any}, x}, {{y, any}, z}, {{z, any}, y},
                              {{x, x}, x}, {{x, y}, y}, {{y, z}, z}, {{z, y}, y}]),

    [
     test_chain_negotiation_success([A, B, C], x, y, [x, x, x, y]),
     test_chain_negotiation_success([A, B, C], x, x, [x, x, x, x]),
     test_chain_negotiation_success([A, B, C], y, y, [y, z, x, y]),
     test_chain_negotiation_success([A, B, C], y, x, [y, z, x, x]),
     test_chain_negotiation_success([A, B, C], z, y, [z, y, z, y]),
     test_chain_negotiation_success([A, B, C], z, z, [z, y, y, z]),

     test_chain_negotiation_success([A, B], x, x, [x, x, x]),
     test_chain_negotiation_success([A, B], y, x, [y, z, x]),
     test_chain_negotiation_success([A, B], z, z, [z, y, z]),
     test_chain_negotiation_success([A, B], z, y, [z, y, y]),

     test_chain_negotiation_success([B, C], x, y, [x, x, y]),
     test_chain_negotiation_success([B, C], x, x, [x, x, x]),
     test_chain_negotiation_success([B, C], z, y, [z, x, y]),
     test_chain_negotiation_success([B, C], z, x, [z, x, x]),
     test_chain_negotiation_success([B, C], y, y, [y, z, y]),
     test_chain_negotiation_success([B, C], y, z, [y, y, z]),

     test_chain_negotiation_success([A, C], x, x, [x, x, x]),
     test_chain_negotiation_success([A, C], x, y, [x, x, y]),
     test_chain_negotiation_success([A, C], y, y, [y, z, y]),
     test_chain_negotiation_success([A, C], z, z, [z, y, z]),

     test_basic_negotiation_failure([A, B, C], [x, q], [z, q]),
     test_basic_negotiation_failure([A, B, C], [y, q], [z, q]),
     test_basic_negotiation_failure([A, B, C], [z, q], [x, q]),

     test_basic_negotiation_failure([A, B], [x, q], [y, q]),
     test_basic_negotiation_failure([A, B], [x, q], [z, q]),
     test_basic_negotiation_failure([A, B], [y, q], [y, q]),
     test_basic_negotiation_failure([A, B], [y, q], [z, q]),
     test_basic_negotiation_failure([A, B], [z, q], [x, q]),

     test_basic_negotiation_failure([B, C], [x, q], [z, q]),
     test_basic_negotiation_failure([B, C], [y, q], [x, q]),
     test_basic_negotiation_failure([B, C], [z, q], [z, q]),

     test_basic_negotiation_failure([A, C], [x, q], [z, q]),
     test_basic_negotiation_failure([A, C], [y, q], [x, q]),
     test_basic_negotiation_failure([A, C], [y, q], [z, q]),
     test_basic_negotiation_failure([A, C], [z, q], [x, q]),
     test_basic_negotiation_failure([A, C], [z, q], [y, q]),

     test_chain_negotiation_success([A, B, C], any, x, [x, x, x, x]),
     test_chain_negotiation_success([A, B, C], any, y, [x, x, x, y]),
     test_chain_negotiation_success([A, B, C], any, z, [z, y, y, z]),
     test_chain_negotiation_success([A, B, C], x, any, [x, x, x, x]),
     test_chain_negotiation_success([A, B, C], y, any, [y, z, x, x]),
     test_chain_negotiation_success([A, B, C], z, any, [z, y, y, z]),

     test_chain_negotiation_success([A, B], any, x, [x, x, x]),
     test_chain_negotiation_success([A, B], any, y, [z, y, y]),
     test_chain_negotiation_success([A, B], any, z, [z, y, z]),
     test_chain_negotiation_success([A, B], x, any, [x, x, x]),
     test_chain_negotiation_success([A, B], y, any, [y, z, x]),
     test_chain_negotiation_success([A, B], z, any, [z, y, y]),

     test_chain_negotiation_success([B, C], any, x, [x, x, x]),
     test_chain_negotiation_success([B, C], any, y, [x, x, y]),
     test_chain_negotiation_success([B, C], any, z, [y, y, z]),
     test_chain_negotiation_success([B, C], x, any, [x, x, x]),
     test_chain_negotiation_success([B, C], y, any, [y, y, z]),
     test_chain_negotiation_success([B, C], z, any, [z, x, x]),

     test_chain_negotiation_success([A, C], any, x, [x, x, x]),
     test_chain_negotiation_success([A, C], any, y, [x, x, y]),
     test_chain_negotiation_success([A, C], any, z, [z, y, z]),
     test_chain_negotiation_success([A, C], x, any, [x, x, x]),
     test_chain_negotiation_success([A, C], y, any, [y, z, y]),
     test_chain_negotiation_success([A, C], z, any, [z, y, z]),

     test_multiple_negotiation_success([A, B, C], x, [x, y], [x, x, x, x]),
     test_multiple_negotiation_success([A, B, C], x, [y, x], [x, x, x, y]),
     test_multiple_negotiation_success([A, B, C], y, [x, y], [y, z, x, x]),
     test_multiple_negotiation_success([A, B, C], y, [y, x], [y, z, x, y]),
     test_multiple_negotiation_success([A, B, C], z, [y, z], [z, y, z, y]),
     test_multiple_negotiation_success([A, B, C], z, [z, y], [z, y, y, z]),

     %% Input formats list order is not relevent
     test_multiple_negotiation_success([A, B, C], [x, y], x, [x, x, x, x]),
     test_multiple_negotiation_success([A, B, C], [y, x], x, [x, x, x, x]),
     test_multiple_negotiation_success([A, B, C], [x, z], y, [x, x, x, y]),
     test_multiple_negotiation_success([A, B, C], [z, x], y, [x, x, x, y]),
     test_multiple_negotiation_success([A, B, C], [y, z], z, [z, y, y, z]),
     test_multiple_negotiation_success([A, B, C], [z, y], z, [z, y, y, z])

    ].


%% ====================================================================
%% Internal Functions
%% ====================================================================

mk_negotiation_success_checker(In, Out) ->
    fun(Result) ->
            ?assertMatch({ok, _}, Result),
            {ok, P} = Result,
            CanIn = stage_formats:canonical(In),
            CanOut = stage_formats:canonical(Out),
            ?assertEqual({ok, CanIn}, twerl_pipeline:get_input(P)),
            ?assertMatch(?PIPELINE_MATCH_INPUT(CanIn), P),
            ?assertEqual({ok, CanOut}, twerl_pipeline:get_output(P)),
            ?assertMatch(?PIPELINE_MATCH_OUTPUT(CanOut), P)
    end.

mk_negotiation_success_checker(Chain) ->
    fun(Result) ->
            ?assertMatch({ok, _}, Result),
            {ok, P} = Result,
            ?assertEqual({ok, Chain}, twerl_pipeline:get_formats(P))
    end.

mk_negotiation_failure_checker() ->
    fun(Result) -> ?assertMatch({error, not_negotiated}, Result) end.

mk_production_checker(Outputs) ->
    fun(Result) ->
            ?assertMatch({ok, _}, Result),
            {ok, Pipe} = Result,
            check_running(Pipe, Outputs)
    end.

mk_production_checker(Outputs, BeforeChain, AfterChain) ->
    fun(Result) ->
            ?assertMatch({ok, _}, Result),
            {ok, Pipe} = Result,
            ?assertEqual({ok, BeforeChain}, twerl_pipeline:get_formats(Pipe)),
            NewPipe = check_running(Pipe, Outputs),
            ?assertEqual({ok, AfterChain}, twerl_pipeline:get_formats(NewPipe))
    end.

mk_pipeline_maker(Checker, Stages) ->
    fun(InFmts, OutFmts) ->
            Checker(twerl_pipeline:new(InFmts, OutFmts, Stages))
    end.

mk_combinations_test(Checker, A, B) ->
    fun() -> check_combinations(Checker, A, B, B) end.

mk_test(Checker, InFmts, OutFmts) ->
    fun() -> Checker(InFmts, OutFmts) end.

check_running(Pipe, Outputs) ->
    RunResult = twerl_pipeline:run(Pipe),
    ?assertMatch({ok, _, _}, RunResult),
    {ok, RunOutput, NewPipe} = RunResult,
    ?assertEqual(Outputs, RunOutput),
    StepResult = step_loop(Pipe, []),
    ?assertMatch({ok, _, _}, StepResult),
    {ok, StepOutput, NewPipe} = StepResult,
    ?assertEqual(Outputs, StepOutput),
    NewPipe.

step_loop(Pipe, Acc) -> handle_step_result(twerl_pipeline:push(next, Pipe), Acc).

handle_step_result({data, D, _F, Pipe}, Acc) ->
    handle_step_result(twerl_pipeline:pull(next, Pipe), [D |Acc]);
handle_step_result({consumed, Pipe}, Acc) ->
    handle_step_result(twerl_pipeline:pull(next, Pipe), Acc);
handle_step_result({eos, Pipe}, Acc) ->
    {ok, lists:reverse(Acc), Pipe};
handle_step_result(Any, _Acc) -> Any.

check_combinations(_, [], _, _) -> ok;
check_combinations(Checker, [_ |AT], B, []) ->
    check_combinations(Checker, AT, B, B);
check_combinations(Checker, [AH |_] = A, B, [BH |BT]) ->
    Checker(AH, BH),
    check_combinations(Checker, A, B, BT).

combine_success_formats(F, [O]) ->
    [any, F, [F], [F, O], [O, F]];
combine_success_formats(F, [O1, O2]) ->
    [any, F, [F],
     [F, O1], [O1, F], [F, O2], [O2, F],
     [F, O1, O2], [O1, F, O2], [O1, O2, F],
     [F, O2, O1], [O2, F, O1], [O2, O1, F]].

combine_failure_formats([O]) ->
    [O, [O]];
combine_failure_formats([O1, O2]) ->
    [O1, O2, [O1], [O2],
     [O1, O2], [O1, O2], [O1, O2],
     [O2, O1], [O2, O1], [O2, O1]].

combine_formats([O1, O2]) ->
    [[O1, O2], [O1, O2], [O1, O2],
     [O2, O1], [O2, O1], [O2, O1]].

test_basic_negotiation_success(Stages, InFmt, OutFmt, BadIn, BadOut) ->
    Checker = mk_negotiation_success_checker(InFmt, OutFmt),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_combinations_test(Maker, combine_success_formats(InFmt, BadIn),
                         combine_success_formats(OutFmt, BadOut)).

test_chain_negotiation_success(Stages, InFmt, OutFmt, Chain) ->
    Checker = mk_negotiation_success_checker(Chain),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_combinations_test(Maker, combine_formats([InFmt, w]),
                         combine_formats([OutFmt, w])).

test_basic_negotiation_failure(Stages, OtherIn, OtherOut) ->
    Checker = mk_negotiation_failure_checker(),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_combinations_test(Maker, combine_failure_formats(OtherIn),
                         combine_failure_formats(OtherOut)).

test_multiple_negotiation_success(Stages, InFmts, OutFmts, FmtsChain) ->
    Checker = mk_negotiation_success_checker(FmtsChain),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_test(Maker, InFmts, OutFmts).

test_producing(Stages, Inputs, Outputs) ->
    {ok, Producer} = new_basic_producer(none, Inputs),
    test_producing([Producer |Stages], Outputs).

test_producing(Stages, Outputs) ->
    Checker = mk_production_checker(Outputs),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_test(Maker, 'query', any).

test_producing_chain(Stages, BeforeChain, AfterChain, Outputs) ->
    Checker = mk_production_checker(Outputs, BeforeChain, AfterChain),
    Maker = mk_pipeline_maker(Checker, Stages),
    mk_test(Maker, 'query', any).
