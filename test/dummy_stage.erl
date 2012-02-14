%% ===========================================================================
%% @doc        Dummy multi-faced stage used for unit testing.
%% @since      Mar 25, 2010
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

-module(dummy_stage).

-author('Sebastien Merle <s.merle@gmail.com>').


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([new_basic/2,
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

%% Pipeline callbacks exports
-export([init/1,
         negotiate/2,
         agree/3,
         setup/3,
         process/3,
         continue/3]).


%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(twerl_stage, [produce/3,
                      need_more/3,
                      negotiate_upstream/3,
                      negotiate_downstream/3,
                      finished/2]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Stage's state record name
-define(St, ?MODULE).

%% Uncomment for logging calls to the dummy stages
%-define(debug_calls, True).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {name, sub_state, init_fun, negotiate_fun, agree_fun,
              setup_fun, process_fun, continue_fun}).


%% ====================================================================
%% API Functions
%% ====================================================================

new_basic(InFmt, OutFmt) ->
    {ok, #?St{name = "basic",
              sub_state = nil,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = mk_dummy_process(),
              continue_fun = mk_dummy_continue()}}.

new_negotiator(NegLst, AgrLst) ->
    {ok, #?St{name = "negotiator",
              sub_state = nil,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_match_negotiate(NegLst),
              agree_fun = mk_match_agree(AgrLst),
              setup_fun = mk_dummy_setup(),
              process_fun = mk_dummy_process(),
              continue_fun = mk_dummy_continue()}}.

new_flattener(InFmt, OutFmt) ->
    % [M, N], [O, P, Q] -> M, N, O, P, Q
    Proc = fun([H |T], P, [], C) -> produce(H, C(T), P) end,
    Cont = fun(next, P, [H |T], C) -> produce(H, C(T), P);
              (next, P, [], C) -> need_more(next, C([]), P) end,
    {ok, #?St{name = "flattener",
              sub_state = [],
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = Cont}}.

new_pairmaker(InFmt, OutFmt) ->
    % M, N, O, P, Q -> [M, N], [O, P]
    Proc = fun(D, P, none, C) -> need_more(next, C(D), P);
              (D, P, L, C) -> produce([L, D], C(none), P) end,
    {ok, #?St{name = "pairmaker",
              sub_state = none,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = mk_dummy_continue_next()}}.

new_adder(InFmt, OutFmt) ->
    % M, N, O, P, Q -> M+N, O+P
    Proc = fun(D, P, none, C) -> need_more(next, C(D), P);
              (D, P, L, C) -> produce(L + D, C(none), P) end,
    {ok, #?St{name = "adder",
              sub_state = none,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = mk_dummy_continue_next()}}.

new_moreorlesser(InFmt, OutFmt) ->
    % M, N, O -> M+7, M-3, N+7, N-3, O+7, O-3
    Proc = fun(D, P, none, C) -> produce(D+7, C(D-3), P) end,
    Cont = fun(next, P, none, C) -> need_more(next, C(none), P);
              (next, P, D, C) -> produce(D, C(none), P) end,
    {ok, #?St{name = "moreorlesser",
              sub_state = none,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = Cont}}.

new_negator() ->
    % If OutFmt = pos: M, N, O -> M, N, O
    % If OutFmt = neg: M, N, O -> -M, -N, -O
    Setup = fun(I, O, _, C) -> {ok, C({I, O})} end,
    Proc = fun(D, P, {pos, pos} = S, C) -> produce(D, C(S), P);
              (D, P, {pos, neg} = S, C) -> produce(-D, C(S), P);
              (D, P, {neg, pos} = S, C) -> produce(-D, C(S), P);
              (D, P, {neg, neg} = S, C) -> produce(D, C(S), P) end,
    NegLst = [{any, [pos, neg]}, {neg, [pos, neg]}, {pos, [pos, neg]}],
    AgrLst = [{{pos, any}, pos}, {{neg, any}, neg},
              {{pos, pos}, pos}, {{pos, neg}, neg},
              {{neg, pos}, pos}, {{neg, neg}, neg}],
    {ok, #?St{name = "negator",
              sub_state = nil,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_match_negotiate(NegLst),
              agree_fun = mk_match_agree(AgrLst),
              setup_fun = Setup,
              process_fun = Proc,
              continue_fun = mk_dummy_continue()}}.

new_basic_producer(OutFmt, List) ->
    Proc = fun(next, P, [], C) -> finished(C([]), P);
              (next, P, [H |T], C) -> produce(H, C(T), P) end,
    {ok, #?St{name = "basic_producer",
              sub_state = List,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate('query', OutFmt),
              agree_fun = mk_basic_agree('query', OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = mk_dummy_continue_producing(Proc)}}.

new_number_producer(Kind, Num) when Kind =:= pos; Kind =:= neg; Num > 0 ->
    %% Can generate decrementing positive integer
    %% or incrementing negative integer.
    Setup = fun(_,K,{_,N},C) when K =:= pos; K =:= neg -> {ok, C({K, N})} end,
    Proc = fun(next, P, {_, 0} = S, C) ->
                   finished(C(S), P);
              (next, P, {pos, N}, C) ->
                   produce(N, C({pos, N - 1}), P);
              (next, P, {neg, N}, C) ->
                   produce(-N, C({neg, N - 1}), P)
           end,
    NegLst = [{any, 'query'}, {neg, 'query'}, {pos, 'query'}],
    AgrLst = [{{'query', any}, pos},
              {{'query', pos}, pos},
              {{'query', neg}, neg}],
    {ok, #?St{name = "number_producer",
              sub_state = {Kind, Num},
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_match_negotiate(NegLst),
              agree_fun = mk_match_agree(AgrLst),
              setup_fun = Setup,
              process_fun = Proc,
              continue_fun = mk_dummy_continue_producing(Proc)}}.

new_downstream_format_switcher(InFmt, OutFmt, Triggers) ->
    %% Pass-through stage that renegotiate downstream when triggered
    SwitchIf = fun({D, F}, D, P, T, C) ->
                       {ok, F, X, P2} = negotiate_downstream(F, C(T), P),
                       produce(D, X, P2);
                  (false, D, P, T, C) ->
                       produce(D, C(T), P)
               end,
    Proc = fun(D, P, T, C) ->
                   SwitchIf(lists:keyfind(D, 1, T), D, P, T, C)
           end,
    {ok, #?St{name = "downstream_switcher",
              sub_state = Triggers,
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = mk_dummy_continue()}}.

new_upstream_format_switcher(InFmt, OutFmt, Triggers) ->
    %% Pass-through stage that renegotiate upstream when triggered
    SwitchIf = fun({D, F}, D, P, T, C) ->
                       produce(D, C({F, T}), P);
                  (false, D, P, T, C) ->
                       produce(D, C({nil, T}), P)
               end,
    Proc = fun(D, P, {nil, T}, C) ->
                   SwitchIf(lists:keyfind(D, 1, T), D, P, T, C)
           end,
    Cont = fun(Q, P, {nil, _} = S, C) ->
                   need_more(Q, C(S), P);
              (Q, P, {F, T}, C) ->
                   {ok, F, X, P2} = negotiate_upstream(F, C({nil, T}), P),
                   need_more(Q, X, P2)
           end,
    {ok, #?St{name = "upstream_switcher",
              sub_state = {nil, Triggers},
              init_fun = mk_dummy_init(),
              negotiate_fun = mk_basic_negotiate(InFmt, OutFmt),
              agree_fun = mk_basic_agree(InFmt, OutFmt),
              setup_fun = mk_dummy_setup(),
              process_fun = Proc,
              continue_fun = Cont}}.


%% ====================================================================
%% Pipeline Callbacks Functions
%% ====================================================================

init(#?St{init_fun = Fun, sub_state = SState} = State) ->
    Constructor = fun(NewSState) -> State#?St{sub_state = NewSState} end,
    Fun(SState, Constructor).

-ifdef(debug_calls).
negotiate(OutFmt, #?St{negotiate_fun = Fun, sub_state = SState} = _State) ->
    Res = Fun(OutFmt, SState),
    erlog:log("~s:negotiate(~w) -> ~w", [_State#?St.name, OutFmt, Res]),
    Res.
-else.
negotiate(OutFmt, #?St{negotiate_fun = Fun, sub_state = SState}) ->
    Fun(OutFmt, SState).
-endif.

-ifdef(debug_calls).
agree(InFmt, OutFmt, #?St{agree_fun = Fun, sub_state = SState} = _State) ->
    Res = Fun(InFmt, OutFmt, SState),
    erlog:debug("~s:agree(~w,~w) -> ~w", [_State#?St.name, InFmt, OutFmt, Res]),
    Res.
-else.
agree(InFmt, OutFmt, #?St{agree_fun = Fun, sub_state = SState}) ->
    Fun(InFmt, OutFmt, SState).
-endif.

-ifdef(debug_calls).
setup(InFmt, OutFmt, #?St{setup_fun = Fun, sub_state = SState} = State) ->
    erlog:log("~s:setup(~w, ~w)", [State#?St.name, InFmt, OutFmt]),
    Constructor = fun(NewSState) -> State#?St{sub_state = NewSState} end,
    Fun(InFmt, OutFmt, SState, Constructor).
-else.
setup(InFmt, OutFmt, #?St{setup_fun = Fun, sub_state = SState} = State) ->
    Constructor = fun(NewSState) -> State#?St{sub_state = NewSState} end,
    Fun(InFmt, OutFmt, SState, Constructor).
-endif.

process(Data, Pipe, #?St{process_fun = Fun, sub_state = SState} = State) ->
    Constructor = fun(NewSState) -> State#?St{sub_state = NewSState} end,
    Fun(Data, Pipe, SState, Constructor).

continue(Query, Pipe, #?St{continue_fun = Fun, sub_state = SState} = State) ->
    Constructor = fun(NewSState) -> State#?St{sub_state = NewSState} end,
    Fun(Query, Pipe, SState, Constructor).


%% ====================================================================
%% Internal Funtions
%% ====================================================================

mk_dummy_init() ->
    fun(State, Constructor) -> {ok, Constructor(State)} end.

mk_basic_negotiate(DIF, DOF) ->
    CDOF = stage_formats:canonical(DOF),
    fun(any, _) -> DIF;
       (AOF, _)
         when AOF =:= CDOF -> DIF;
       (_, _) -> rejected
    end.

mk_basic_agree({_, {_, _}} = DIF, DOF) ->
    fun(AIF, any, _)
         when AIF =:= DIF -> DOF;
       (AIF, AOF, _)
         when AIF =:= DIF, AOF =:= DOF -> DOF;
       (AIF, {AOT, block} = AOF, _)
         when AIF =:= DIF, AOT =:= DOF -> AOF;
       (AIF, {AOT, {block, _}} = AOF, _)
         when AIF =:= DIF, AOT =:= DOF -> AOF;
       (_, _, _) -> rejected
    end;
mk_basic_agree({DIT, DIP} = DIF, DOF) ->
    fun(AIF, any, _)
         when AIF =:= DIF -> DOF;
       (AIF, AOF, _)
         when AIF =:= DIF, AOF =:= DOF -> DOF;
       ({AIT, {AIP, _}}, any, _)
         when AIT =:= DIT, AIP =:= DIP -> DOF;
       ({AIT, {AIP, _}}, AOF, _)
         when AIT =:= DIT, AIP =:= DIP, AOF =:= DOF -> DOF;
       (_, _, _) -> rejected
    end;
mk_basic_agree([_|_] = DIF, DOF) ->
    fun(AIF, _, _) ->
            case lists:member(AIF, DIF) of
                true -> DOF;
                false -> rejected
            end
    end;
mk_basic_agree(DIT, DOF) ->
    CDOF = stage_formats:canonical(DOF),
    fun(AIF, any, _)
         when AIF =:= DIT -> CDOF;
       (AIF, AOF, _)
         when AIF =:= DIT, AOF =:= CDOF -> CDOF;
       ({AIT, _}, any, _)
         when AIT =:= DIT -> DOF;
       ({AIT, _}, AOF, _)
         when AIT =:= DIT, AOF =:= CDOF -> CDOF;
       ({AIT, {_, _}}, any, _)
         when AIT =:= DIT -> DOF;
       ({AIT, {_, _}}, AOF, _)
         when AIT =:= DIT, AOF =:= CDOF -> CDOF;
       (_, _, _) -> rejected
    end.

mk_dummy_setup() ->
    fun(_, _, State, Constructor) -> {ok, Constructor(State)} end.

mk_dummy_process() ->
    fun(Data, Pipe, State, Constructor) ->
            produce(Data, Constructor(State), Pipe)
    end.

mk_dummy_continue() ->
    fun(Query, Pipe, State, Constructor) ->
            need_more(Query, Constructor(State), Pipe)
    end.

mk_dummy_continue_next() ->
    fun(next, Pipe, State, Constructor) ->
            need_more(next, Constructor(State), Pipe)
    end.

mk_dummy_continue_producing(ProduceFun) ->
    fun(next, Pipe, State, Constructor) ->
            ProduceFun(next, Pipe, State, Constructor)
    end.

mk_match_negotiate(Lst) ->
   fun(OutFmt, _) ->
           case lists:keyfind(OutFmt, 1, Lst) of
               false -> rejected;
               {_, Fmts} -> Fmts
           end
   end.

mk_match_agree(Lst) ->
   fun(InFmt, OutFmt, _) ->
           case lists:keyfind({InFmt, OutFmt}, 1, Lst) of
               false -> rejected;
               {_, Fmt} -> Fmt
           end
   end.
