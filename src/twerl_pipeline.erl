%% ===========================================================================
%% @doc        Data pipeline abstraction.
%% @since      Apr 18, 2010
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

-module(twerl_pipeline).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(pipeline).

% Set to log for negotiation logging
-erlog_max_level(debug).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("erlib/include/erlog.hrl").

-include("twerl_pipeline.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/1, new/2, new/3,
         run/1, run/2,
         consume/2,
         consume_packets/2,
         push/2,
         pull/2,
         negotiate/3,
         get_input/1,
         get_output/1,
         get_formats/1,
         get_stages/1]).

%% Private exports, used by twerl_stage
-export([negotiate_stages_/4,
         setup_stages_/4]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Like new/3 but with empty InFmts and OutFmts.

new(Stages) -> init_pipeline_(#?Pl{}, any, any, Stages).


%% --------------------------------------------------------------------
%% @doc Like new/3 but with empty InFmts.

new(OutFmts, Stages) -> init_pipeline_(#?Pl{}, any, OutFmts, Stages).


%% --------------------------------------------------------------------
%% @doc Creates a pipeline from a list of stages and initialize it
%% so the pipeline output and input formats are selected from
%% the specified lists.
%% The output formats list order give a priority between output formats,
%% but the input format order IS NOT relevent.

new(InFmts, OutFmts, Stages) ->
    init_pipeline_(#?Pl{}, InFmts, OutFmts, Stages).


%% --------------------------------------------------------------------
%% @doc Runs a pipeline until end-of-stream and returns the list of
%% all produced data packets.

run(Pipe) ->
    run_pipeline_(Pipe, next).


%% --------------------------------------------------------------------
%% @doc Runs a pipeline with the specified query until end-of-stream
%% and returns the list of all produced data packets.

run(Query, Pipe) ->
    run_pipeline_(Pipe, Query).


%% --------------------------------------------------------------------
%% @doc Makes the pipeline consume completely specified data and return
%% the list of produced data packets.

consume(Data, Pipe) ->
    consume_data_(Pipe, Data).


%% --------------------------------------------------------------------
%% @doc Makes the pipeline consume completely the specified list
%% of data packets and return the list of produced data packets.

consume_packets(Packets, Pipe) ->
    consume_packets_(Pipe, Packets).


%% --------------------------------------------------------------------
%% @doc Pushes specified data into the pipeline.

push(Input, Pipe) ->
    call_process_(first_(Pipe), Input).


%% --------------------------------------------------------------------
%% @doc Pull data from the pipeline using specified query.

pull(Query, Pipe) ->
    call_continue_(last_(Pipe), Query).


%% --------------------------------------------------------------------
%% @doc Negotiates pipeline input/output format.

negotiate(InFmts, OutFmts, Pipe) ->
    negotiate_(Pipe, as_list_(InFmts), as_list_(OutFmts)).


%% --------------------------------------------------------------------
%% @doc Gets pipeline input format.

get_input(Pipe) ->
    {ok, Pipe#?Pl.input}.


%% --------------------------------------------------------------------
%% @doc Gets pipeline output format

get_output(Pipe) ->
    {ok, Pipe#?Pl.output}.


%% --------------------------------------------------------------------
%% @doc Gets pipeline list of internally negotiated formats.

get_formats(Pipe) ->
    #?Pl{prev_fmts = Prevs, next_fmts = Nexts} = Pipe,
    {ok, lists:reverse(Prevs, Nexts)}.


%% --------------------------------------------------------------------
%% @doc Gets the pipeline stages.

get_stages(Pipe) ->
    #?Pl{prevs = Prevs, curr = Curr, nexts = Nexts} = Pipe,
    lists:reverse(Prevs, [Curr |Nexts]).


%% ====================================================================
%% Internal Functions
%% ====================================================================

as_list_([_|_] = L) -> L;
as_list_(O) -> [O].

first_(#?Pl{prevs = []} = Pipe) -> Pipe;
first_(Pipe) ->
    #?Pl{prev_fmts = PFmts, next_fmts = NFmts,
         prevs = Prevs, curr = Curr, nexts = Nexts} = Pipe,
    [NewPFmt |NewNFmts] = lists:reverse(PFmts, NFmts),
    [NewCurr |NewNexts] = lists:reverse(Prevs, [Curr |Nexts]),
    Pipe#?Pl{prev_fmts = [NewPFmt], next_fmts = NewNFmts,
             prevs = [], curr = NewCurr, nexts = NewNexts}.

last_(#?Pl{nexts = []} = Pipe) -> Pipe;
last_(Pipe) ->
    #?Pl{prev_fmts = PFmts, next_fmts = NFmts,
         prevs = Prevs, curr = Curr, nexts = Nexts} = Pipe,
    [NewNFmt |NewPFmts] = lists:reverse(NFmts, PFmts),
    [NewCurr |NewPrevs] = lists:reverse([Curr |Nexts], Prevs),
    Pipe#?Pl{prev_fmts = NewPFmts, next_fmts = [NewNFmt],
             prevs = NewPrevs, curr = NewCurr, nexts = []}.

init_pipeline_(Pipe, InFmts, OutFmts, Stages) ->
    case init_stages_(Pipe, as_list_(Stages)) of
        {ok, [H |T]} ->
            Pipe2 = Pipe#?Pl{prevs = [], curr = H, nexts = T},
            negotiate_(Pipe2, as_list_(InFmts), as_list_(OutFmts));
        Any -> Any
    end.

init_stages_(Pipe, Stages) ->
    init_stages_(Pipe, [], lists:reverse(Stages)).

init_stages_(_Pipe, Stages, []) ->
    {ok, Stages};
init_stages_(Pipe, Stages, [H |T]) ->
    case call_init_(Pipe, H) of
        {ok, Stage} ->
            init_stages_(Pipe, [Stage |Stages], T);
        Any -> Any
    end.

negotiate_(Pipe, InFmts, OutFmts) ->
    #?Pl{prevs = Prevs, curr = Curr, nexts = Nexts} = Pipe,
    InvStages = lists:reverse([Curr |Nexts], Prevs),
    case negotiate_stages_(Pipe, InFmts, OutFmts, InvStages) of
        {ok, RevFmts} ->
            [OutFmt |_] = RevFmts,
            case setup_stages_(Pipe, [], RevFmts, InvStages) of
                {ok, [FirstStage |NextsStages]} ->
                    Fmts = lists:reverse(RevFmts),
                    [InFmt |NFmts] = Fmts,
                    {ok, Pipe#?Pl{input = InFmt, output = OutFmt,
                                  prev_fmts = [InFmt],
                                  next_fmts = NFmts,
                                  prevs = [],
                                  curr = FirstStage,
                                  nexts = NextsStages}};
                Any -> Any
            end;
        Any -> Any
    end.

% Called by twerl_stage, so it's exported
negotiate_stages_(Pipe, InFmts, OutFmts, Stages) ->
    CanInFmts = stage_formats:canonical(ensure_list_(InFmts)),
    CanOutFmts = stage_formats:canonical(ensure_list_(OutFmts)),
    negotiate_output_(Pipe, CanInFmts, CanOutFmts, Stages).     

ensure_list_([_|_] = L) -> L;
ensure_list_(V) -> [V].

negotiate_output_(_Pipe, InFmts, OutFmts, []) ->
    % Not stages to negotiate with, choose the format from possible inputs
    erlog:log("Choosing INPUT format from ~w...", [InFmts]),
    case choose_format_(InFmts, OutFmts) of
        {ok, Fmt} ->
            erlog:log("Choosed ~w INPUT format.", [Fmt]),
            {ok, [Fmt]};
        {error, not_negotiated} ->
            erlog:log("No INPUT format supported."),
            {error, not_negotiated}
    end;
negotiate_output_(_Pipe, _InFmts, [], _Stages) ->
    % Not output format to negotiate, negotiation failed
    {error, not_negotiated};            
negotiate_output_(Pipe, InFmts, [OutFmt |OutFmts], Stages) ->
    % Now negotiate input format for select output format
    case negotiate_input_(Pipe, InFmts, OutFmt, Stages) of
        {ok, NegFmts} ->
            erlog:log("Upstream formats negotiated: ~w.", [NegFmts]),
            {ok, NegFmts};
        {error, not_negotiated} ->
            % Try next output format
            negotiate_output_(Pipe, InFmts, OutFmts, Stages)
    end.

negotiate_input_(Pipe, InFmts, OutFmt, [Stage |RemStages]) ->
    % Call stage negotiate to get the supported input format
    % for a given output format.
    erlog:log("Negotiating stage ~w INPUT for ~w OUTPUT...",
              [element(1, Stage), OutFmt]),
    case call_negotiate_(Pipe, Stage, OutFmt) of
        rejected ->
            % Output format not supported
            erlog:log("Stage ~w do not support ~w OUTPUT.",
                      [element(1, Stage), OutFmt]),
            {error, not_negotiated};
        StageInFmts ->
            % Move to output negotiation of the upstream stage
            CanInFmts = stage_formats:canonical(StageInFmts),
            erlog:log("Stage ~w supports INPUT formats ~w for ~w OUTPUT.",
                      [element(1, Stage), CanInFmts, OutFmt]),
            case negotiate_output_(Pipe, InFmts, CanInFmts, RemStages) of
                {error, not_negotiated} ->
                    % Upstream stages not negotiated
                    {error, not_negotiated};
                {ok, [InFmt |_] = FmtChain} ->
                    % Upstream stages nogotiation was successful.
                    % Agree with current stage on an output format.
                    erlog:log("Agreeing with ~w on OUTPUT for ~w INPUT...",
                              [element(1, Stage), InFmt]),
                    case call_agree_(Pipe, Stage, InFmt, OutFmt) of
                        rejected ->
                            erlog:log("Stage ~w did not agree on any OUTPUT "
                                      "for ~w INPUT.",
                                      [element(1, Stage), InFmt]),
                            {error, not_negotiated};
                        AgreedOutFmt ->
                            CanOutFmt = stage_formats:canonical(AgreedOutFmt),
                            erlog:log("Stage ~w agreed on ~w OUTPUT "
                                      "for ~w INPUT.",
                                      [element(1, Stage), CanOutFmt, InFmt]),
                            {ok, [CanOutFmt |FmtChain]}
                    end
            end
                
    end.

choose_format_([InFmt |InFmts], AllFmts) ->
    choose_output_format_(InFmts, AllFmts, InFmt, AllFmts).

choose_output_format_([], _, _, []) ->
    {error, not_negotiated};
choose_output_format_([Fmt |InFmts], AllFmts, _, []) ->
    choose_output_format_(InFmts, AllFmts, Fmt, AllFmts);
choose_output_format_(InFmts, AllFmts, InFmt, [OutFmt |RemFmt]) ->
    erlog:log("Validating ~w INPUT against ~w...",
              [InFmt, OutFmt]),
    case validate_format_(InFmt, OutFmt) of
        {ok, OutFmt2} ->
            erlog:log("Format ~w validated", [OutFmt2]),
            {ok, OutFmt2};
        continue ->
            choose_output_format_(InFmts, AllFmts, InFmt, RemFmt)
    end.

validate_format_(any, Fmt) -> {ok, Fmt};
validate_format_(Fmt, any) -> {ok, Fmt};
validate_format_(Fmt, Fmt) -> {ok, Fmt};
validate_format_({T, {block, _}} = Fmt, {T, block}) -> {ok, Fmt};
validate_format_(_, _) -> continue.

% Called by twerl_stage, so it's exported
setup_stages_(_Pipe, Acc, _Formats, []) ->
    {ok, Acc};
setup_stages_(Pipe, Acc, [OutFmt, InFmt|_] = Formats, [Stage |Stages]) ->
    case call_setup_(Pipe, Stage, InFmt, OutFmt) of
        {ok, NewStage} ->
            setup_stages_(Pipe, [NewStage |Acc], tl(Formats), Stages);
        Any -> Any
    end.

run_pipeline_(Pipe, Query) ->
    run_loop_(Pipe, Query, []).

run_loop_(Pipe, Query, Acc) ->
    run_handler_(pull(Query, Pipe), Acc).

run_handler_({data, Data, _Fmt, Pipe}, Acc) ->
    run_loop_(Pipe, next, [Data |Acc]);
run_handler_({more, next, message, Pipe}, Acc) ->
    %TODO: Add suport for timeout, and ignored messages
    receive Msg -> run_handler_(push(Msg, Pipe), Acc) end;
run_handler_({eos, Pipe}, Acc) ->
    {ok, lists:reverse(Acc), Pipe};
run_handler_({consumed, Pipe}, Acc) ->
    run_loop_(Pipe, next, Acc);
run_handler_(Any, _Acc) ->
    Any.

consume_data_(Pipe, Data) ->
    consume_handler_(push(Data, Pipe), []).

consume_handler_({data, Data, _Fmt, Pipe}, Acc) ->
    consume_handler_(pull(next, Pipe), [Data |Acc]);
consume_handler_({more, next, _Fmt, Pipe}, Acc) ->
    {more, lists:reverse(Acc), Pipe};
consume_handler_({consumed, Pipe}, Acc) ->
    consume_handler_(pull(next, Pipe), Acc);
consume_handler_({eos, Pipe}, Acc) ->
    {eos, lists:reverse(Acc), Pipe}.

consume_packets_(Pipe, Packets) ->
    consume_packets_(Pipe, Packets, []).

consume_packets_(Pipe, [], Acc) ->    
    {more, lists:append(lists:reverse(Acc)), Pipe};
consume_packets_(Pipe, [Packet |Packets], Acc) ->
    case consume_handler_(push(Packet, Pipe), []) of
        {more, Result, NewPipe} ->
            consume_packets_(NewPipe, Packets, [Result |Acc]);
        {eos, Result, NewPipe} ->
            {eos, lists:flatten(lists:reverse([Result |Acc])), NewPipe}
    end.

call_init_(_Pipe, Stage) ->
    Stage:init().

call_negotiate_(_Pipe, Stage, Fmt) ->
    % Supports unique format or a list of formats 
    case Stage:negotiate(Fmt) of 
        rejected -> rejected;
        [_|_] = Result -> Result;
        Result -> [Result]
    end.

call_agree_(_Pipe, Stage, InFmt, OutFmt) ->
    Stage:agree(InFmt, OutFmt).

call_setup_(_Pipe, Stage, InFmt, OutFmt) ->
    Stage:setup(InFmt, OutFmt).

call_process_(#?Pl{curr = Stage} = Pipe, Data) ->
    Stage:process(Data, Pipe).

call_continue_(#?Pl{curr = Stage} = Pipe, Query) ->
    Stage:continue(Query, Pipe).

