%% ===========================================================================
%% @doc        Set of functions called from inside a twerl stage
%%             that could alter pipeline internal state.
%% @since      May 28, 2010
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

-module(super_pipeline).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(stage).

% Set to log for extra logging
-erlog_max_level(debug).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("erlib/include/erlog.hrl").

-include("twerl_pipeline.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% Stage callback exports
-export([produce/3,
         consumed/2,
         ignore/3,
         need_more/3,
         pull/3,
         negotiate_upstream/3,
         negotiate_downstream/3,
         finished/2,
         failed/3]).


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type super() :: #?Pl{}.


%% ====================================================================
%% Stage callbacks Functions, only to be called from inside a stage
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Makes the stage produce some data.

-spec produce(Super::super(), Stage::twerl_stage(),
              Data::twerl_stage_data()) ->
          {data, twerl_stage_data(), twerl_stage_format(), super()}
        | {consumed, super()}
        | {more, twerl_stage_query(), twerl_stage_format(), super()}
        | {eos, super()}.

produce(#?Pl{target = Targ, nexts = [Targ |_]} = Super, Stage, Data) ->
    % The target of the data is the next stage
    #?Pl{prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
         prevs = Prevs, nexts = [Next |Nexts]} = Super,
    NewSuper = Super#?Pl{target = undefined,
                         prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
                         prevs = [Stage |Prevs], curr = Next, nexts = Nexts},
    {data, Data, Fmt, NewSuper};
produce(#?Pl{nexts = []} = Super, Stage, Data) ->
	% No more downstream stage to give the data, just return it
    #?Pl{next_fmts = [Fmt]} = Super,
    {data, Data, Fmt, Super#?Pl{curr = Stage}};
produce(Super, Stage, Data) ->
	% Pass data to the next downstream stage
    #?Pl{prev_fmts = PFmts, next_fmts = [Fmt  |NFmts],
         prevs = Prevs, nexts = [Next |Nexts]} = Super,
    NewSuper = Super#?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
                         prevs = [Stage |Prevs], curr = Next, nexts = Nexts},
    call_process_(NewSuper, Data).


%% --------------------------------------------------------------------
%% @doc Informs that some data have been consumed. 

-spec consumed(Super::super(), Stage::twerl_stage()) ->
          {consumed, super()}.

consumed(#?Pl{nexts = []} = Super, Stage) ->
    % Data consumed, just return the new pipeleine
    {consumed, Super#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs the data hsa been ignored.
%% the caller is free to do something else with the data.
%% Normally used by stages consuming messages to filter the one
%% for them and ignore the others.

-spec ignore(Super::super(), Stage::twerl_stage(),
             Data::twerl_stage_data()) ->
          {ignore, twerl_stage_data(), super()}.

ignore(#?Pl{prevs = []} = Super, Stage, Data) ->
    % Producer ignore the data, normally used with messages
    {ignore, Data, Super#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs that more data is needed to continue.

-spec need_more(Super::super(), Stage::twerl_stage(),
                Query::twerl_stage_query()) ->
          {data, twerl_stage_data(), twerl_stage_format(), super()}
        | {consumed, super()}
        | {more, twerl_stage_query(), twerl_stage_format(), super()}
        | {eos, super()}.

need_more(#?Pl{prevs = [], target = undefined} = Super, Stage, Query) ->
    % No more upstream stages, really need more
    #?Pl{prev_fmts = [Fmt]} = Super,
    {more, Query, Fmt, Super#?Pl{curr = Stage}};
need_more(#?Pl{prevs = []}, _Stage, _Query) ->
    % A stage is pulling data for itself, and there is no producer stage
    erlang:error(no_producer);
need_more(Super, Stage, Query) ->
    % Go back to the previous downstream stage and continue
    #?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
         prevs = [Prev |Prevs], nexts = Nexts} = Super,
    NewSuper = Super#?Pl{prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
                         prevs = Prevs, curr = Prev, nexts = [Stage |Nexts]},
    call_continue_(NewSuper, Query).


%% --------------------------------------------------------------------
%% @doc Pulls some data from upstream using specified query.

-spec pull(Super::super(), Stage::twerl_stage(),
           Query::twerl_stage_query()) ->
          {data, twerl_stage_data(), twerl_stage_format(), super()}
        | {consumed, super()}
        | {more, twerl_stage_query(), twerl_stage_format(), super()}
        | {eos, super()}.

pull(#?Pl{prevs = []}, _Stage, _Query) ->
    % A stage is pulling data for itself, and there is no producer stage
    erlang:error(no_producer);
pull(Super, Stage, Query) ->
    % A stage is pulling data for itself, set it as target and continue
    #?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
         prevs = [Prev |Prevs], nexts = Nexts} = Super,
    NewSuper = Super#?Pl{target = Stage,
                         prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
                         prevs = Prevs, curr = Prev, nexts = [Stage |Nexts]},
    call_continue_(NewSuper, Query).


%% --------------------------------------------------------------------
%% @doc Negotiates upstream format.

-spec negotiate_upstream(Super::super(), Stage::twerl_stage(),
                         Formats::twerl_stage_formats()) ->
          {ok, twerl_stage_format(), twerl_stage(), super()}
        | {error, not_negotiated}.

negotiate_upstream(Super, Stage, Formats) ->
    FmtLst = as_list_(Formats),
    case renegotiate_upstream_(Super#?Pl{curr = Stage}, FmtLst) of
        {ok, NewSuper} ->
            #?Pl{prev_fmts = [NewFmt |_], curr = NewStage} = NewSuper,
            {ok, NewFmt, NewStage, NewSuper};
        {error, not_negotiated} ->
            {error, not_negotiated}
    end.


%% --------------------------------------------------------------------
%% @doc Negotiates downstream format.

-spec negotiate_downstream(Super::super(), Stage::twerl_stage(),
                           Formats::twerl_stage_formats()) ->
          {ok, twerl_stage_format(), twerl_stage(), super()}
        | {error, not_negotiated}.

negotiate_downstream(Super, Stage, Formats) ->
    FmtLst = as_list_(Formats),
    case renegotiate_downstream_(Super#?Pl{curr = Stage}, FmtLst) of
        {ok, NewSuper} ->
            #?Pl{next_fmts = [NewFmt |_], curr = NewStage} = NewSuper,
            {ok, NewFmt, NewStage, NewSuper};
        {error, not_negotiated} ->
            {error, not_negotiated}
    end.


%% --------------------------------------------------------------------
%% @doc Informs the stage finished processing data.

-spec finished(Super::super(), Stage::twerl_stage()) ->
          {eos, super()}.

finished(Super, Stage) ->
    {eos, Super#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs the stage failed for some reaons.

-spec failed(any(), any(), any()) -> no_return().

failed(_Super, Stage, Reason) ->
    erlang:error({stage_error, Reason, Stage}).


%% ====================================================================
%% Internal Functions
%% ====================================================================

as_list_([_|_] = L) -> L;
as_list_(O) -> [O].

renegotiate_upstream_(#?Pl{prevs = []}, _NewFmts) ->
    {error, not_negotiated};
renegotiate_upstream_(Super, NewFmts) ->
    #?Pl{input = InFmt, prevs = Prevs} = Super,
    erlog:log("Renegotiating formats of ~w upstream stages from ~w to ~w",
              [length(Prevs), NewFmts, InFmt]),
    case twerl_pipeline:negotiate_stages_(Super, [InFmt], NewFmts, Prevs) of
        {ok, InvFmts} ->
            case twerl_pipeline:setup_stages_(Super, [], InvFmts, Prevs) of
                {ok, NewStages} ->
                    {ok, Super#?Pl{prev_fmts = InvFmts,
                                  prevs = lists:reverse(NewStages)}};
                Any -> Any
            end;
        Any -> Any
    end.

renegotiate_downstream_(#?Pl{nexts = []}, _NewFmts) ->
    {error, not_negotiated};
renegotiate_downstream_(Super, NewFmts) ->
    #?Pl{output = OutFmt, nexts = Nexts} = Super,
    erlog:log("Renegotiating formats of ~w downstream stages from ~w to ~w",
              [length(Nexts), NewFmts, OutFmt]),
    InvStages = lists:reverse(Nexts),
    case twerl_pipeline:negotiate_stages_(Super, NewFmts, OutFmt, InvStages) of
        {ok, InvFmts} ->
            case twerl_pipeline:setup_stages_(Super, [], InvFmts, InvStages) of
                {ok, NewStages} ->
                    {ok, Super#?Pl{next_fmts = lists:reverse(InvFmts),
                                  nexts = NewStages}};
                Any -> Any
            end;
        Any -> Any
    end.

call_process_(#?Pl{curr = Stage} = Super, Data) ->
    Stage:process(Data, Super).

call_continue_(#?Pl{curr = Stage} = Super, Query) ->
    Stage:continue(Query, Super).

