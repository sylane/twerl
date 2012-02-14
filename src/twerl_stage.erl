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

-module(twerl_stage).

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


%% ====================================================================
%% Stage callbacks Functions, only to be called from inside a stage
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Makes the stage produce some data.

produce(Data, Stage, #?Pl{target = Targ, nexts = [Targ |_]} = Pipe) ->
	% The target of the data is the next stage
    #?Pl{prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
         prevs = Prevs, nexts = [Next |Nexts]} = Pipe,
    NewPipe = Pipe#?Pl{target = undefined,
                       prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
                       prevs = [Stage |Prevs], curr = Next, nexts = Nexts},
    {data, Data, Fmt, NewPipe};
produce(Data, Stage, #?Pl{nexts = []} = Pipe) ->
	% No more downstream stage to give the data, just return it
    #?Pl{next_fmts = [Fmt]} = Pipe,
    {data, Data, Fmt, Pipe#?Pl{curr = Stage}};
produce(Data, Stage, Pipe) ->
	% Pass data to the next downstream stage
    #?Pl{prev_fmts = PFmts, next_fmts = [Fmt  |NFmts],
         prevs = Prevs, nexts = [Next |Nexts]} = Pipe,
    NewPipe = Pipe#?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
                       prevs = [Stage |Prevs], curr = Next, nexts = Nexts},
    call_process_(NewPipe, Data).


%% --------------------------------------------------------------------
%% @doc Informs that some data have been consumed. 

consumed(Stage, #?Pl{nexts = []} = Pipe) ->
    % Data consumed, just return the new pipeleine
    {consumed, Pipe#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs the data hsa been ignored.
%% the caller is free to do something else with the data.
%% Normally used by stages consuming messages to filter the one
%% for them and ignore the others.

ignore(Data, Stage, #?Pl{prevs = []} = Pipe) ->
    % Producer ignore the data, normally used with messages
    {ignore, Data, Pipe#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs that more data is needed to continue.

need_more(Query, Stage, #?Pl{prevs = [], target = undefined} = Pipe) ->
    % No more upstream stages, really need more
    #?Pl{prev_fmts = [Fmt]} = Pipe,
    {more, Query, Fmt, Pipe#?Pl{curr = Stage}};
need_more(_Query, _Stage, #?Pl{prevs = []}) ->
    % A stage is pulling data for itself, and there is no producer stage
    erlang:error(no_producer);
need_more(Query, Stage, Pipe) ->
    % Go back to the previous downstream stage and continue
    #?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
         prevs = [Prev |Prevs], nexts = Nexts} = Pipe,
    NewPipe = Pipe#?Pl{prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
                       prevs = Prevs, curr = Prev, nexts = [Stage |Nexts]},
    call_continue_(NewPipe, Query).


%% --------------------------------------------------------------------
%% @doc Pulls some data from upstream using specified query.

pull(_Query, _Stage, #?Pl{prevs = []}) ->
    % A stage is pulling data for itself, and there is no producer stage
    erlang:error(no_producer);
pull(Query, Stage, Pipe) ->
    % A stage is pulling data for itself, set it as target and continue
    #?Pl{prev_fmts = [Fmt |PFmts], next_fmts = NFmts,
         prevs = [Prev |Prevs], nexts = Nexts} = Pipe,
    NewPipe = Pipe#?Pl{target = Stage,
                       prev_fmts = PFmts, next_fmts = [Fmt |NFmts],
                       prevs = Prevs, curr = Prev, nexts = [Stage |Nexts]},
    call_continue_(NewPipe, Query).


%% --------------------------------------------------------------------
%% @doc Negotiates upstream format.

negotiate_upstream(Formats, Stage, Pipe) ->
    FmtLst = as_list_(Formats),
    case renegotiate_upstream_(Pipe#?Pl{curr = Stage}, FmtLst) of
        {ok, NewPipe} ->
            #?Pl{prev_fmts = [NewFmt |_], curr = NewStage} = NewPipe,
            {ok, NewFmt, NewStage, NewPipe};
        {error, not_negotiated} ->
            {error, not_negotiated}
    end.


%% --------------------------------------------------------------------
%% @doc Negotiates downstream format.

negotiate_downstream(Formats, Stage, Pipe) ->
    FmtLst = as_list_(Formats),
    case renegotiate_downstream_(Pipe#?Pl{curr = Stage}, FmtLst) of
        {ok, NewPipe} ->
            #?Pl{next_fmts = [NewFmt |_], curr = NewStage} = NewPipe,
            {ok, NewFmt, NewStage, NewPipe};
        {error, not_negotiated} ->
            {error, not_negotiated}
    end.


%% --------------------------------------------------------------------
%% @doc Informs the stage finished processing data.

finished(Stage, Pipe) ->
    {eos, Pipe#?Pl{curr = Stage}}.


%% --------------------------------------------------------------------
%% @doc Informs the stage failed for some reaons.

-spec failed(any(), any(), any()) -> no_return().

failed(Reason, Stage, _Pipe) ->
    erlang:error({stage_error, Reason, Stage}).


%% ====================================================================
%% Internal Functions
%% ====================================================================

as_list_([_|_] = L) -> L;
as_list_(O) -> [O].

renegotiate_upstream_(#?Pl{prevs = []}, _NewFmts) ->
    {error, not_negotiated};
renegotiate_upstream_(Pipe, NewFmts) ->
    #?Pl{input = InFmt, prevs = Prevs} = Pipe,
    erlog:log("Renegotiating formats of ~w upstream stages from ~w to ~w",
              [length(Prevs), NewFmts, InFmt]),
    case twerl_pipeline:negotiate_stages_(Pipe, [InFmt], NewFmts, Prevs) of
        {ok, InvFmts} ->
            case twerl_pipeline:setup_stages_(Pipe, [], InvFmts, Prevs) of
                {ok, NewStages} ->
                    {ok, Pipe#?Pl{prev_fmts = InvFmts,
                                  prevs = lists:reverse(NewStages)}};
                Any -> Any
            end;
        Any -> Any
    end.

renegotiate_downstream_(#?Pl{nexts = []}, _NewFmts) ->
    {error, not_negotiated};
renegotiate_downstream_(Pipe, NewFmts) ->
    #?Pl{output = OutFmt, nexts = Nexts} = Pipe,
    erlog:log("Renegotiating formats of ~w downstream stages from ~w to ~w",
              [length(Nexts), NewFmts, OutFmt]),
    InvStages = lists:reverse(Nexts),
    case twerl_pipeline:negotiate_stages_(Pipe, NewFmts, OutFmt, InvStages) of
        {ok, InvFmts} ->
            case twerl_pipeline:setup_stages_(Pipe, [], InvFmts, InvStages) of
                {ok, NewStages} ->
                    {ok, Pipe#?Pl{next_fmts = lists:reverse(InvFmts),
                                  nexts = NewStages}};
                Any -> Any
            end;
        Any -> Any
    end.

call_process_(#?Pl{curr = Stage} = Pipe, Data) ->
    Stage:process(Data, Pipe).

call_continue_(#?Pl{curr = Stage} = Pipe, Query) ->
    Stage:continue(Query, Pipe).

