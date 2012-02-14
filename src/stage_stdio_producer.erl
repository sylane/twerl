%% ===========================================================================
%% @doc Stage producing data from standard input.
%%
%% Supported output formats:
%%
%%  - {iodata, line}
%%  - {binary, line}
%%  - {list, line}
%%  - {iodata, block}
%%  - {binary, block}
%%  - {list, block}
%%
%% Supported input formats:
%%
%%  - query
%%
%% Supported input queries:
%%
%%  - next
%%
%% Supported pull queries:
%%
%%  - next
%%
%% @since      Mar 23, 2010
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

-module(stage_stdio_producer).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(stdio_prod).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_stage.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0, new/1]).

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
-record(?St, {output, eos = "\n"}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a stage producing data read from console standard input.
%% Use "\n" as end-of-stream marker, same as new("\n").

new() -> {ok, #?St{}}.


%% --------------------------------------------------------------------
%% @doc Creates a stage producing data read from console standard input
%% using specified tag as end-of-stream marker.

new(EOS) -> {ok, #?St{eos = EOS}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)             -> 'query';
negotiate({list, line}, _)    -> 'query';
negotiate({binary, line}, _)  -> 'query';
negotiate({iodata, line}, _)  -> 'query';
negotiate({list, block}, _)   -> 'query';
negotiate({binary, block}, _) -> 'query';
negotiate({iodata, block}, _) -> 'query';
negotiate(_, _)               -> rejected.


agree(_, any, _)        -> {binary, line};
agree(_, {T, line}, _)  -> {T, line};
agree(_, {T, block}, _) -> {T, block};
agree(_, _, _)          -> rejected.


setup(_InFmt, {iodata, _}, State) ->
    {ok, State#?St{output = binary, eos = fix_type_(binary, State#?St.eos)}};
setup(_InFmt, {T, _}, State) ->
    {ok, State#?St{output = T, eos = fix_type_(T, State#?St.eos)}}.


process(next, Pipe, State) ->
    produce_(State, Pipe);
process(Query, Pipe, State) ->
    not_supported_(State, Pipe, Query).


continue(next, Pipe, State) ->
    produce_(State, Pipe);
continue(Query, Pipe, State) ->
    not_supported_(State, Pipe, Query).


%% ====================================================================
%% Internal Functions
%% ====================================================================

fix_type_(binary, V) when is_binary(V) -> V;
fix_type_(binary, V) when is_list(V) -> list_to_binary(V);
fix_type_(list, V) when is_binary(V) -> binary_to_list(V);
fix_type_(list, V) when is_list(V) -> V.

produce_(State, Pipe) ->
    #?St{output = OutFmt, eos = EOS} = State,
    % We do it every time because we don't own standard IO, config could change
    ok = io:setopts([OutFmt]),
    case io:get_line("") of
        eof -> twerl_stage:finished(State, Pipe);
        {error, Reason} -> twerl_stage:failed(Reason, State, Pipe);
        EOS -> twerl_stage:finished(State, Pipe);
        Data -> twerl_stage:produce(Data, State, Pipe)
    end.

-spec not_supported_(any(), any(), any()) -> no_return().
not_supported_(State, Pipe, Query) ->
    Reason = {query_not_supported, Query, ?MODULE},
    twerl_stage:failed(Reason, State, Pipe).
