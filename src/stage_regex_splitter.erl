%% ===========================================================================
%% @doc Stage splitting input data using regular expresion.
%%
%% Supported output formats:
%%
%%  - {io_data, field}
%%  - {binary, field}
%%  - {list, field}
%%  - {io_data, {field, RegExp}}
%%  - {binary, {field, RegExp}}
%%  - {list, {field, RegExp}}
%%
%% Supported input formats:
%%
%%  - io_data
%%  - binary
%%  - list
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

-module(stage_regex_splitter).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(rxsplit_fltr).


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

%% Default splitting regular expresion
-define(DEFAULT_EXP, "\n").


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {regex, output, defexp = ?DEFAULT_EXP, data = <<>>}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a regular new line splitter.
%% Same as new("\n").

new() -> {ok, #?St{}}.


%% --------------------------------------------------------------------
%% @doc Creates a splitter stage with specified regular expresion
%% to match separators.

new(Sep) -> {ok, #?St{defexp = Sep}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)                  -> [iodata, binary, list];
negotiate({list, field}, _)        -> [iodata, binary, list];
negotiate({binary, field}, _)      -> [iodata, binary, list];
negotiate({iodata, field}, _)      -> [iodata, binary, list];
negotiate({binary, {field, _}}, _) -> [iodata, binary, list];
negotiate({list, {field, _}}, _)   -> [iodata, binary, list];
negotiate({iodata, {field, _}}, _) -> [iodata, binary, list];
negotiate(_, _)                    -> rejected.


agree(_, any, St)                  -> {binary, {field, St#?St.defexp}};
agree(_, {T, field}, St)           -> {T, {field, St#?St.defexp}};
agree(_, {_, {field, _}} = Fmt, _) -> Fmt;
agree(_, T, St)                    -> {T, {field, St#?St.defexp}}.


setup(_InFmt, {FmtType, {field, Sep}}, State) ->
    {ok, Regex} = re:compile(Sep, [multiline]),
    {ok, State#?St{output = FmtType, regex = Regex}}.


process(Data, Super, State) ->
    #?St{regex = Regex, output = RetType, data = Last} = State,
    Parts = re:split([Last, Data], Regex,
                     [{parts, 2}, {return, RetType}, group]),
    case Parts of
        [[Field |_], [Rem]] ->
			?super:produce(Super, State#?St{data = Rem}, Field);
        [[Rem]] ->
			?super:need_more(Super, State#?St{data = Rem}, next)
    end.


continue(next, Super, State) ->
    process(<<>>, Super, State);
continue(Query, Super, State) ->
    % For any other queries, we just flush and pass the query upstream
    ?super:need_more(Super, State#?St{data = <<>>}, Query).
