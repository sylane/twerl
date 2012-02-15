%% ===========================================================================
%% @doc Stage priting consumed data to standard output. 
%% A prefix that will be printed before every lines can be specified.
%% If input format are fields, a carriage return is added afeter every fields.
%%
%% Supported output formats:
%%
%%  - none
%%
%% Supported input formats:
%%
%%  - {iodata, line}
%%  - {binary, line}
%%  - {list, line}
%%  - {iodata, field}
%%  - {binary, field}
%%  - {list, field}
%%  - {iodata, {field, _}}
%%  - {binary, {field, _}}
%%  - {list, {field, _}}
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

-module(stage_stdio_consumer).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(stdio_cons).


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
-record(?St, {prefix = "", postfix = "", device = standard_io}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a stage printing data it receive to standard output.

new() -> {ok, #?St{}}.


%% --------------------------------------------------------------------
%% @doc Creates a stage printing data it receive to standard output
%% each lines prefixed with specifed value.

new(Prefix) -> {ok, #?St{prefix = Prefix}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _) ->
    [{iodata, line}, {binary, line}, {list, line},
     {iodata, field}, {binary, field}, {list, field}];
negotiate(none, _) ->
    [{iodata, line}, {binary, line}, {list, line},
     {iodata, field}, {binary, field}, {list, field}];
negotiate(_, _) ->
    rejected.


agree(_, _, _) -> none.


setup({_, field}, none, State) ->
    {ok, State#?St{postfix = "\n"}};
setup({_, {field, _}}, none, State) ->
    {ok, State#?St{postfix = "\n"}};
setup({_, line}, none, State) ->
    {ok, State}.


process(Data, Super, State) ->
    #?St{prefix = Prefix, postfix = Postfix, device = Dev} = State,
    ok = file:write(Dev, [Prefix, Data, Postfix]),
    ?super:consumed(Super, State).


continue(Query, Super, State) ->
    ?super:need_more(Super, State, Query).
