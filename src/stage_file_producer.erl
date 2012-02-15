%% ===========================================================================
%% @doc Stage producing data form a disk file.
%%
%% Supported output formats:
%%
%%  - {binary, line}
%%  - {list, line}
%%  - {iodata, line}
%%  - {binary, block}
%%  - {list, block}
%%  - {iodata, block}
%%  - {binary, {block, MaxSize}}          when is_integer(MaxSize)
%%  - {list, {block, MaxSize}}            when is_integer(MaxSize)
%%  - {iodata, {block, MaxSize}}          when is_integer(MaxSize)
%%
%% Supported input formats:
%%
%%  - query
%%
%% Supported input queries:
%%
%%  - next
%%  - {size, {bytes, Max}}                when is_integer(Max)
%%  - {position, {bytes, Pos}}            when is_integer(Max)
%%  - {position, {bytes, {bof, Pos}}}     when is_integer(Pos)
%%  - {position, {bytes, {cur, Pos}}}     when is_integer(Pos)
%%  - {position, {bytes, {eof, Pos}}}     when is_integer(Pos)
%%  - {segment, {bytes, Pos, Max}}        when is_integer(Pos), is_integer(Max)
%%  - {segment, {bytes, {bof, Pos}, Max}} when is_integer(Pos), is_integer(Max)
%%  - {segment, {bytes, {cur, Pos}, Max}} when is_integer(Pos), is_integer(Max)
%%  - {segment, {bytes, {eof, Pos}, Max}} when is_integer(Pos), is_integer(Max)
%%
%% Supported pull queries:
%%
%%  All suported input queries.
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

-module(stage_file_producer).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(file_prod).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_stage.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/1, new/2]).

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

-define(DEFAULT_BLOCK_SIZE, 8*1024*1024).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {file, block = ?DEFAULT_BLOCK_SIZE}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a file producer stage with default block size producing
%% data from specified disk file.

new(File) ->
    {ok, #?St{file = File}}.


%% --------------------------------------------------------------------
%% @doc Creates a file producer stage with specified block size producing
%% data from specified disk file.

new(File, BlockSize) ->
    {ok, #?St{file = File, block = BlockSize}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)                  -> 'query';
negotiate({list, line}, _)         -> 'query';
negotiate({binary, line}, _)       -> 'query';
negotiate({iodata, line}, _)       -> 'query';
negotiate({list, block}, _)        -> 'query';
negotiate({binary, block}, _)      -> 'query';
negotiate({iodata, block}, _)      -> 'query';
negotiate({list, {block, _}}, _)   -> 'query';
negotiate({binary, {block, _}}, _) -> 'query';
negotiate({iodata, {block, _}}, _) -> 'query';
negotiate(_, _)                    -> rejected.


agree(_, any, St)                  -> {binary, {block, St#?St.block}};
agree(_, {_, line} = F, _)         -> F;
agree(_, {T, block}, St)           -> {T, {block, St#?St.block}};
agree(_, {_, {block, _}} = F, _)   -> F;
agree(_, _, _)                     -> rejected.


setup(_InFmt, {iodata, line}, State) ->
    ok = io:setopts(State#?St.file, [binary]),
    {ok, State#?St{block = line}};
setup(_InFmt, {T, line}, State) ->
    ok = io:setopts(State#?St.file, [T]),
    {ok, State#?St{block = line}};
setup(_InFmt, {iodata, {block, MaxSize}}, State) ->
    ok = io:setopts(State#?St.file, [binary]),
    {ok, State#?St{block = MaxSize}};
setup(_InFmt, {T, {block, MaxSize}}, State) ->
    ok = io:setopts(State#?St.file, [T]),
    {ok, State#?St{block = MaxSize}}.


process(next, Super, #?St{block = line} = State) ->
    #?St{file = File} = State,
    case io:get_line(File, "") of
        eof -> ?super:finished(Super, State);
        {error, Reason} -> ?super:failed(Super, State, Reason);
        Data -> ?super:produce(Super, State, Data)
    end;
process(Query, Super, State) ->
    #?St{file = File} = State,
    case parse_query_(State, Query) of
        {ok, {cur, 0}, Size} ->
            handle_read_result_(Super, State, file:read(File, Size));
        {ok, Offset, Size} ->
            handle_read_result_(Super, State, file:pread(File, Offset, Size))
    end.


continue(Query, Super, State) ->
    process(Query, Super, State).


%% ====================================================================
%% Internal Functions
%% ====================================================================

parse_query_(St, next) ->
    {ok, {cur, 0}, St#?St.block};
parse_query_(St, {position, {bytes, {bof, Offset}}})
  when is_integer(Offset) ->
    {ok, {bof, Offset}, St#?St.block};
parse_query_(St, {position, {bytes, {cur, Offset}}})
  when is_integer(Offset) ->
    {ok, {cur, Offset}, St#?St.block};
parse_query_(St, {position, {bytes, {eof, Offset}}})
  when is_integer(Offset) ->
    {ok, {bof, Offset}, St#?St.block};
parse_query_(St, {position, {bytes, Offset}})
  when is_integer(Offset) ->
    {ok, {bof, Offset}, St#?St.block};
parse_query_(_St, {size, {bytes, Size}})
  when is_integer(Size) ->
    {ok, {cur, 0}, Size};
parse_query_(_St, {segment, {bytes, {bof, Offset}, Size}})
  when is_integer(Offset), is_integer(Size) ->
    {ok, {bof, Offset}, Size};
parse_query_(_St, {segment, {bytes, {cur, Offset}, Size}})
  when is_integer(Offset), is_integer(Size) ->
    {ok, {cur, Offset}, Size};
parse_query_(_St, {segment, {bytes, {eof, Offset}, Size}})
  when is_integer(Offset), is_integer(Size) ->
    {ok, {bof, Offset}, Size};
parse_query_(_St, {segment, {bytes, Offset, Size}})
  when is_integer(Offset), is_integer(Size) ->
    {ok, {bof, Offset}, Size};
parse_query_(_St, _Query) ->
    {error, query_not_supported}.

handle_read_result_(Super, State, eof) ->
    ?super:finished(Super, State);
handle_read_result_(Super, State, ebadf) ->
    ?super:failed(Super, State, ebadf);
handle_read_result_(Super, State, {error, Reason}) ->
    ?super:failed(Super, State, Reason);
handle_read_result_(Super, State, {ok, Data}) ->
    ?super:produce(Super, State, Data);
handle_read_result_(Super, State, {no_translation, _, _} = Reason) ->
    ?super:failed(Super, State, Reason).
