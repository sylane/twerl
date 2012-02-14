%% ===========================================================================
%% @doc Stage writing consumed data to a TCP socket.
%%
%% Supported output formats:
%%
%%  - none
%%
%% Supported input formats:
%%
%%  - {iodata, block}
%%  - {list, block}
%%  - {binary, block}
%%  - {iodata, line}
%%  - {list, line}
%%  - {binary, line}
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

-module(stage_tcp_consumer).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(tcp_cons).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_stage.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/2]).

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
-record(?St, {sock, peer, input}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creats a TCP consumer writing consumed data to specified socket.

new(_Owner, Sock) ->
    {ok, Peer} = inet:peername(Sock),
    erlog:log("Creating TCP consumer for ~s...", [format:peer(Peer)]),
    {ok, #?St{sock = Sock, peer = Peer}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) ->
    erlog:log("Initializing TCP consumer for ~s...",
              [format:peer(State#?St.peer)]),
    {ok, State}.


negotiate(any, _) ->
    [{iodata, block}, {binary, block}, {list, block},
     {iodata, line}, {binary, line}, {list, line}];
negotiate(none, _) ->
    [{iodata, block}, {binary, block}, {list, block},
     {iodata, line}, {binary, line}, {list, line}];    
negotiate(_, _) ->
    rejected.


agree(_, _, _) -> none.


setup({Fmt, _}, none, State) ->
    setup_(State, Fmt).


process(Data, Pipe, #?St{input = list} = State) ->
    send_(State, Pipe, Data);
process(Data, Pipe, #?St{input = binary} = State) ->
    send_(State, Pipe, Data);
process(Data, Pipe, #?St{input = iodata} = State) ->
    send_(State, Pipe, erlang:iolist_to_binary(Data)).


continue(Query, Pipe, State) ->
    twerl_stage:need_more(Query, State, Pipe).


%% ====================================================================
%% Internal Functions
%% ====================================================================

setup_(State, Fmt) ->
    erlog:log("Setting up TCP consumer for ~s with format ~w.",
              [format:peer(State#?St.peer), Fmt]),
    {ok, State#?St{input = Fmt}}.

send_(State, Pipe, Data) ->
    #?St{sock = Sock} = State,
    case gen_tcp:send(Sock, Data) of
        ok -> twerl_stage:consumed(State, Pipe);
        {error, Reason} -> twerl_stage:failed(Reason, State, Pipe)
    end.
