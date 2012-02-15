%% ===========================================================================
%% @doc Stage producing data from a TCP socket.
%%
%% Supported output formats:
%%
%%  - {list, line}
%%  - {list, block}
%%  - {list, {block, MaxSize}}   when is_integer(MaxSize)
%%  - {list, {packet, T}}        when T =:= 1; T =:= 2; T =:= 4
%%                                    T =:= asn1; T =:= acdr; T =:= sunrm;
%%                                    T =:= fcgi; T =:= tpkt
%%  - {binary, line}
%%  - {binary, block}
%%  - {binary, {block, MaxSize}} when is_integer(MaxSize)
%%  - {binary, {packet, T}}      when T =:= 1; T =:= 2; T =:= 4
%%                                    T =:= asn1; T =:= acdr; T =:= sunrm;
%%                                    T =:= fcgi; T =:= tpkt
%%  - {iodata, line}
%%  - {iodata, block}
%%  - {iodata, {block, MaxSize}} when is_integer(MaxSize)
%%  - {iodata, {packet, T}}      when T =:= 1; T =:= 2; T =:= 4
%%                                    T =:= asn1; T =:= acdr; T =:= sunrm;
%%                                    T =:= fcgi; T =:= tpkt
%%  - {http, list}
%%  - {http, binary}
%%
%% Supported input format:
%%
%%  - message
%%
%% Supported pull queries:
%%
%%  - next
%%  - {size, {bytes, S}}        when is_integer(S) (Only for block format)
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

-module(stage_tcp_producer).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(tcp_prod).


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

-define(DEFAULT_BLOCK_SIZE, 8*1024*1024).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Stage's state
-record(?St, {state, sock, peer, output, block=?DEFAULT_BLOCK_SIZE}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a TCP stage producing data from the specified socket.

new(Owner, Sock) ->
    {ok, Peer} = inet:peername(Sock),
    erlog:log("Creating TCP producer for ~s...", [format:peer(Peer)]),
    ok = gen_tcp:controlling_process(Sock, Owner),
    {ok, #?St{state = init, sock = Sock, peer = Peer}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) ->
    erlog:log("Initializing TCP producer for ~s...",
              [format:peer(State#?St.peer)]),
    {ok, State#?St{state = setup}}.


negotiate(any, _)                       -> message;
negotiate({list, line}, _)              -> message;
negotiate({list, block}, _)             -> message;
negotiate({list, {block, M}}, _)
  when is_integer(M)                    -> message;
negotiate({list, {packet, 1}}, _)       -> message;
negotiate({list, {packet, 2}}, _)       -> message;
negotiate({list, {packet, 4}}, _)       -> message;
negotiate({list, {packet, asn1}}, _)    -> message;
negotiate({list, {packet, cdr}}, _)     -> message;
negotiate({list, {packet, sunrm}}, _)   -> message;
negotiate({list, {packet, fcgi}}, _)    -> message;
negotiate({list, {packet, tpkt}}, _)    -> message;
negotiate({binary, line}, _)            -> message;
negotiate({binary, block}, _)           -> message;
negotiate({binary, {block, M}}, _)
  when is_integer(M)                    -> message;
negotiate({binary, {packet, 1}}, _)     -> message;
negotiate({binary, {packet, 2}}, _)     -> message;
negotiate({binary, {packet, 4}}, _)     -> message;
negotiate({binary, {packet, asn1}}, _)  -> message;
negotiate({binary, {packet, cdr}}, _)   -> message;
negotiate({binary, {packet, sunrm}}, _) -> message;
negotiate({binary, {packet, fcgi}}, _)  -> message;
negotiate({binary, {packet, tpkt}}, _)  -> message;
negotiate({iodata, line}, _)            -> message;
negotiate({iodata, block}, _)           -> message;
negotiate({iodata, {block, M}}, _)
  when is_integer(M)                    -> message;
negotiate({iodata, {packet, 1}}, _)     -> message;
negotiate({iodata, {packet, 2}}, _)     -> message;
negotiate({iodata, {packet, 4}}, _)     -> message;
negotiate({iodata, {packet, asn1}}, _)  -> message;
negotiate({iodata, {packet, cdr}}, _)   -> message;
negotiate({iodata, {packet, sunrm}}, _) -> message;
negotiate({iodata, {packet, fcgi}}, _)  -> message;
negotiate({iodata, {packet, tpkt}}, _)  -> message;
negotiate(http, _)                      -> message;
negotiate({http, list}, _)              -> message;
negotiate({http, binary}, _)            -> message;
negotiate(_, _)                         -> rejected.


agree(message, any, St)                 -> {binary, {block, St#?St.block}};
agree(message, http, _)                 -> {http, binary};
agree(message, {list, block}, St)       -> {list, {block, St#?St.block}};
agree(message, {binary, block}, St)     -> {binary, {block, St#?St.block}};
agree(message, {iodata, block}, St)     -> {iodata, {block, St#?St.block}};
agree(message, F, _)                    -> F.


setup(message, {http, list} = F, State) ->
    setup_(State, F, list, http);
setup(message, {http, binary} = F, State) ->
    setup_(State, F, binary, http_bin);
setup(message, {Type, line} = F, State) ->
    setup_(State, F, Type, line);
setup(message, {Type, {packet, PacketType}} = F, State) ->
    setup_(State, F, Type, PacketType);
setup(message, {Type, {block, MaxSize}} = F, State) ->
    setup_(State#?St{block = MaxSize}, F, Type, raw).


process({http, Sock, Data}, Super, #?St{sock = Sock} = State) ->
    ?super:produce(Super, State, Data);
process({tcp, Sock, Data}, Super, #?St{sock = Sock} = State) ->
    ?super:produce(Super, State, Data);
process({tcp_closed, Sock}, Super, #?St{sock = Sock} = State) ->
    erlog:debug("Connection to ~s closed.", [format:peer(State#?St.peer)]),
    ?super:finished(Super, State);
process(Msg, Super, State) ->
    ?super:ignore(Super, State, Msg).


continue(next, Super, State) ->
    #?St{sock = Sock} = State,
    ok = inet:setopts(Sock, [{active, once}]),
    ?super:need_more(Super, State, next);
continue({size, {bytes, Max}}, Super,
         #?St{output = {_, {block, _}}} = State) ->
    #?St{sock = Sock} = State,
    case gen_tcp:recv(Sock, Max) of
        {ok, Packet} -> ?super:produce(Super, State, Packet);
        {error, Reason} -> ?super:failed(Super, State, Reason)
    end.


%% ====================================================================
%% Internal Functions
%% ====================================================================

setup_(State, Format, Type, PacketType) ->
    erlog:log("Setting up TCP producer for ~s with format ~w.",
              [format:peer(State#?St.peer), Format]),
    ok = setup_types_(State, Type, PacketType),
    bootstrap_(State#?St{output = Format}).

setup_types_(State, iodata, PacketType) ->
    inet:setopts(State#?St.sock, [binary, {packet, PacketType}]);
setup_types_(State, Type, PacketType) ->
    inet:setopts(State#?St.sock, [Type, {packet, PacketType}]).

bootstrap_(#?St{state = normal} = State) ->
    % Already waiting for messages
    {ok, State};
bootstrap_(State) ->
    #?St{sock = Sock} = State,
    ok = inet:setopts(Sock, [{active, once}]),
    {ok, State#?St{state = normal}}.
