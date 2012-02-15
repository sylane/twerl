%% ===========================================================================
%% @doc Stage decoding packets from raw data.
%%
%% The stage will decode packet it knows about the previous stage
%% is not able to provide it already decoded, otherwise it will act as
%% a pass-through stage.
%%
%% Supported output formats:
%%
%%  - any
%%  - {binary, block}
%%  - {binary, line}
%%  - {binary, {packet, T}}      when T =:= 1; T =:= 2; T =:= 4
%%                                    T =:= asn1; T =:= acdr; T =:= sunrm;
%%                                    T =:= fcgi; T =:= tpkt
%%  - {http, list}
%%  - {http, binary}
%%
%% Supported input formats:
%%
%%  - any 
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

-module(stage_packet_decoder).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(packet_dec).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_stage.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0]).

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
-record(?St, {input, output, format, data = <<>>}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a packet deocder stage.

new() -> {ok, #?St{}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _) ->
    [{binary, block}];

negotiate({binary, block}, _) ->
    [{binary, block}, {iodata, block}, {list, block}];
negotiate({list, block}, _) ->
    [{list, block}, {binary, block}, {iodata, block}];
negotiate({iodata, block}, _) ->
    [{iodata, block}, {binary, block}, {list, block}];

negotiate({binary, {block, S}}, _) when is_integer(S) ->
    [{binary, {block, S}}, {iodata, {block, S}}, {list, {block, S}},
     {binary, block}, {iodata, block}, {list, block}];
negotiate({list, {block, S}}, _) when is_integer(S) ->
    [{list, {block, S}}, {binary, {block, S}}, {iodata, {block, S}},
     {list, block}, {binary, block}, {iodata, block}];
negotiate({iodata, {block, S}}, _) when is_integer(S) ->
    [{iodata, {block, S}}, {binary, {block, S}}, {list, {block, S}},
     {iodata, block}, {binary, block}, {list, block}];

negotiate({binary, line}, _) ->
    [{binary, line}, {iodata, line}, {list, line},
     {binary, block}, {iodata, block}, {list, block}];
negotiate({list, line}, _) ->
    [{list, line}, {binary, line}, {iodata, line},
     {list, block}, {binary, block}, {iodata, block}];
negotiate({iodata, line}, _) ->
    [{iodata, line}, {binary, line}, {list, line},
     {iodata, block}, {binary, block}, {list, block}];

negotiate({binary, {packet, P}} = F, _)
  when P =:= 1; P =:= 2; P =:= 4; P =:= asn1; P =:= cdr;
       P =:= sunrm; P =:= fcgi; P =:= tpkt ->
    [F, {binary, block}, {iodata, block}, {list, block}];
negotiate({iodata, {packet, P}} = F, _)
  when P =:= 1; P =:= 2; P =:= 4; P =:= asn1; P =:= cdr;
       P =:= sunrm; P =:= fcgi; P =:= tpkt ->
    [F, {iodata, block}, {binary, block}, {list, block}];
negotiate({list, {packet, P}} = F, _)
  when P =:= 1; P =:= 2; P =:= 4; P =:= asn1; P =:= cdr;
       P =:= sunrm; P =:= fcgi; P =:= tpkt ->
    [F, {list, block}, {binary, block}, {iodata, block}];

negotiate(http, _) ->
    [{http, binary}, {binary, block}, {list, block}, {iodata, block}];
negotiate({http, list}, _) ->
    [{http, list}, {binary, block}, {list, block}, {iodata, block}];
negotiate({http, binary}, _) ->
    [{http, binary}, {binary, block}, {list, block}, {iodata, block}];

negotiate(Passthroug, _) -> Passthroug.


agree({F, {block, _}}, {F, {block, _}} = Fmt, _) -> Fmt;
agree({_, {block, S}}, {O, block}, _) -> {O, {block, S}};
%agree({F, {block, _}} = Fmt, F, _) -> Fmt;
agree(Fmt, any, _) -> Fmt;
agree(_, Fmt, _) -> Fmt.


setup(Fmt, Fmt, State) ->
    {ok, State#?St{format = passthrough}};
setup({I, block}, {O, {block, S}}, State) ->
    {ok, State#?St{input = I, output = O, format = {block, S}}};
setup({I, {block, _}}, {O, {block, S}}, State) ->
    {ok, State#?St{input = I, output = O, format = {block, S}}};
setup({I, block}, {O, block}, State) ->
    {ok, State#?St{input = I, output = O, format = passthrough}};
setup(InFmt, OutFmt, State) ->
    {ok, State1} = setup_input_(State, InFmt),
    setup_output_(State1, OutFmt).


process(Data, Super, #?St{format = passthrough} = State) ->
    Converted = convert_output_(State, convert_input_(State, Data)),
    ?super:produce(Super, State, Converted);
process(Data, Super, #?St{data = Rem, format = {block, S}} = State) ->
    Converted = convert_input_(State, Data),
    parse_block_(Super, State, S, <<Rem/binary, Converted/binary>>);
process(Data, Super, #?St{data = Rem} = State) ->
    Converted = convert_input_(State, Data),
    parse_packet_(Super, State, <<Rem/binary, Converted/binary>>).


continue(Query, Super, #?St{format = passthrough} = State) ->
    ?super:need_more(Super, State, Query);
continue(next, Super, #?St{data = Buff, format = {block, S}} = State) ->
    parse_block_(Super, State, S, Buff);
continue(next, Super, State) ->
    #?St{data = Rem} = State,
    parse_packet_(Super, State, Rem);
continue(Query, Super, State) ->
    % Flush remaining data and forwar the query upstream
    ?super:need_more(Super, State#?St{data = <<>>}, Query).


%% ====================================================================
%% Internal Functions
%% ====================================================================

setup_input_(State, {I, block})
  when I =:= binary; I =:= list; I =:= iodata ->
    {ok, State#?St{input = I}};
setup_input_(State, {I, {block, _}})
  when I =:= binary; I =:= list; I =:= iodata ->
    {ok, State#?St{input = I}}.

setup_output_(State, {http, list}) ->
    {ok, State#?St{format = http}};
setup_output_(State, {http, binary}) ->
    {ok, State#?St{format = http_bin}};
setup_output_(State, {binary, line}) ->
    {ok, State#?St{output = binary, format = line}};
setup_output_(State, {binary, {packet, Format}}) ->
    {ok, State#?St{output = binary, format = Format}};
setup_output_(State, {list, line}) ->
    {ok, State#?St{output = list, format = line}};
setup_output_(State, {list, {packet, Format}}) ->
    {ok, State#?St{output = list, format = Format}};
setup_output_(State, {iodata, line}) ->
    {ok, State#?St{output = iodata, format = line}};
setup_output_(State, {iodata, {packet, Format}}) ->
    {ok, State#?St{output = iodata, format = Format}}.

convert_input_(#?St{input = list}, Data) -> list_to_binary(Data);
convert_input_(#?St{input = iodata}, Data) -> iolist_to_binary(Data);
convert_input_(_, Data) -> Data.

convert_output_(#?St{output = list}, Data) -> binary_to_list(Data);
convert_output_(_, Data) -> Data.

parse_block_(Super, State, Size, Buff) when byte_size(Buff) >= Size ->
    <<Packet:Size/binary, NewBuff/binary>> = Buff,
    Packet1 = convert_output_(State, Packet),
    ?super:produce(Super, State#?St{data = NewBuff}, Packet1);
parse_block_(Super, State, _, Buff) ->
    ?super:need_more(Super, State#?St{data = Buff}, next).

parse_packet_(Super, #?St{format = Type} = State, Data) ->
    case erlang:decode_packet(Type, Data, []) of
        {error, Reason} ->
            ?super:failed(Super, State, Reason);
        {more, _Length} ->
            ?super:need_more(Super, State#?St{data = Data}, next);
        {ok, Packet, Rest} ->
            {ok, NewState} = update_type_(State#?St{data = Rest}),
            Packet1 = convert_output_(State, Packet),
            ?super:produce(Super, NewState, Packet1)
    end.

update_type_(#?St{format = http}) -> {ok, httph};
update_type_(#?St{format = http_bin}) -> {ok, httph_bin};
update_type_(State) -> {ok, State}.
