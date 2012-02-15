%% ===========================================================================
%% @doc Stage decoding data to http_pkt idiom.
%%
%% Supported output formats:
%%
%%  - {http_pkt, list}
%%  - {http_pkt, binary}
%%
%% Supported input formats:
%%
%%  - {binary, block}
%%
%% Supported input queries:
%%
%%  - next
%%
%% Supported pull queries:
%%
%%  - next
%%
%% @since      Aug 06, 2010
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

-module(stage_http_pkt_decoder).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(http_dec).


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
-record(?St, {type, state, header_regex, body_len, rem_len, data = <<>>}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates an HTTP decoder stage decoding data to http_pkt idiom.

new() -> {ok, #?St{state = request}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)                -> {binary, block};
negotiate(http_pkt, _)           -> {binary, block};
negotiate({http_pkt, list}, _)   -> {binary, block};
negotiate({http_pkt, binary}, _) -> {binary, block};
negotiate(_, _)                  -> rejected.


agree({binary, block}, any, _)                     -> {http_pkt, binary};
agree({binary, block}, http_pkt, _)                -> {http_pkt, binary};
agree({binary, block}, {http_pkt, binary}, _)      -> {http_pkt, binary};
agree({binary, block}, {http_pkt, list}, _)        -> {http_pkt, list};
agree({binary, {block, _}}, http_pkt, _)           -> {http_pkt, binary};
agree({binary, {block, _}}, {http_pkt, binary}, _) -> {http_pkt, binary};
agree({binary, {block, _}}, {http_pkt, list}, _)   -> {http_pkt, list};
agree(_, _, _)                                     -> rejected.


setup(_InFmt, {http_pkt, list}, State) ->
    setup_(State#?St{type = list}); 
setup(_InFmt, {http_pkt, binary}, State) ->
    setup_(State#?St{type = binary}).


process(Data, Super, State) ->
    #?St{data = Rem} = State,
    parse_http_(Super, State, <<Rem/binary, Data/binary>>).


continue(next, Super, #?St{data = Rem} = State) ->
    parse_http_(Super, State, Rem);
continue(Query, Super, State) ->
    Reason = {query_not_supported, Query, ?MODULE},
    ?super:failed(Super, State, Reason).


%% ====================================================================
%% Internal Functions
%% ====================================================================

setup_(State) ->
    {ok, Regex} = re:compile(" *, *"),
    {ok, State#?St{header_regex = Regex}}.

parse_http_(Super, #?St{state = error} = State, _) ->
    ?super:finished(Super, State);
parse_http_(Super, #?St{state = fixed_body} = State, Data) ->
    parse_fixed_body_(Super, State, Data);
parse_http_(State, Super, <<>>) ->
    ?super:need_more(Super, State, next);
parse_http_(Super, State, Data) ->
    case erlang:decode_packet(packet_type_(State), Data, []) of
        {error, Reason} ->
            ?super:failed(Super, State, Reason);
        {more, _Length} ->
            ?super:need_more(Super, State#?St{data = Data}, next);
        {ok, Packet, Rest} ->
            produce_packet_(Super, State, Packet, Rest)
    end.

parse_fixed_body_(Super, #?St{rem_len = 0} = State, Data) ->
    NewState = State#?St{state = request, data = Data,
                         body_len = undefined, rem_len = undefined},
    ?super:produce(Super, NewState, eob);
parse_fixed_body_(Super, State, <<>>) ->
    ?super:need_more(Super, State, next);
parse_fixed_body_(Super, #?St{rem_len = RemLen} = State, Data)
  when byte_size(Data) < RemLen ->
    NewState = State#?St{data = <<>>, rem_len = RemLen - byte_size(Data)},
    produce_body_(Super, NewState, Data);
parse_fixed_body_(Super, #?St{rem_len = RemLen} = State, Data) ->
    <<Body:RemLen/binary, RemData/binary>> = Data,
    NewState = State#?St{data = RemData, rem_len = 0},
    produce_body_(Super, NewState, Body).

produce_body_(Super, #?St{type = list} = State, Data) ->
    ?super:produce(Super, State, {body, erlang:binary_to_list(Data)});
produce_body_(Super, State, Data) ->
    ?super:produce(Super, State, {body, Data}).

produce_packet_(Super, #?St{state = request} = State,
                {http_request, Method, Path, Ver}, Rest) ->
    NewState = State#?St{state = headers, data = Rest},
    Packet = {request, Ver, Method, parse_path_(State, Path)},
    ?super:produce(Super, NewState, Packet);
produce_packet_(Super, #?St{state = request} = State,
                {http_response, Ver, Code, Msg}, Rest) ->
    NewState = State#?St{state = headers, data = Rest},
    Packet = {response, Ver, Code, Msg},
    ?super:produce(Super, NewState, Packet);
produce_packet_(Super, #?St{state = headers} = State,
                {http_header, _, 'Content-Length', _, Value}, Rest) ->
    try header_to_integer_(State, Value) of
        BodyLen ->
            NewState = State#?St{data = Rest, body_len = BodyLen},
            Packet = {header, 'Content-Length', Value},
            ?super:produce(Super, NewState, Packet)
    catch
        error:badarg ->
            fatal_error_(Super, State, bad_request,
                         {bad_header, 'Content-Length', Value})
    end;
produce_packet_(Super, #?St{state = headers} = State,
                {http_header, _, Header, _, Value}, Rest) ->
    Value1 = header_value_(State, httplib:combined_header(Header), Value),
    Packet = {header, Header, Value1},
    ?super:produce(Super, State#?St{data = Rest}, Packet);
produce_packet_(Super, #?St{state = headers, body_len = undefined} = State,
                http_eoh, Rest) ->
    NewState = State#?St{state = fixed_body, data = Rest,
                         body_len = 0, rem_len = 0},
    ?super:produce(Super, NewState, eoh);
produce_packet_(Super, #?St{state = headers, body_len = BodyLen} = State,
                http_eoh, Rest) ->
    NewState = State#?St{state = fixed_body, data = Rest, rem_len = BodyLen},
    ?super:produce(Super, NewState, eoh);
produce_packet_(Super, State, {http_error, Data}, Rest) ->
    NewState = State#?St{data = Rest},
    error_(Super, NewState, unexpected, Data).

header_value_(_State, false, Value) -> Value;
header_value_(State, true, Value) ->
    #?St{type = Type, header_regex = Regex} = State,
    [V || V <- re:split(Value, Regex, [{return, Type}]), V =/= <<>>].

parse_path_(#?St{type = list}, {abs_path, Path}) ->
    url:parse(Path);
parse_path_(#?St{type = list}, Path) ->
    url:parse(Path);
parse_path_(#?St{type = binary}, {abs_path, Path}) ->
    url:parse(binary_to_list(Path));
parse_path_(#?St{type = binary}, Path) ->
    url:parse(binary_to_list(Path)).

header_to_integer_(#?St{type = list}, Val) ->
    erlang:list_to_integer(Val);
header_to_integer_(#?St{type = binary}, Val) ->
    erlang:list_to_integer(erlang:binary_to_list(Val)).

packet_type_(#?St{type = list, state = request}) -> http;
packet_type_(#?St{type = binary, state = request}) -> http_bin;
packet_type_(#?St{type = list, state = headers}) -> httph;
packet_type_(#?St{type = binary, state = headers}) -> httph_bin.

error_(Super, State, Kind, Info) ->
    Packet = {error, {Kind, Info}},
    ?super:produce(Super, State, Packet).

fatal_error_(Super, State, Kind, Info) ->
    NewState = State#?St{state = error, data = <<>>},
    Packet = {error, {Kind, Info}},
    ?super:produce(Super, NewState, Packet).
