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


process(Data, Pipe, State) ->
    #?St{data = Rem} = State,
    parse_http_(State, Pipe, <<Rem/binary, Data/binary>>).


continue(next, Pipe, #?St{data = Rem} = State) ->
    parse_http_(State, Pipe, Rem);
continue(Query, Pipe, State) ->
    Reason = {query_not_supported, Query, ?MODULE},
    twerl_stage:failed(Reason, State, Pipe).


%% ====================================================================
%% Internal Functions
%% ====================================================================

setup_(State) ->
    {ok, Regex} = re:compile(" *, *"),
    {ok, State#?St{header_regex = Regex}}.

parse_http_(#?St{state = error} = State, Pipe, _) ->
    twerl_stage:finished(State, Pipe);
parse_http_(#?St{state = fixed_body} = State, Pipe, Data) ->
    parse_fixed_body_(State, Pipe, Data);
parse_http_(State, Pipe, <<>>) ->
    twerl_stage:need_more(next, State, Pipe);
parse_http_(State, Pipe, Data) ->
    case erlang:decode_packet(packet_type_(State), Data, []) of
        {error, Reason} ->
            twerl_stage:failed(Reason, State, Pipe);
        {more, _Length} ->
            twerl_stage:need_more(next, State#?St{data = Data}, Pipe);
        {ok, Packet, Rest} ->
            produce_packet_(State, Pipe, Packet, Rest)
    end.

parse_fixed_body_(#?St{rem_len = 0} = State, Pipe, Data) ->
    NewState = State#?St{state = request, data = Data,
                         body_len = undefined, rem_len = undefined},
    twerl_stage:produce(eob, NewState, Pipe);
parse_fixed_body_(State, Pipe, <<>>) ->
    twerl_stage:need_more(next, State, Pipe);
parse_fixed_body_(#?St{rem_len = RemLen} = State, Pipe, Data)
  when byte_size(Data) < RemLen ->
    NewState = State#?St{data = <<>>, rem_len = RemLen - byte_size(Data)},
    produce_body_(NewState, Pipe, Data);
parse_fixed_body_(#?St{rem_len = RemLen} = State, Pipe, Data) ->
    <<Body:RemLen/binary, RemData/binary>> = Data,
    NewState = State#?St{data = RemData, rem_len = 0},
    produce_body_(NewState, Pipe, Body).

produce_body_(#?St{type = list} = State, Pipe, Data) ->
    twerl_stage:produce({body, erlang:binary_to_list(Data)}, State, Pipe);
produce_body_(State, Pipe, Data) ->
    twerl_stage:produce({body, Data}, State, Pipe).

produce_packet_(#?St{state = request} = State, Pipe,
                {http_request, Method, Path, Ver}, Rest) ->
    NewState = State#?St{state = headers, data = Rest},
    Packet = {request, Ver, Method, parse_path_(State, Path)},
    twerl_stage:produce(Packet, NewState, Pipe);
produce_packet_(#?St{state = request} = State, Pipe,
                {http_response, Ver, Code, Msg}, Rest) ->
    NewState = State#?St{state = headers, data = Rest},
    Packet = {response, Ver, Code, Msg},
    twerl_stage:produce(Packet, NewState, Pipe);
produce_packet_(#?St{state = headers} = State, Pipe,
                {http_header, _, 'Content-Length', _, Value}, Rest) ->
    try header_to_integer_(State, Value) of
        BodyLen ->
            NewState = State#?St{data = Rest, body_len = BodyLen},
            Packet = {header, 'Content-Length', Value},
            twerl_stage:produce(Packet, NewState, Pipe)
    catch
        error:badarg ->
            fatal_error_(State, Pipe, bad_request,
                         {bad_header, 'Content-Length', Value})
    end;
produce_packet_(#?St{state = headers} = State, Pipe,
                {http_header, _, Header, _, Value}, Rest) ->
    Value1 = header_value_(State, httplib:combined_header(Header), Value),
    Packet = {header, Header, Value1},
    twerl_stage:produce(Packet, State#?St{data = Rest}, Pipe);
produce_packet_(#?St{state = headers, body_len = undefined} = State, Pipe,
                http_eoh, Rest) ->
    NewState = State#?St{state = fixed_body, data = Rest,
                         body_len = 0, rem_len = 0},
    twerl_stage:produce(eoh, NewState, Pipe);
produce_packet_(#?St{state = headers, body_len = BodyLen} = State, Pipe,
                http_eoh, Rest) ->
    NewState = State#?St{state = fixed_body, data = Rest, rem_len = BodyLen},
    twerl_stage:produce(eoh, NewState, Pipe);
produce_packet_(State, Pipe, {http_error, Data}, Rest) ->
    NewState = State#?St{data = Rest},
    error_(NewState, Pipe, unexpected, Data).

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

error_(State, Pipe, Kind, Info) ->
    Packet = {error, {Kind, Info}},
    twerl_stage:produce(Packet, State, Pipe).

fatal_error_(State, Pipe, Kind, Info) ->
    NewState = State#?St{state = error, data = <<>>},
    Packet = {error, {Kind, Info}},
    twerl_stage:produce(Packet, NewState, Pipe).
