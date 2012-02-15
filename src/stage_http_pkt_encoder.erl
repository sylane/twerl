%% ===========================================================================
%% @doc Stage encoding http_pkt idiom to raw data.
%%
%% Supported output formats:
%%
%%  - {io_data, block}
%%
%% Supported input formats:
%%
%%  - {http_pkt, list} -> NOT SUPPORTED YET
%%  - {http_pkt, binary}
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

-module(stage_http_pkt_encoder).

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
-record(?St, {}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates an HTTP encoder stage converting http_pkt idiom to raw data.

new() -> {ok, #?St{}}.


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

init(State) -> {ok, State}.


negotiate(any, _)             -> {http_pkt, binary};
negotiate({iodata, block}, _) -> {http_pkt, binary};
negotiate(_, _)               -> rejected.


agree({http_pkt, binary}, any, _)             -> {iodata, block};
agree({http_pkt, binary}, {iodata, block}, _) -> {iodata, block};
agree(_, _, _)                                -> rejected.


setup(_InFmt, _OutFmt, State) ->
    {ok, State}.


process(Data, Super, State) ->
    encode_http_(Super, State, Data).


continue(next, Super, State) ->
    ?super:need_more(Super, State, next);
continue(Query, Super, State) ->
    Reason = {query_not_supported, Query, ?MODULE},
    ?super:failed(Super, State, Reason).


%% ====================================================================
%% Internal Functions
%% ====================================================================

encode_http_(Super, State, eoh) ->
    ?super:produce(Super, State, <<"\r\n">>);
encode_http_(Super, State, eob) ->
    ?super:need_more(Super, State, next);
encode_http_(Super, State, {request, Ver, Method, Url}) ->
    produce_http_request_(Super, State, Ver, Method, Url);
encode_http_(Super, State, {response, Ver, Status}) ->
    Code = httplib:response_code(Status),
    Message = httplib:response_message(Status),
    produce_http_response_(Super, State, Ver, Code, Message);
encode_http_(Super, State, {response, Ver, Status, undefined}) ->
    Code = httplib:response_code(Status),
    Message = httplib:response_message(Status),
    produce_http_response_(Super, State, Ver, Code, Message);
encode_http_(Super, State, {response, Ver, Status, Message}) ->
    Code = httplib:response_code(Status),
    produce_http_response_(Super, State, Ver, Code, Message);
encode_http_(Super, State, {header, Name, Value}) ->
    produce_http_header_(Super, State, Name, Value);
encode_http_(Super, State, {body, Data}) ->
    produce_http_body_(Super, State, Data);
encode_http_(Super, State, {error, {unexpected, _}}) ->
    ?super:need_more(Super, State, next);
encode_http_(Super, State, {error, {bad_request, _}}) ->
    ?super:finished(Super, State).

produce_http_response_(Super, State, Ver, Code, Message) ->
    Data = [<<"HTTP/">>, format:version(Ver), <<$ >>,
            erlang:integer_to_list(Code), <<$ >>,
            Message, <<"\r\n">>],
    ?super:produce(Super, State, Data).

produce_http_request_(Super, State, Ver, Method, Url) ->
    Data = [erlang:atom_to_list(Method), <<$ >>,
            url:format_location(Url), <<$ >>,
            <<"HTTP/">>, format:version(Ver), <<"\r\n">>],
    ?super:produce(Super, State, Data).

produce_http_header_(Super, State, Name, Value) ->
    IsCombined = httplib:combined_header(Name),
    produce_http_header_(Super, State, IsCombined, Name, Value).

produce_http_header_(Super, State, false, Name, Value) ->
    Data = [header_name_(Name), <<": ">>, Value, <<"\r\n">>],
    ?super:produce(Super, State, Data);
produce_http_header_(Super, State, true, _Name, []) ->
    ?super:consumed(Super, State);
produce_http_header_(Super, State, true, Name, Values) ->
    Data = [header_name_(Name), <<": ">>, join_values(Values), <<"\r\n">>],
    ?super:produce(Super, State, Data).

header_name_(Name) when is_atom(Name) -> erlang:atom_to_list(Name);
header_name_(Name) -> Name.

join_values([V |Vs]) -> join_values(Vs, [V]). 

join_values([], Acc) -> lists:reverse(Acc);
join_values([V |Vs], Acc) -> join_values(Vs, [V, <<", ">> |Acc]).

produce_http_body_(Super, State, Data) ->
    ?super:produce(Super, State, Data).
