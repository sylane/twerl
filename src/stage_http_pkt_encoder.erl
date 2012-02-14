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


process(Data, Pipe, State) ->
    encode_http_(State, Pipe, Data).


continue(next, Pipe, State) ->
    twerl_stage:need_more(next, State, Pipe);
continue(Query, Pipe, State) ->
    Reason = {query_not_supported, Query, ?MODULE},
    twerl_stage:failed(Reason, State, Pipe).


%% ====================================================================
%% Internal Functions
%% ====================================================================

encode_http_(State, Pipe, eoh) ->
    twerl_stage:produce(<<"\r\n">>, State, Pipe);
encode_http_(State, Pipe, eob) ->
    twerl_stage:need_more(next, State, Pipe);
encode_http_(State, Pipe, {request, Ver, Method, Url}) ->
    produce_http_request_(State, Pipe, Ver, Method, Url);
encode_http_(State, Pipe, {response, Ver, Status}) ->
    Code = httplib:response_code(Status),
    Message = httplib:response_message(Status),
    produce_http_response_(State, Pipe, Ver, Code, Message);
encode_http_(State, Pipe, {response, Ver, Status, undefined}) ->
    Code = httplib:response_code(Status),
    Message = httplib:response_message(Status),
    produce_http_response_(State, Pipe, Ver, Code, Message);
encode_http_(State, Pipe, {response, Ver, Status, Message}) ->
    Code = httplib:response_code(Status),
    produce_http_response_(State, Pipe, Ver, Code, Message);
encode_http_(State, Pipe, {header, Name, Value}) ->
    produce_http_header_(State, Pipe, Name, Value);
encode_http_(State, Pipe, {body, Data}) ->
    produce_http_body_(State, Pipe, Data);
encode_http_(State, Pipe, {error, {unexpected, _}}) ->
    twerl_stage:need_more(next, State, Pipe);
encode_http_(State, Pipe, {error, {bad_request, _}}) ->
    twerl_stage:finished(State, Pipe).

produce_http_response_(State, Pipe, Ver, Code, Message) ->
    Data = [<<"HTTP/">>, format:version(Ver), <<$ >>,
            erlang:integer_to_list(Code), <<$ >>,
            Message, <<"\r\n">>],
    twerl_stage:produce(Data, State, Pipe).

produce_http_request_(State, Pipe, Ver, Method, Url) ->
    Data = [erlang:atom_to_list(Method), <<$ >>,
            url:format_location(Url), <<$ >>,
            <<"HTTP/">>, format:version(Ver), <<"\r\n">>],
    twerl_stage:produce(Data, State, Pipe).

produce_http_header_(State, Pipe, Name, Value) ->
    IsCombined = httplib:combined_header(Name),
    produce_http_header_(State, Pipe, IsCombined, Name, Value).

produce_http_header_(State, Pipe, false, Name, Value) ->
    Data = [header_name_(Name), <<": ">>, Value, <<"\r\n">>],
    twerl_stage:produce(Data, State, Pipe);
produce_http_header_(State, Pipe, true, _Name, []) ->
    twerl_stage:consumed(State, Pipe);
produce_http_header_(State, Pipe, true, Name, Values) ->
    Data = [header_name_(Name), <<": ">>, join_values(Values), <<"\r\n">>],
    twerl_stage:produce(Data, State, Pipe).

header_name_(Name) when is_atom(Name) -> erlang:atom_to_list(Name);
header_name_(Name) -> Name.

join_values([V |Vs]) -> join_values(Vs, [V]). 

join_values([], Acc) -> lists:reverse(Acc);
join_values([V |Vs], Acc) -> join_values(Vs, [V, <<", ">> |Acc]).

produce_http_body_(State, Pipe, Data) ->
    twerl_stage:produce(Data, State, Pipe).
