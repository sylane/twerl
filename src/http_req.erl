%% ===========================================================================
%% @doc        HTTP request handling.
%% @since      Jan 13, 2012
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

-module(http_req).

-author('Sebastien Merle <s.merle@gmail.com>').


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("http_req.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0, new/4,
         format/1,
         add_header/3,
         get_headers/1,
         get_header_value/2]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% @spec new() -> #http_req{}
%%
%% @doc Creates a new HTTP request.

-spec new() -> #http_req{}.

new() ->
    #http_req{id = erlang:make_ref()}.


%% --------------------------------------------------------------------
%% @spec new(Peer, Method, Url, Version) -> #http_req{}
%%
%%      Peer = {tcp, ip_address(), ip_port()}
%%      Method = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'HEAD'
%%      Url = #url{}
%%      Version = {1, 1} | {1, 0}
%%
%% @doc Creates a new HTTP request.

-spec new(Peer::http_peer(), Method::http_method(),
          Url::#url{}, Version::http_version()) ->
          #http_req{}.

new(Peer, Method, Url, Version) ->
    #http_req{id = erlang:make_ref(), peer = Peer,
                  method = Method, version = Version, url = Url}.


%% --------------------------------------------------------------------
%% @spec format(Request::#http_req{}) -> string()
%%
%% @doc Format an HTTP request as a string.

format(Req) ->
    url:format(Req#http_req.url).


%% @spec add_header(Name::atom(), Value::any(), Request::#http_req{})
%%           -> #http_req{}
%%
%% @doc Adds specified header to the HTTP request.

-spec add_header(Name::atom(), Value::any(), Request::#http_req{}) ->
          #http_req{}.

add_header(Name, Value, Req) ->
    Req#http_req{headers = [{Name, Value} | Req#http_req.headers]}.


%% --------------------------------------------------------------------
%% @spec get_headers(Request::#http_req{}) -> [atom()]
%%
%% @doc Return all the defined headers. 

-spec get_headers(Request::#http_req{}) -> [atom()].

get_headers(Request) ->
    proplists:get_keys(Request#http_req.headers).


%% --------------------------------------------------------------------
%% @spec get_header_value(Name::atom(), Request::#http_req{})
%%           -> [any()] | any()
%%
%% @doc Return specified header value. If the header is a combined one, all
%% the values are returned as a list, otherwise only the last one is returned.

-spec get_header_value(Name::atom(), Request::#http_req{}) ->
          [any()] | any().

get_header_value(Name, Req) ->
    get_header_value(httplib:combined_header(Name), Name, Req).

get_header_value(true, Name, Req) ->
    proplists:append_values(Name, Req#http_req.headers);
get_header_value(false, Name, Req) ->
    proplists:get_value(Name, Req#http_req.headers).
