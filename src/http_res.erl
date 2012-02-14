%% ===========================================================================
%% @doc        HTTP response handling.
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

-module(http_res).

-author('Sebastien Merle <s.merle@gmail.com>').


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("http_res.hrl").

-include("http_req.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0, new/2, new/3, new/4,
         format/1,
         add_header/3,
         get_headers/1,
         get_header_value/2]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @spec new() -> #http_res{}
%%
%% @doc Creates a new HTTP response.

-spec new() -> #http_res{}.

new() ->
    #http_res{}.


%% --------------------------------------------------------------------
%% @spec new(Request, Status) -> #http_res{}
%%
%%      Request = #http_req{}
%%      Status = atome() | integer()
%%
%% @doc Creates a new HTTP response.

-spec new(Request::#http_req{}, Satus::http_status())
        -> #http_res{}.

new(Request, Status) ->
    #http_res{version = Request#http_req.version, status = Status}.


%% --------------------------------------------------------------------
%% @spec new(Request, Status, Headers) -> #http_res{}
%%
%%      Request = #http_req{}
%%      Status = atome() | integer()
%%      Headers = [{atom(), list() | binary()}]
%%
%% @doc Creates a new HTTP response.

-spec new(Request::#http_req{}, Satus::http_status(), Headers::http_headers())
        -> #http_res{}.

new(Request, Status, Headers) when is_list(Headers)->
    #http_res{version = Request#http_req.version,
              status = Status, headers = Headers}.

%% --------------------------------------------------------------------
%% @spec new(Peer, Version, Status, Message) -> #http_res{}
%%
%%      Peer = {tcp, ip_address(), ip_port()}
%%      Version = {1, 1} | {1, 0}
%%      Status = atome() | integer()
%%      Message = string() | binary()
%%
%% @doc Creates a new HTTP response.

-spec new(Peer::http_peer(), Version::http_version(),
          Status::http_status(), Message::http_message()) ->
          #http_res{}.

new(Peer, Version, Status, Message) ->
    #http_res{peer = Peer, status = Status,
              message = Message, version = Version}.


%% --------------------------------------------------------------------
%% @spec format(Response::#http_res{}) -> string()
%%
%% @doc Format an HTTP response as a string.

format(#http_res{status = Status, message = undefined}) ->
    io_lib:format("~s (~b)", [httplib:response_message(Status),
                              httplib:response_code(Status)]);
format(#http_res{status = Status, message = Message})->
    io_lib:format("~s (~b)", [Message, httplib:response_code(Status)]).


%% --------------------------------------------------------------------
%% @spec add_header(Name::atom(), Value::any(), Response::#http_res{})
%%           -> #http_res{}
%%
%% @doc Adds specified header to the HTTP Response.

-spec add_header(Name::atom(), Value::any(), Response::#http_res{}) ->
          #http_res{}.

add_header(Name, Value, Res) ->
    Res#http_res{headers = [{Name, Value} | Res#http_res.headers]}.


%% --------------------------------------------------------------------
%% @spec get_headers(Response::#http_res{}) -> [atom()]
%%
%% @doc Return all the defined headers. 

-spec get_headers(Response::#http_res{}) -> [atom()].

get_headers(Response) ->
    proplists:get_keys(Response#http_res.headers).


%% --------------------------------------------------------------------
%% @spec get_header_value(Name::atom(), Response::#http_res{})
%%           -> [any()] | any()
%%
%% @doc Return specified header value. If the header is a combined one all
%% the values are returned as a list, otherwise only the last one is returned.

-spec get_header_value(Name::atom(), Response::#http_res{}) ->
          [any()] | any().

get_header_value(Name, Res) ->
    get_header_value(httplib:combined_header(Name), Name, Res).

get_header_value(true, Name, Res) ->
    proplists:append_values(Name, Res#http_res.headers);
get_header_value(false, Name, Res) ->
    proplists:get_value(Name, Res#http_res.headers).
