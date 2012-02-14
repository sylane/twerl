%% ===========================================================================
%% @doc        Idiom http_pkt decoding/encoding mapping.
%% @since      Aug 13, 2010
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

-module(idiom_http_pkt).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_max_level(debug).

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([request/5,
         response/4, response/5,
         header/3,
         eoh/1,
         body/2,
         eob/1,
         error_pkt/3,
         make_tests/2]).


%% --------------------------------------------------------------------
%% Exported Functions
%% --------------------------------------------------------------------

request(_Variation, Method, Path, VerHigh, VerLow)
  when is_integer(VerHigh), is_integer(VerLow),
       VerHigh >= 0, VerLow >= 0 ->
    {request, {VerHigh, VerLow}, Method, url:parse(mk_str(Path))}.

response(Variation, Code, Message, VerHigh, VerLow)
  when is_integer(Code), is_integer(VerHigh), is_integer(VerLow),
       VerHigh >= 0, VerLow >= 0 ->
    {response, {VerHigh, VerLow}, Code, mk_var(Variation, Message)}.

response(_Variation, Status, VerHigh, VerLow)
  when is_atom(Status), is_integer(VerHigh), is_integer(VerLow),
       VerHigh >= 0, VerLow >= 0 ->
    {response, {VerHigh, VerLow}, Status}.

header(Variation, Name, Value)
  when is_atom(Name) ->
    {header, Name, mk_var(Variation, Value)}.

eoh(_Variation) -> eoh.

body(Variation, Data) ->
    {body, mk_var(Variation, Data)}.

eob(_Variation) -> eob.

error_pkt(Variation, unexpected, Data) ->
    {error, {unexpected, mk_var(Variation, Data)}};
error_pkt(Variation, bad_request, {bad_header, Name, Value})
  when is_atom(Name) ->
    {error, {bad_request, {bad_header, Name, mk_var(Variation, Value)}}}.

make_tests(Variation, MkFun) ->
	I = binary,
    O = Variation,
    Map = [{mk_var(I, "GET / HTTP/1.0\r\n\r\n"),
            [request(O, 'GET', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "GET / HTTP/1.1\r\n\r\n"),
            [request(O, 'GET', "/", 1, 1), eoh(O), eob(O)]},
           {mk_var(I, "GET / HTTP/0.9\r\n\r\n"),
            [request(O, 'GET', "/", 0, 9), eoh(O), eob(O)]},
           {mk_var(I, "HEAD / HTTP/1.0\r\n\r\n"),
            [request(O, 'HEAD', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "PUT / HTTP/1.0\r\n\r\n"),
            [request(O, 'PUT', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "POST / HTTP/1.0\r\n\r\n"),
            [request(O, 'POST', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "DELETE / HTTP/1.0\r\n\r\n"),
            [request(O, 'DELETE', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "GET /a/b/c/d HTTP/1.0\r\n\r\n"),
            [request(O, 'GET', "/a/b/c/d", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "GET /a/b/c/d/ HTTP/1.0\r\n\r\n"),
            [request(O, 'GET', "/a/b/c/d/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "GET a/b/c/d/ HTTP/1.0\r\n\r\n"),
            [request(O, 'GET', "a/b/c/d/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "HTTP/1.0 200 OK\r\n\r\n"),
            [response(O, 200, "OK", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, "HTTP/1.1 404 Not Found\r\n\r\n"),
            [response(O, 404, "Not Found", 1, 1), eoh(O), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n",
                        "Referer: http://dummy/url\r\n",
                        "User-Agent: foo\r\n\r\n">>),
            [request(O, 'GET', "/", 1, 0),
             header(O, 'Referer', "http://dummy/url"),
             header(O, 'User-Agent', "foo"),
             eoh(O), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n",
                        "Content-Length: 8\r\n\r\n",
                        "XXXXXXXX">>),
            [request(O, 'GET', "/", 1, 0),
             header(O, 'Content-Length', "8"), eoh(O),
             body(O, "XXXXXXXX"), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n",
                        "Content-Length: 0\r\n\r\n">>),
            [request(O, 'GET', "/", 1, 0),
             header(O, 'Content-Length', "0"),
             eoh(O), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n">>),
            [request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n",
                        "GET / HTTP/1.0\r\n\r\n">>),
            [request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0), eoh(O), eob(O)]},
           {mk_var(I, <<"GET / HTTP/1.0\r\n\r\n",
                        "GET baz HTTP/1.1\r\n",
                        "User-Agent: spam\r\n\r\n",
                        "GET / HTTP/1.0\r\n",
                        "Content-Length: 1\r\n\r\nX",
                        "GET / HTTP/1.1\r\n",
                        "Content-Length: 0\r\n\r\n",
                        "GET / HTTP/1.0\r\n",
                        "Content-Length: 42\r\n\r\n",
                        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
                        "GET /foo/bar HTTP/1.1\r\n\r\n",
                        "GET / HTTP/1.1\r\n",
                        "User-Agent: bacon\r\n",
                        "Content-Length: 18\r\n\r\n",
                        "YYYYYYYYYYYYYYYYYY",
                        "GET / HTTP/1.1\r\n",
                        "Content-Length: 3\r\n",
                        "User-Agent: eggs\r\n\r\n",
                        "ZZZ">>),
            [request(O, 'GET', "/", 1, 0), eoh(O), eob(O),
             request(O, 'GET', "baz", 1, 1),
             header(O, 'User-Agent', "spam"), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0),
             header(O, 'Content-Length', "1"), eoh(O),
             body(O, "X"), eob(O),
             request(O, 'GET', "/", 1, 1),
             header(O, 'Content-Length', "0"), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 0),
             header(O, 'Content-Length', "42"), eoh(O),
             body(O, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
             eob(O),
             request(O, 'GET', "/foo/bar", 1, 1), eoh(O), eob(O),
             request(O, 'GET', "/", 1, 1),
             header(O, 'User-Agent', "bacon"),
             header(O, 'Content-Length', "18"),
             eoh(O),
             body(O, "YYYYYYYYYYYYYYYYYY"),
             eob(O),
             request(O, 'GET', "/", 1, 1),
             header(O, 'Content-Length', "3"),
             header(O, 'User-Agent', "eggs"),
             eoh(O),
             body(O, "ZZZ"),
             eob(O)]}],
    [MkFun([Enc], Dec) || {Enc, Dec} <- Map].


%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

mk_bin(Value) when is_binary(Value) -> Value;
mk_bin(Value) when is_list(Value) -> erlang:list_to_binary(Value).

mk_str(Value) when is_list(Value) -> Value;
mk_str(Value) when is_binary(Value) -> erlang:binary_to_list(Value).

mk_var(list, Value) -> mk_str(Value);
mk_var(binary, Value) -> mk_bin(Value).

