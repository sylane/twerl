%% ===========================================================================
%% @doc        Acceptor using TCP/IP to listen for connections.
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

%TODO: Use proplists for option handling.

-module(acceptor_tcp).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_acceptor).

-erlog_category(tcp_lis).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([prepare/1,
         create/2]).

%% Overridden base_acceptor callbacks
-export([init/1,
         handle_message/3,
         start_accepting/2,
         stop_accepting/2]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% TCP Acceptor's state record name
-define(St, ?MODULE).

%% Default socket options value
-define(DEFAULT_SOCK_OPTS, [binary, {active, false}]).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% TCP Acceptor's state
-record(?St, {port, sockopts, sock, ref}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares a TCP acceptor definition.

prepare(Port) ->
    %TODO: make prepare parameter a proplist
    {fun gen_acceptor:create/3, ?MODULE, [{reuseaddr, true} |prepare_(Port)]}.


%% --------------------------------------------------------------------
%% @doc Request the service context to create a acceptor process.
%% Config is a list of tuple that can contains:
%%     {args, any()} -> Arguments passed to the factory
%%     {port, pos_integer()}
%%     {reuseaddr, boolean()}
%%     {backlog, pos_integer()}

create(Context, Port) ->
    gen_acceptor:create(Context, ?MODULE, prepare_(Port)).


%% ====================================================================
%% Overriden base_acceptor Callbacks
%% ====================================================================

init(Config) ->
    {ok, State} = initialize_(#?St{}, Config),
    start_(State).

handle_message({inet_async, Sock, Ref, {ok, CliSock}},
               #?St{sock = Sock, ref = Ref} = State, Acceptor) ->
    connection_accepted_(State, Acceptor, CliSock);
handle_message({inet_async, Sock, Ref, Error},
               #?St{sock = Sock, ref = Ref} = State, Acceptor) ->
    accept_error_(State, Acceptor, Error);
handle_message(Msg, State, Acceptor) ->
    base_acceptor:handle_message(Msg, State, Acceptor).

start_accepting(#?St{port = Port, sockopts = SockOpts} = State, Acceptor) ->
    erlog:log("Start listening on TCP port ~w...", [Port]),
    case gen_tcp:listen(Port, SockOpts) of
        {ok, Sock} ->
            erlog:info("Listening on port ~w.", [Port]),
            accept_connection_(State#?St{sock = Sock}, Acceptor);
        {error, _} = Error -> Error
    end.

stop_accepting(#?St{sock = undefined} = State, _Acceptor) -> {ok, State};
stop_accepting(#?St{port = Port, sock = Sock} = State, _Acceptor) ->
    erlog:info("Stopped listening on TCP port ~w.", [Port]),
    ok = gen_tcp:close(Sock),
    {ok, State#?St{sock = undefined}}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

prepare_(Port) when is_integer(Port) ->
    [{port, Port}].

initialize_(St, Config) ->
    erlog:log("Configuring TCP port with ~p...", [Config]),
    initialize_(St, [], [], Config).

initialize_(St, OAcc, [], []) ->
    SockOpt = optlib:set_defaults(?DEFAULT_SOCK_OPTS, lists:reverse(OAcc)),
    {ok, St#?St{sockopts = SockOpt}};
initialize_(St, OAcc, AAcc, [{port, Port} |Rest]) ->
    initialize_(St#?St{port = Port}, OAcc, AAcc, Rest);
initialize_(St, OAcc, AAcc, [{reuseaddr, _} = Opt |Rest]) ->
    initialize_(St, [Opt |OAcc], AAcc, Rest);
initialize_(St, OAcc, AAcc, [{backlog, _} = Opt |Rest]) ->
    initialize_(St, [Opt |OAcc], AAcc, Rest);
initialize_(_St, _OAcc, _AAcc, [Opt |_Rest]) ->
    erlang:error({bad_config, Opt}).

start_(St) -> {ok, St}.

accept_connection_(#?St{port = Port, sock = Sock} = St, _Acceptor) ->
    erlog:log("Start accepting TCP connection from port ~w...", [Port]),
    case prim_inet:async_accept(Sock, -1) of
        {ok, Ref} ->
            erlog:debug("Accepting TCP connection from port ~w...", [Port]),
            {ok, St#?St{ref = Ref}};
        {error, Reason} ->
            erlog:error("Failed to accept from port ~w: ~p.", [Port, Reason]),
            {error, Reason}
    end.

accept_error_(#?St{port = Port} = St, _Acceptor, Reason) ->
    erlog:error("Failed to accept from port ~w: ~p.", [Port, Reason]),
    {stop, Reason, St}.

connection_accepted_(#?St{port = Port, sock = Sock} = St, Acceptor, CliSock) ->
    erlog:debug("Connection accepted from port ~w.", [Port]),
    true = inet_db:register_socket(CliSock, inet_tcp),
    OptNames = [active, nodelay, keepalive, delay_send, priority, tos],
    case prim_inet:getopts(Sock, OptNames) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSock, Opts) of
                ok -> delegate_connection_(St, Acceptor, CliSock);
                {error, Reason} ->
                    erlog:error("Connection setup fail: ~p.", [Reason]),
                    gen_tcp:close(CliSock),
                    {stop, Reason, St}
            end;
        {error, Reason} ->
            erlog:error("Connection setup fail: ~p.", [Reason]),
            gen_tcp:close(CliSock),
            {stop, Reason, St}
    end.

delegate_connection_(St, Acceptor, CliSock) ->
    erlog:log("Building TCP input/output stages..."),
    {ok, Peer} = inet:peername(CliSock),
    {ok, Proto} = gen_acceptor:inner_create_protocol(Acceptor),
    {ok, Producer} = stage_tcp_producer:new(Proto, CliSock),
    {ok, Consumer} = stage_tcp_consumer:new(Proto, CliSock),
    ok = gen_acceptor:inner_connect_protocol({tcp, Peer}, Proto,
                                           [Producer], [Consumer], Acceptor),
    accept_connection_(St, Acceptor).
