%% ===========================================================================
%% @doc        Connector opening a TCP/IP connection. 
%% @since      May 28, 2010
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

-module(connector_tcp).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_connector).

-erlog_category(tcp_conn).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([prepare/2,
         create/3]).

%% Overridden base_connector callbacks
-export([init/1,
         connect/2,
         terminate/3]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% TCP connector's state record name
-define(St, ?MODULE).

%% Default socket options value
-define(DEFAULT_SOCK_OPTS, [binary, {active, false}]).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% TCP connector's state
-record(?St, {addr = "localhost", port, timeout = infinity, sockopts}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Prepares a TCP connector definition.

prepare(Addr, Port) ->
    {fun gen_connector:create/3, ?MODULE, prepare_(Addr, Port)}.


%% --------------------------------------------------------------------
%% @doc Request the service context to create a connector process.
%%      Config is a list of tuples that can contains:
%%        {addr, string()} -> address to connect to
%%        {port, pos_integer()} -> TCP port to connect to
%%        {keepalive, boolean()} -> socket keepalive flag
%%        {args, any()} -> Arguments passed to the factory

create(Context, Addr, Port) ->
    gen_connector:create(Context, ?MODULE, prepare_(Addr, Port)).


%% ====================================================================
%% Overriden base_connector Callbacks
%% ====================================================================

init(Config) ->
    {ok, State} = initialize_(#?St{}, Config),
    init_connector_(State).

connect(#?St{addr = Addr, port = Port} = State, Connector) ->
    erlog:log("Start connecting to ~s...", [format:peer(Addr, Port)]),
    tcp_connect_(State, Connector).

terminate(shutdown, _State, _Connector) ->
    erlog:info("Connector shutdown."),
    ok;
terminate(normal, _State, _Connector) ->
    erlog:info("Connector terminated normaly."),
    ok;
terminate(Reason, #?St{addr = Addr, port = Port}, Connector) ->
    erlog:warn("Connector terminated: ~p.", [Reason]),
    Peer = make_peer_(Addr, Port),
    gen_connector:inner_connection_failed(Peer, Reason, Connector),
    ok.


%% ====================================================================
%% Internal Functions
%% ====================================================================

make_peer_(Addr, Port) -> {tcp, {Addr, Port}}.

prepare_(Addr, Port) when is_list(Addr), is_integer(Port) ->
    [{addr, Addr}, {port, Port}].

initialize_(St, Config) ->
    erlog:log("Configuring TCP port with ~p...", [Config]),
    initialize_(St, [], [], Config).

initialize_(St, OAcc, [], []) ->
    SockOpt = optlib:set_defaults(?DEFAULT_SOCK_OPTS, lists:reverse(OAcc)),
    check_port_(St#?St{sockopts = SockOpt});
initialize_(St, OAcc, AAcc, [{port, Port} |Rest]) ->
    initialize_(St#?St{port = Port}, OAcc, AAcc, Rest);
initialize_(St, OAcc, AAcc, [{addr, Addr} |Rest]) ->
    initialize_(St#?St{addr = Addr}, OAcc, AAcc, Rest);
initialize_(St, OAcc, AAcc, [{keepalive, _} = Opt |Rest]) ->
    initialize_(St, [Opt |OAcc], AAcc, Rest);
initialize_(_St, _OAcc, _AAcc, [Opt |_Rest]) ->
    erlang:error({bad_config, Opt}).

check_port_(#?St{port = undefined}) ->
    erlang:error(no_port_specified);
check_port_(St) -> {ok, St}.

init_connector_(St) -> {ok, St}.

tcp_connect_(State, Connector) ->
    #?St{addr = Addr, port = Port, timeout = To, sockopts = SockOpts} = State,
    Peer = make_peer_(Addr, Port),
    case gen_tcp:connect(Addr, Port, SockOpts, To) of
        {ok, Sock} ->
            erlog:info("Connected to ~s.", [format:peer(Peer)]),
            delegate_connection_(State, Connector, Sock);
        {error, Reason} ->
            ok = gen_connector:inner_connection_failed(Peer, Reason, Connector),
            {stop, normal, State}
    end.

delegate_connection_(State, Connector, Sock) ->
    erlog:log("Building TCP input/output stages..."),
    {ok, Peer} = inet:peername(Sock),
    {ok, Proto} = gen_connector:inner_create_protocol(Connector),
    {ok, Producer} = stage_tcp_producer:new(Proto, Sock),
    {ok, Consumer} = stage_tcp_consumer:new(Proto, Sock),
    ok = gen_connector:inner_connect_protocol({tcp, Peer}, Proto,
                                             [Producer], [Consumer], Connector),
    {stop, normal, State}.
