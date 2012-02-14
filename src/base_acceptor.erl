%% ===========================================================================
%% @doc        Base module for twerl acceptors declaring all required
%%             callbacks and implementing some when meaningful.
%% @since      Apr 10, 2010
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

-module(base_acceptor).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(gen_acceptor).

-erlog_category(acceptor).


%% --------------------------------------------------------------------
%% Includes
%% -------------------------------------------------------------------

-include("twerl.hrl").

-include("base_acceptor.specs.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([]).

%% Behaviour gen_acceptor callbacks
-export([init/1,
         setup/2,
         handle_message/3,
         start_accepting/2,
         pause_accepting/2,
         resume_accepting/2,
         stop_accepting/2,
         terminate/3,
         code_change/3]).


%% ====================================================================
%% Behaviour gen_acceptor Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Called to initialise acceptor's process state.

init(_Args) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called to setup acceptor process.

setup(State, _Acceptor) ->
    {ok, State}.


%% --------------------------------------------------------------------
%% @doc Called to handle process messages.

handle_message(Msg, _State, _Acceptor) ->
    erlang:error({unexpected_msg, Msg}).

%% --------------------------------------------------------------------
%% @doc Called to start accepting for connections.

start_accepting(_State, _Acceptor) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called to pause accepting for connections.

pause_accepting(_State, _Acceptor) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called to resume accepting for connections.

resume_accepting(_State, _Acceptor) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called to stop accepting for connections.

stop_accepting(_State, _Acceptor) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called when acceptor process is terminating.

terminate(shutdown, _State, _Acceptor) ->
    erlog:info("Acceptor shutdown."),
    ok;
terminate(normal, _State, _Acceptor) ->
    erlog:info("Acceptor terminated normaly."),
    ok;
terminate(Reason, _State, _Acceptor) ->
    erlog:warn("Acceptor terminated: ~p.", [Reason]),
    ok.


%% --------------------------------------------------------------------
%% @doc Called to upgrade acceptor's process state.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
