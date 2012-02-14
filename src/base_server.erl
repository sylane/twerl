%% ===========================================================================
%% @doc        Base module for generic servers declaring all required
%%             callbacks and implementing some when meaningful.
%% @since      Apr 09, 2010
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

-module(base_server).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(gen_server).

-erlog_category(server).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("base_server.specs.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% debug exports
-export([dump_state/1]).

%% Behaviour gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% ====================================================================
%% Debug Functions
%% ====================================================================

dump_state(Server) ->
    gen_server:call(Server, dump_state).


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Called to initialise server's process state. 

init(_Args) ->
    erlang:error(not_implemented).


%% --------------------------------------------------------------------
%% @doc Called to handle process calls.

handle_call(dump_state, _From, State) ->
    {reply, State, State};
handle_call(Request, _From, _State) ->
    erlang:error({unexpected_call, Request}).


%% --------------------------------------------------------------------
%% @doc Called to handle process cast.

handle_cast(Msg, _State) ->
    erlang:error({unexpected_cast, Msg}).


%% --------------------------------------------------------------------
%% @doc Called to handle process messages.

handle_info(Info, _State) ->
    erlang:error({unexpected_info, Info}).


%% --------------------------------------------------------------------
%% @doc Called when server process is terminating.

terminate(_Reason, _State) ->
    ok.


%% --------------------------------------------------------------------
%% @doc Called to upgrade server's process state.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
