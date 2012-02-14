%% ===========================================================================
%% @doc        Protocol supervisor.
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

-module(twerl_protocol_sup).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(proto_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([start_protocol/7]).

%% External exports
-export([start_link/1]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Requests the protocol supervisor to starts a protocol process.

start_protocol(Supervisor, Context, Factory,
               Parent, Args, ProtoMod, ProtoConf) ->
    erlog:log("Supervising ~w protocol...", [ProtoMod]),
    ProtoWkrArgs = [Context, Factory, Parent, Args, ProtoMod, ProtoConf],
    supervisor:start_child(Supervisor, ProtoWkrArgs).


%% ====================================================================
%% External Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a protocol supervisor.

start_link(Name) ->
    erlog:log("Starting protocol supervisor for service ~w...", [Name]),
    supervisor:start_link(?MODULE, {Name}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({Name}) ->
    erlog:set_name(Name),
    erlog:info("Initializing protocol supervisor for service ~w...", [Name]),
    ProtoWkrSpec = {proto, {gen_protocol, start_link, []},
                    temporary, 2000, worker, dynamic},
    {ok, {{simple_one_for_one, 0, 1}, [ProtoWkrSpec]}}.
