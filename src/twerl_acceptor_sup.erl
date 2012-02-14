%% ===========================================================================
%% @doc        Acceptor supervisor.
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
%%     notice, this Accept of conditions and the following disclaimer.
%%   * Redistributions in binary form must reproduce the above copyright
%%     notice, this Accept of conditions and the following disclaimer in the
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

-module(twerl_acceptor_sup).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(acceptor_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([start_acceptor/5]).

%% External exports
-export([start_link/1]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Requests the acceptor supervidor to start a acceptor process.

start_acceptor(Supervisor, Name, Context, AcceptMod, AcceptConf) ->
    erlog:log("Supervising ~w acceptor for service ~w...", [AcceptMod, Name]),
    AcceptWkrArgs = [Name, Context, AcceptMod, AcceptConf],
    AcceptWkrDeps = uniquify_(gen_acceptor, AcceptMod),
    Constructor = {gen_acceptor, start_link, AcceptWkrArgs},
    AcceptWkrSpec = {acceptor, Constructor, transient,
                   2000, worker, AcceptWkrDeps},
    supervisor:start_child(Supervisor, AcceptWkrSpec).


%% ====================================================================
%% External Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a acceptor supervisor.

start_link(Name) ->
    erlog:log("Starting acceptor supervisor for service ~w...", [Name]),
    supervisor:start_link(?MODULE, {Name}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({Name}) ->
    erlog:set_name(Name),
    erlog:info("Initializing acceptor supervisor..."),
    {ok, {{one_for_one, 1, 1}, []}}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

uniquify_(Name, Name) -> [Name];
uniquify_(Name1, Name2) -> [Name1, Name2].
