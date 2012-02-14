%% ===========================================================================
%% @doc        Context supervisor.
%% @since      Apr 12, 2010
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

-module(twerl_context_sup).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(context_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([get_service_sup/1,
         start_service_sup/5]).

%% Startup exports
-export([start_link/3]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Returns the service supervisor for the specified context supervisor.

get_service_sup(CtxSup) ->
    twerl_utils:get_supervisor_child(CtxSup, service_sup).


%% --------------------------------------------------------------------
%% @doc Adds a service supervisor to the specified supervisor and starts it.
%%
%% The service supervisor in temporary because it's started by the context
%% and shouldn't be restarted automatically when the context is restarted.
%% The context itself will monitor it and restart it if needed.

start_service_sup(Supervisor, Name, Context, ServMod, ServConf) ->
    erlog:log("Supervising ~w service ~w supervisor...", [ServMod, Name]),
    ServSupDeps = [twerl_context, twerl_service_sup],
    ServSupArgs = [Name, Context, ServMod, ServConf],
    ServSupConsArgs = [Context, service_sup, twerl_service_sup, ServSupArgs],
    ServSupConsCall = {twerl_context, start_supervisor, ServSupConsArgs},
    ServSupSpec = {service_sup, ServSupConsCall, temporary,
                   infinity, supervisor, ServSupDeps},
    supervisor:start_child(Supervisor, ServSupSpec).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a context supervisor.

start_link(Name, ServMod, ServConf) ->
    erlog:log("Starting context supervisor for ~w service ~w...",
              [ServMod, Name]),
    supervisor:start_link(?MODULE, {Name, ServMod, ServConf}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({Name, ServMod, ServConf}) ->
    erlog:set_name(Name),
    erlog:info("Initializing context supervisor for ~w service...", [ServMod]),

    CtxWkr = {context, {twerl_context, start_link, [Name]},
              transient, 2000, worker, [twerl_context]},

    ServSupArgs = [Name, ServMod, ServConf],
    ServSupSpec = {service_sup, {twerl_service_sup, start_link, ServSupArgs},
                   transient, infinity, supervisor, [twerl_service_sup]},

    {ok, {{one_for_all, 1, 1}, [CtxWkr, ServSupSpec]}}.
