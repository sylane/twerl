%% ===========================================================================
%% @doc        Service supervisor.
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

-module(twerl_service_sup).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(service_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([get_service/1,
         get_acceptor_sup/1,
         get_connector_sup/1,
         get_factory_group/1]).

%% Startup exports
-export([start_link/3]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Gets the service process for the specified service supervisor. 

get_service(ServSup) ->
    twerl_utils:get_supervisor_child(ServSup, service).


%% --------------------------------------------------------------------
%% @doc Gets the acceptor supervidor for the specified service supervisor.

get_acceptor_sup(ServSup) ->
    twerl_utils:get_supervisor_child(ServSup, acceptor_sup).


%% --------------------------------------------------------------------
%% @doc Gets the connector supervidor for the specified service supervisor.

get_connector_sup(ServSup) ->
    twerl_utils:get_supervisor_child(ServSup, connector_sup).


%% --------------------------------------------------------------------
%% @doc Gets the factory group for the specified service supervisor.

get_factory_group(ServSup) ->
    twerl_utils:get_supervisor_child(ServSup, factory_group).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a service supervisor.
%% --------------------------------------------------------------------
start_link(Name, ServMod, ServConf) ->
    erlog:log("Starting ~w service ~w supervisor...", [ServMod, Name]),
    supervisor:start_link(?MODULE, {Name, ServMod, ServConf}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({Name, ServMod, ServConf}) ->
    erlog:set_name(Name),
    erlog:info("Initializing ~w service supervisor...", [ServMod]),

    % Resolve the context PID, it the context die everything will be restarted
    Context = whereis(Name),
    % Just to be sure...
    true = is_pid(Context),

    ServWkrArgs = [Name, Context, ServMod, ServConf],
    ServWkrDeps = uniquify_(gen_service, ServMod),
    ServWkrCons = {gen_service, start_link, ServWkrArgs},
    ServWkrSpec = {service, ServWkrCons, transient, 2000, worker, ServWkrDeps},

    FactGrpDeps = [twerl_context, twerl_acceptor_sup],
    FactGrpConsArgs = [Context, factory_group, twerl_factory_group, [Name]],
    FactGrpConsCall = {twerl_context, start_supervisor, FactGrpConsArgs},
    FactGrpSpec = {factory_group, FactGrpConsCall, transient,
                   infinity, supervisor, FactGrpDeps},

    AcceptSupDeps = [twerl_context, twerl_acceptor_sup],
    AcceptSupConsArgs = [Context, acceptor_sup, twerl_acceptor_sup, [Name]],
    AcceptSupConsCall = {twerl_context, start_supervisor, AcceptSupConsArgs},
    AcceptSupSpec = {acceptor_sup, AcceptSupConsCall, transient,
                   infinity, supervisor, AcceptSupDeps},

    ConnSupDeps = [twerl_context, twerl_connector_sup],
    ConnSupConsArgs = [Context, connector_sup, twerl_connector_sup, [Name]],
    ConnSupConsCall = {twerl_context, start_supervisor, ConnSupConsArgs},
    ConnSupSpec = {connector_sup, ConnSupConsCall, transient,
                   infinity, supervisor, ConnSupDeps},

    Specs = [ServWkrSpec, FactGrpSpec, AcceptSupSpec, ConnSupSpec],
    {ok, {{one_for_all, 1, 1}, Specs}}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

uniquify_(Name, Name) -> [Name];
uniquify_(Name1, Name2) -> [Name1, Name2].
