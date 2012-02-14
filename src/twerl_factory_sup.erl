%% ===========================================================================
%% @doc        Factory supervisor.
%% @since      Apr 9, 2010
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

-module(twerl_factory_sup).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(fact_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([get_factory/1,
         get_protocol_sup/1]).

%% Startup exports
-export([start_link/4]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Gets the factory process for the specified factory supervisor.

get_factory(FactSup) ->
    twerl_utils:get_supervisor_child(FactSup, factory).


%% --------------------------------------------------------------------
%% @doc Gets the protocol supervisor for the specified factory supervisor.

get_protocol_sup(FactSup) ->
    twerl_utils:get_supervisor_child(FactSup, proto_sup).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links a factory supervisor.

start_link(Name, Context, FactMod, FactConf) ->
    erlog:log("Starting ~w factory supervisor for service ~w...",
              [FactMod, Name]),
    supervisor:start_link(?MODULE, {Name, Context, FactMod, FactConf}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({Name, Context, FactMod, FactConf}) ->
    erlog:set_name(Name),
    erlog:info("Initializing ~w factory supervisor...", [FactMod]),

    FactWkrArgs = [Name, Context, FactMod, FactConf, self()],
    FactWkrDeps = uniquify_(gen_factory, FactMod),
    FactWkrCons = {gen_factory, start_link, FactWkrArgs},
    FactWkrSpec = {factory, FactWkrCons, transient,
                   2000, worker, FactWkrDeps},

    ProtoSupDeps = [twerl_context, twerl_protocol_sup],
    ProtoSupConsArgs = [Context, proto_sup, twerl_protocol_sup, [Name]],
    ProtoSupConsCall = {twerl_context, start_supervisor, ProtoSupConsArgs},
    ProtoSupSpec = {proto_sup, ProtoSupConsCall, transient,
                    infinity, supervisor, ProtoSupDeps},

    {ok, {{one_for_all, 1, 1}, [FactWkrSpec, ProtoSupSpec]}}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

uniquify_(Name, Name) -> [Name];
uniquify_(Name1, Name2) -> [Name1, Name2].
