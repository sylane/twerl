%% ===========================================================================
%% @doc        Supervisor of all context supervisors. 
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

-module(twerl_context_group).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(supervisor).

-erlog_category(bunch_sup).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([start_context_sup/3,
         remove_context_sup/1]).

%% External exports
-export([start_link/0]).

%% Behaviour supervisor callbacks
-export([init/1]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% Supervisor name
-define(PROCESS_NAME, twerl_contexts).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Requests the context group supervisor to start a context supervisor.

start_context_sup(Name, ServMod, ServConf) ->
    erlog:log("Supervising context supervisor for ~w service ~w...",
              [ServMod, Name]),
    CtxSupArgs = [Name, ServMod, ServConf],
    CtxSupSpec = {Name, {twerl_context_sup, start_link, CtxSupArgs},
                  transient, infinity, supervisor, [twerl_context_sup]},
    supervisor:start_child(?PROCESS_NAME, CtxSupSpec).


%% --------------------------------------------------------------------
%% @doc Requests the context group supervisor to terminate and remove
%% the context supervisor with specified name.

remove_context_sup(Name) ->
    erlog:log("Removing context supervisor for service ~w...", [Name]),
    case supervisor:terminate_child(?PROCESS_NAME, Name) of
        ok -> supervisor:delete_child(?PROCESS_NAME, Name);
        {error, _} = Error -> Error
    end.


%% ====================================================================
%% External Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links the context group supervisor.

start_link() ->
    erlog:log("Starting bunch supervisor..."),
    supervisor:start_link({local, ?PROCESS_NAME}, ?MODULE, {}).


%% ====================================================================
%% Behaviour supervisor Functions
%% ====================================================================

init({}) ->
    erlog:info("Initializing bunch supervisor..."),
    {ok, {{one_for_one, 1, 1}, []}}.
