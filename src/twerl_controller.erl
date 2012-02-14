%% ===========================================================================
%% @doc        Twerl controller server.
%% @since      Jul 27, 2010
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

-module(twerl_controller).

-author('Sebastien Merle <s.merle@gmail.com>').

-extends(base_server).

-erlog_category(controller).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([create_service/2,
         stop_service/1,
         stop_service_async/1]).

%% Startup exports
-export([start_link/0, start_link/1]).

%% Overriden base_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Controller's state record name
-define(St, ?MODULE).

% Controller process registration name
-define(PROCESS_NAME, ?MODULE).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Controller's state
-record(?St, {}).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates and starts a service with specified name and definition. 

create_service(Name, ServiceDef) ->
    {Constructor, Module, Config} = ServiceDef,
    Constructor(Name, Module, Config).


%% --------------------------------------------------------------------
%% @doc Stops the service with specified name.

stop_service(Name) ->
    gen_server:call(?PROCESS_NAME, {stop_service, Name}).


%% --------------------------------------------------------------------
%% @doc Requests asynchronously to stop the service with specified name.

stop_service_async(Name) ->
    gen_server:cast(?PROCESS_NAME, {stop_service, Name}).


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and link the twerl controller process.

start_link() ->
    start_link([]).


%% --------------------------------------------------------------------
%% @doc Starts and link the twerl controller process with extra options.

start_link(Options) ->
    erlog:log("Starting twerl controller..."),
    gen_server:start_link({local, ?PROCESS_NAME}, ?MODULE, {}, Options).


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

init({}) ->
    erlog:info("Initializing controller..."),
    {ok, #?St{}}.

handle_call({stop_service, Name}, _From, State) ->
    {Result, NewState} = stop_service_(State, Name),
    {reply, Result, NewState};
handle_call(Request, From, State) ->
    base_server:handle_call(Request, From, State).

handle_cast({stop_service, Name}, State) ->
    {_, NewState} = stop_service_(State, Name),
    {noreply, NewState};
handle_cast(Msg, State) ->
    base_server:handle_info(Msg, State).


%% ====================================================================
%% Internal Functions
%% ====================================================================

stop_service_(State, Name) ->
    {twerl_context_group:remove_context_sup(Name), State}.
