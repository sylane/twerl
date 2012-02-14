%% ===========================================================================
%% @doc        Twerl Framework.
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

-module(twerl_app).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(application).

-erlog_category(twerl_app).


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% Behaviour application callbacks
-export([start/2,
         stop/1]).


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.

-type start_error() :: {already_started, pid()} | shutdown | term().


%% ====================================================================
%% Behaviour application Callbacks
%% ====================================================================


%% --------------------------------------------------------------------
%% @doc application beaviour start callback.

-spec start(start_type(), []) -> {ok, pid()} | {error, start_error()}.

start(_Type, []) ->
    erlog:info("Starting twerl..."),
    twerl_sup:start_link().


%% --------------------------------------------------------------------
%% @doc application beaviour stop callback.

-spec stop(term()) -> any().

stop(_State) ->
    erlog:info("Stopping twerl..."),
    ok.
