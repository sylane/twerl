%% ===========================================================================
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

-ifndef(IS_GEN_FACTORY_SPECS_INCLUDED).
-define(IS_GEN_FACTORY_SPECS_INCLUDED, true).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl_types.hrl").


%% --------------------------------------------------------------------
%% Specifications
%% --------------------------------------------------------------------

%% API Functions

%FIXME: Add API functions specs.


%% Startup Functions

%FIXME: Add Startup functions specs.


%% Behaviour registration Function

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].


%% Overriden base_server callbacks

-spec init({ServName::twerl_service_name(), Context::pid(),
            Module::module(), Config::list(), FactSup::pid()}) ->
          {ok, State::term()}
        | {stop, Reason::stop_reason()}.

-spec handle_call(Request::term(), From::term(), State::term()) ->
          {reply, Reply::term(), State::term()}.

-spec handle_cast(Msg::term(), State::term()) ->
          {noreply, State::term()}
        | {stop, Reason::stop_reason(), State::term()}.

-spec terminate(Reason::terminate_reason(), State::term()) ->
          any().

-spec code_change(OldVsn::term(), State::term(), Extra::term()) ->
          {ok, NewState::term()}.


-endif.
