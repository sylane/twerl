%% ===========================================================================
%% @doc        Base module for twerl stages declaring all required
%%             callbacks and implementing some when meaningful.
%% @since      May 28, 2010
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

-module(base_stage).

-author('Sebastien Merle <s.merle@gmail.com>').

-erlog_category(stage).


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% Pipeline callbacks exports
-export([init/1,
         negotiate/2,
         agree/3,
         setup/3,
         process/3,
         continue/3]).


%% ====================================================================
%% Pipeline Callback Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Called to initialize the pipeline stage.

init(State) -> {ok, State}.


%% --------------------------------------------------------------------
%% @doc Called to negotiate input format.
%% Tell me what you want I'll tell you what I need for it.

negotiate(_OutFmts, _State)    -> rejected.


%% --------------------------------------------------------------------
%% @doc Called to agree on a negotiated formats.
%% Should fixate the output format, it cannot be generic (any)
%% and should be cannonical ({binary, block} instead of binary).  

agree(_InFmt, _OutFmt, _State) -> rejected.


%% --------------------------------------------------------------------
%% @doc Called to setup the pipeline stage.

setup(_InFmt, _OutFmt, State) -> {ok, State}.


%% --------------------------------------------------------------------
%% @doc Called to process some data or query.

process(_Query, _Pipe, _State) -> erlang:error("not_implemented").


%% --------------------------------------------------------------------
%% @doc Called to continue data processing.

continue(_Query, _Pipe, _State) -> erlang:error("not_implemented").
