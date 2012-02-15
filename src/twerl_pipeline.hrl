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

-ifndef(IS_TWERL_PIPELINE_INCLUDED).
-define(IS_TWERL_PIPELINE_INCLUDED, true).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("twerl.hrl").

-include("twerl_types.hrl").


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Pipeline's state record name
-define(Pl, twerl_pipeline).

% Macro usable in pattern matching
-define(PIPELINE_MATCH_INPUT(F), #?Pl{input = F}).
-define(PIPELINE_MATCH_OUTPUT(F), #?Pl{output = F}).

% Macro to access public attributes
-define(PIPELINE_GET_INPUT(P), (P)#?Pl.input).
-define(PIPELINE_GET_OUTPUT(P), (P)#?Pl.output).


%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% Pipeline's state
-record(?Pl, {input,
              output,
              % The rest is opaque and shouldn't be used directly
              prev_fmts,
              next_fmts,
              prevs,
              curr,
              nexts,
              target}).


-endif.
