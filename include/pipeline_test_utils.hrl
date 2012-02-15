%% ===========================================================================
%% Global include file for twerl's pipeline and stages unit tests.
%% @since      Apr 25, 2010
%% @version    1.0
%% @copyright  (c) 2009, Sebastien Merle <s.merle@gmail.com>
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

-ifndef(IS_PIPELINE_TEST_UTILS_INCLUDED).
-define(IS_PIPELINE_TESTS_UTILS_INCLUDED, true).

-erlog_category(test).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-include_lib("erlib/include/erlog.hrl").


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(MODULE_COVERAGE(Package, Module),
        CorePackage = "lib/core",
        RelBeamPath = io_lib:format("ebin/~w.beam", [Module]),
        RelSourcePath = io_lib:format("~s/src/~w.erl", [Package, Module]),
        RelReportPath = io_lib:format("~s/test/coverage_~w.html", [Package, Module]),
        CorePkgIncludePath = io_lib:format("~s/include", [CorePackage]),
        RelPkgIncludePath = io_lib:format("~s/include", [Package]),
        BeamPath = code:which(Module),
        RootPath = re:replace(BeamPath, RelBeamPath, "", [{return, list}]),
        SourcePath = RootPath ++ RelSourcePath,
        ReportPath = RootPath ++ RelReportPath,
        CoreIncludePath = RootPath ++ CorePkgIncludePath,
        IncludePath = RootPath ++ RelPkgIncludePath,
        {ok, Module} = cover:compile(SourcePath, [{i, CoreIncludePath},
                                                  {i, IncludePath}]),
        ?MODULE:test(),
        cover:analyse_to_file(Module, ReportPath, [html]),
        code:purge(Module),
        code:delete(Module)).


-define(MK_TEST_CONSUME(P, Check, Input, Expected),
        fun() -> Value = case P:consume_packets(Input) of
                             {more, Data, _} -> Data;
                             {eos, Data, _} -> Data
                         end,
				 Check(Expected, Value)

        end).


-endif.
