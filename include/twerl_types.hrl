%% ===========================================================================
%% Global types include file.
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

-ifndef(IS_TWERL_TYPES_INCLUDED).
-define(IS_TWERL_TYPES_INCLUDED, true).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("kernel/include/inet.hrl").


%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type custom_opt() :: {atom(), any()} | atom().

-type error() :: atom().

-type stop_reason() :: normal | shutdown | term().

-type terminate_reason() :: normal | shutdown | term().

-type twerl_peer_type() :: tcp.

-type twerl_peer() :: {twerl_peer_type(),
                       {Addr::ip_address(), Port::ip_port()}}.

-type twerl_def() :: {function(), module(), any()}.

-type twerl_stage_opt() :: custom_opt().

-type twerl_stage_opts() :: [twerl_stage_opt()].

-type twerl_stage_mode() :: push | pull.

-type twerl_stage_format() :: any().

-type twerl_stage_formats() :: [twerl_stage_format()].

-type twerl_stage() :: tuple().

-type twerl_stages() :: [twerl_stage()].

-type twerl_stage_data() :: any().

-type twerl_stage_packets() :: [twerl_stage_data()].

-type twerl_stage_query() :: any().


-type twerl_service_name() :: atom().


-type twerl_proto_opt() :: custom_opt().

-type twerl_proto_opts() :: [twerl_proto_opt()].

-type twerl_protocol() :: pid().

-type twerl_proto_key() :: twerl_protocol() | term().

-type twerl_proto_parent() :: twerl_protocol() | undefined.


-type twerl_factory_opt() :: custom_opt().

-type twerl_factory_opts() :: [twerl_factory_opt()].

-type twerl_factory() :: pid().


-type twerl_acceptor_proto() :: tcp.

-type twerl_acceptor_opt() :: {protocol, twerl_acceptor_proto()}
                              | {port, integer()} | custom_opt().

-type twerl_acceptor_opts() :: [twerl_acceptor_opt()].

-type twerl_acceptor() :: pid().


-endif.
