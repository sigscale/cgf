%%% cgf_3gpp_file.hrl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Header file for 3GPP TS 32.297 Charging Data Records (CDR).

-record(cdr_file_header,
		{low :: cgf_3gpp_file:rel_ver(),
		high :: cgf_3gpp_file:rel_ver(),
		open :: cgf_3gpp_file:timestamp(),
		append :: cgf_3gpp_file:timestamp(),
		count :: non_neg_integer(),
		sequence :: non_neg_integer(),
		reason ::  cgf_3gpp_file:close_reason(),
		address :: inet:ip6_address(),
		lost = {'==', 0} :: cgf_3gpp_file:lost_cdr(),
		filter = <<>> :: binary(),
		private = <<>> :: binary()}).

-record(cdr_header,
		{ts :: ofcs_cdr:ts_number(),
		release :: non_neg_integer(),
		version :: non_neg_integer(),
		format = ber :: cgf_3gpp_file:cdr_format()}).

