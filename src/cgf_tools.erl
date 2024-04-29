%%% cgf_tools.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2024 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%%   module starts and stops the {@link //cgf. cgf} application.
%%%
-module(cgf_tools).
-copyright('Copyright (c) 2024 SigScale Global Inc.').
-author('Vance Shipley <vances@sigscale.org>').

-export([transpile/1, transpile/2]).

-include_lib("asn1/src/asn1_records.hrl").

-define(METASCHEMA, "http://json-schema.org/draft-07/schema#").
-define(BASE_ID, "http://sigscale.org/schema/cgf/").

%%----------------------------------------------------------------------
%%  The cgf_tools public API
%%----------------------------------------------------------------------

-spec transpile(Filename) -> Result
	when
		Filename :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Generate a JSON Schema from an ASN.1 module.
transpile(Filename) ->
	Basename = filename:basename(Filename),
	Rootname = filename:rootname(Basename),
	Dirname = filename:dirname(Filename),
	Schemaname = lists:flatten([Dirname, $/, Rootname, ".json"]),
	transpile(Filename, Schemaname).

-spec transpile(Asn1Filename, JsonSchemaFilename) -> Result
	when
		Asn1Filename :: file:filename(),
		JsonSchemaFilename :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Generate a JSON Schema from an ASN.1 module.
transpile(Asn1Filename, JsonSchemaFilename) ->
	Tokens = asn1ct_tok:file(Asn1Filename),
	case asn1ct_parser2:parse(Asn1Filename, Tokens) of
		{ok, #module{name = Name,
				imports = {imports, Imports},
				typeorval = TypeOrVal}} ->
			Refs = get_refs(Imports, #{}),
			Defs = get_typedefs(TypeOrVal, Refs, [], #{}),
			Schema = #{"$schema" => "http://json-schema.org/draft-07/schema#",
					"$id" => ?BASE_ID ++ atom_to_list(Name),
   				"$defs" => Defs},
			file:write_file(JsonSchemaFilename, zj:encode(Schema));
		{error, Reason} ->
			{error, Reason}
	end.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
get_refs([#'SymbolsFromModule'{symbols = Symbols} | T], Acc) ->
	get_refs(T, get_symbols(Symbols, Acc));
get_refs([], Acc) ->
	Acc.
%% @hidden
get_symbols([#'Externaltypereference'{module = Module,
		type = Type} | T], Acc) ->
	Ref = lists:flatten([$/, atom_to_list(Module),
			"#/$defs/", atom_to_list(Type)]),
	get_symbols(T, Acc#{Type => Ref});
get_symbols([], Acc) ->
	Acc.

%% @hidden
get_typedefs([#typedef{name = Name, typespec = TypeSpec} = H | T],
		Refs, Pending, Acc) ->
	case get_type(TypeSpec, Refs) of
		Type when is_map(Type) ->
			Ref = lists:flatten(["#/$defs/", atom_to_list(Name)]),
			get_typedefs(T, Refs#{Name => Ref}, Pending, Acc#{Name => Type});
		undefined ->
			get_typedefs(T, Refs, [H | Pending], Acc)
	end;
get_typedefs([#ptypedef{name = Name, typespec = TypeSpec} = H | T],
		Refs, Pending, Acc) ->
	case get_type(TypeSpec, Refs) of
		Type when is_map(Type) ->
			Ref = lists:flatten(["#/$defs/", atom_to_list(Name)]),
			get_typedefs(T, Refs#{Name => Ref}, Pending, Acc#{Name => Type});
		undefined ->
			get_typedefs(T, Refs, [H | Pending], Acc)
	end;
get_typedefs([], _Refs, [], Acc) ->
	Acc;
get_typedefs([], Refs, Pending, Acc) ->
	get_typedefs(Pending, Refs, [], Acc).

%% @hidden
get_type(#type{def = 'BOOLEAN'}, _Refs) ->
	#{"type" => "boolean"};
get_type(#type{def = 'INTEGER', constraint = Constraints}, _Refs) ->
	#{"type" => "integer"};
get_type(#type{def = {'INTEGER', _NamedNumbers}, constraint = []}, _Refs) ->
	#{"type" => "integer"};
get_type(#type{def = {'ENUMERATED', _NamedNumbers}, constraint = []}, _Refs) ->
	#{"type" => "integer"};
get_type(#type{def = 'OCTET STRING', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[0-9a-fA-F]*$"};
get_type(#type{def = 'BIT STRING', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[0-9a-fA-F]*$"};
get_type(#type{def = {'BIT STRING', _NamedNumbers},
		constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[0-9a-fA-F]*$"};
get_type(#type{def = 'UTF8String', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'IA5String', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[\x00-\x7F]*$"};
get_type(#type{def = 'VisibleString', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[\x20-\x7E]*$"};
get_type(#type{def = 'PrintableString', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[a-zA-Z0-9 '()+,-./:=?]*$"};
get_type(#type{def = 'NumericString', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[0-9 ]*$"};
get_type(#type{def = 'GraphicString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'GeneralString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'BMPString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'UniversalString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'TeletexString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'VideotexString', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'NULL', constraint = Constraints}, _Refs) ->
	#{"type" => "null"};
get_type(#type{def = 'GeneralizedTime', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^[0-9]{14}([,.][0-9]{1,3})?Z?$"};
get_type(#type{def = 'UTCTime', constraint = Constraints}, _Refs) ->
	#{"type" => "string", "pattern" => "^([0-9]{10}|[0-9]{12})(Z|[+-]{1}[0-9]{4})$"};
get_type(#type{def = 'ObjectDescriptor', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'OBJECT IDENTIFIER', constraint = Constraints}, _Refs) ->
	#{"type" => "string"};
get_type(#type{def = 'REAL', constraint = Constraints}, _Refs) ->
	#{"type" => "number"};
get_type(#type{def =  {'CHOICE', Components}, constraint = []}, Refs) ->
	case get_component(Components, Refs, []) of
		Types when is_list(Types) ->
			#{"oneOf" => Types};
		undefined ->
			undefined
	end;
get_type(#type{def = #'SEQUENCE'{components = Components},
		constraint = []}, Refs) ->
	case get_component(Components, Refs, #{}) of
		Types when is_map(Types) ->
			#{"type" => "object", "properties" => Types};
		undefined ->
			undefined
	end;
get_type(#type{def = #'SET'{components = Components},
		constraint = []}, Refs) ->
	case get_component(Components, Refs, #{}) of
		Types when is_map(Types) ->
			#{"type" => "object", "properties" => Types};
		undefined ->
			undefined
	end;
get_type(#type{def = {'SEQUENCE OF', TypeSpec}, constraint = []}, Refs) ->
	case get_type(TypeSpec, Refs) of
		Type when is_map(Type) ->
			#{"type" => "array", "items" => Type};
		undefined ->
			undefined
	end;
get_type(#type{def = {'SET OF', TypeSpec}, constraint = []}, Refs) ->
	case get_type(TypeSpec, Refs) of
		Type when is_map(Type) ->
			#{"type" => "array", "items" => Type};
		undefined ->
			undefined
	end;
get_type(#type{def = #'Externaltypereference'{type = Type},
		constraint = []}, Refs) when is_map_key(Type, Refs) ->
	#{"type" => #{"$ref" => maps:get(Type, Refs)}};
get_type(#type{def = #'Externaltypereference'{}}, _Refs) ->
	undefined;
get_type(#type{def = {pt, #'Externaltypereference'{type = Type}, _},
		constraint = []}, Refs) when is_map_key(Type, Refs) ->
	#{"type" => #{"$ref" => maps:get(Type, Refs)}};
get_type(#type{def = {pt, #'Externaltypereference'{}, _}}, _Refs) ->
	undefined;
get_type(#type{def = {'ANY_DEFINED_BY', _}}, _Refs) ->
	#{}.

get_component([#'ComponentType'{name = Name, typespec = TypeSpec} | T],
		Refs, Acc) ->
	case get_type(TypeSpec, Refs) of
		Type when is_map(Type), is_list(Acc) ->
			get_component(T, Refs, [#{Name => Type} | Acc]);
		Type when is_map(Type), is_map(Acc) ->
			get_component(T, Refs, Acc#{Name => Type});
		undefined ->
			undefined
	end;	
get_component([], Refs, Acc) when is_list(Acc) ->
	lists:reverse(Acc);
get_component([], Refs, Acc) when is_map(Acc) ->
	Acc.
