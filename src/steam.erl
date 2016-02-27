%%%-------------------------------------------------------------------
%%% File:      steam.erl
%%% @author    Eric Pailleau <geas@crownedgrouse.com>
%%% @copyright 2016 crownedgrouse.com
%%% @doc  
%%% Search Tags in Erlang Application or Module
%%% @end  
%%%
%%% Permission to use, copy, modify, and/or distribute this software
%%% for any purpose with or without fee is hereby granted, provided
%%% that the above copyright notice and this permission notice appear
%%% in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
%%% WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
%%% AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2016-02-21
%%%-------------------------------------------------------------------
-module(steam).
-author("Eric Pailleau <steam@crownedgrouse.com>").

-export([facets/0, facets/1, tags/0, tags/1]).

-include("steam_db.hrl").
-include("steam_use.hrl").

%%-------------------------------------------------------------------------
%% @doc Return list of handled tags
%% @end
%%-------------------------------------------------------------------------

tags() -> lists:usort(?Tags).

%%-------------------------------------------------------------------------
%% @doc Return lists of handled facets
%% @end
%%-------------------------------------------------------------------------

facets() -> lists:usort(lists:flatmap(fun(X) ->  Y = atom_to_list(X),
									 [F | _] = string:tokens(Y, ":" ),
							         [list_to_atom(F)]
						  end, tags())).

%%-------------------------------------------------------------------------
%% @doc Return all facets
%% @end
%%-------------------------------------------------------------------------

facets(all) -> lists:usort(?Facets).

%%-------------------------------------------------------------------------
%% @doc Return tags found in a Erlang application root path
%% @end
%%-------------------------------------------------------------------------

tags(Path) -> 

	try
		{ok, Res} = geas:info(Path),
		% Extract some needed informations
		{name, RawName} = lists:keyfind(name, 1, Res),
		Name = case RawName  of
					undefined -> list_to_atom(filename:basename(Path)) ;
					_  -> RawName
			   end,
		{driver, Driver} = lists:keyfind(driver, 1, Res),
		Calls = case get(geas_calls) of
					 undefined -> [] ;
					 Call -> Call
				end,

		% Search tags from function calls
		TagsCalls = lists:flatmap(fun(X) -> [tag({call, Name, X})] end, Calls),

		% Search tags from function exports
		TagsExports = lists:flatmap(fun(X) -> [tag({export, Name, X})] end, []),

        % Search tags from other geas information
        TagsApp = [tag({application, Name, []}),
				   implemented_in(Driver, Path),
				   use(Name)
				  ],

		% Clean up
		put(geas_calls, undefined), 

		% Unique results
		{ok, lists:usort(lists:flatten(['implemented-in::erlang'] ++ [TagsCalls] ++ [TagsExports] ++ [TagsApp]))}
	catch 
    	throw:Term -> Term;
    	exit:Reason -> {error, Reason} ;
    	error:Reason -> {error,{Reason,erlang:get_stacktrace()}}		
	end.

%%-------------------------------------------------------------------------
%% @doc Return tags from informations not coming from Abstract Code
%% @end
%%-------------------------------------------------------------------------
-spec implemented_in(boolean(), list()) -> atom() | [].

implemented_in(true, Path) -> 
		% Get all files extensions
		AllExts = filelib:fold_files(Path, ".*", true, fun(X, AccIn) -> AccIn ++ [list_to_atom(filename:extension(X))] end, []),
		Exts = lists:usort(lists:flatten(AllExts)),
			
		%%******************************************************************************
		%% Facet: implemented-in
		%% Description: Implemented in
		%%  What language the software is implemented in
		
		% Tag: implemented-in::c
		% Description: C
		% Joker : .c
		C_Ext = ['.c'],
		C = case lists:partition(fun(X) -> lists:member(X, C_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['implemented-in::c'] 
			end,

		% Tag: implemented-in::c++
		% Description: C++
		%  GNU C++ uses: .C, .cc, .cxx, .cpp, .c++
		%  Digital Mars uses: .cpp, .cxx
		%  Borland C++ uses: .cpp
		%  Watcom uses: .cpp
		%  Microsoft Visual C++ uses: .cpp, .cxx, .cc
		%  Metrowerks CodeWarrior uses: .cpp, .cp, .cc, .cxx, .c++
		% Joker : .C, .cc, .CC, .cp, .CP, .cxx, .CXX, .cpp, .CPP, .c++, .C++
		Cpp_Ext = ['.C', '.cc', '.CC', '.cp', '.CP', '.cxx', '.CXX', '.cpp', '.CPP', '.c++', '.C++'],
		Cpp = case lists:partition(fun(X) -> lists:member(X, Cpp_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['implemented-in::c++'] 
			end,

		% Tag: implemented-in::erlang
		% Description: Erlang
		% NOTE : by default if valid Erlang project

		% Tag: implemented-in::java
		% Description: Java
		% Joker : .dpj, .DPJ, .java, .JAVA, .jar, .JAR
		J_Ext = ['.dpj', '.DPJ', '.java', '.JAVA', '.jar', '.JAR'],
		Java = case lists:partition(fun(X) -> lists:member(X, J_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['implemented-in::java']
			   end,
        % Result
		C ++ Cpp ++ Java;

implemented_in(false, _) -> [].


