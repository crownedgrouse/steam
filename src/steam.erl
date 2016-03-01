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
%%% CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER IULTING FROM
%%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
%%% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% Created : 2016-02-21
%%%-------------------------------------------------------------------
-module(steam).
-author("Eric Pailleau <steam@crownedgrouse.com>").

-export([facets/0, facets/1, tags/0, tags/1]).

-include("steam_use.hrl").
-include("steam_db.hrl").

-define(EXIST(A, L), lists:member(A, L)).

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
		% Clean up
		put(geas_calls, []), 
		put(geas_exports, []), 
        % Get infos
		{ok, I} = geas:info(Path),
        % Parent calls
		PC = get(geas_calls),
        % Parent exports
		PE = get(geas_exports),
        % Search inheritance
		_Comp = geas:compat(Path, global),
        % All calls
		AC = get(geas_calls),
        % All exports
		AE = get(geas_exports),
		% Deps only call
		DC = AC -- PC,
		% Deps only exports
		DE = AE -- PE,
		% Extract some needed informations
		{name, RawName} = lists:keyfind(name, 1, I),
		Name = case RawName  of
					undefined -> list_to_atom(filename:basename(Path)) ;
					_  -> RawName
			   end,
		{driver, _Driver} = lists:keyfind(driver, 1, I),

		{type, Type} = lists:keyfind(type, 1, I),

		% Search tags from Parent function calls
		TPC = lists:flatmap(fun(X) -> [tag({call, Name, X})] end, PC),

		% Search tags from Parent function exports
		TPE = lists:flatmap(fun(X) -> [tag({export, Name, X})] end, PE),

		% Search tags from deps function calls
		TDC = lists:flatmap(fun(X) -> [tag({call, deps, X})] end, DC),

		% Search tags from deps function exports
		TDE = lists:flatmap(fun(X) -> [tag({export, deps, X})] end, DE),

        % Search tags from other geas information
        TA = [tag({application, Name, []}),
				   tag({application, Name, {type, Type}}),
				   implemented_in(Path),
				   use(Name)
		     ],		
		% Unique Iults
		{ok, lists:usort(hooks(lists:flatten([TPC] ++ [TPE] ++ [TDC] ++ [TDE] ++ [TA]), I, {PE, PC}, {DE, DC}))}
	catch 
    	throw:Term -> Term;
    	exit:Reason -> {error, Reason} ;
    	error:Reason -> {error,{Reason,erlang:get_stacktrace()}}		
	end.

%%-------------------------------------------------------------------------
%% @doc Return tags from informations not coming from Abstract Code
%% @end
%%-------------------------------------------------------------------------
-spec implemented_in( list()) -> list().

implemented_in(Path) -> 
		% Get all files extensions
		AllExts = filelib:fold_files(Path, "[A-Za-z0-9].*", true, fun(X, AccIn) -> AccIn ++ [list_to_atom(filename:extension(X))] end, []),
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
		E_Ext = ['.erl', '.hrl'],
		E = case lists:partition(fun(X) -> lists:member(X, E_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['implemented-in::erlang'] 
			end,

		% Tag: implemented-in::java
		% Description: Java
		% Joker : .dpj, .DPJ, .java, .JAVA, .jar, .JAR
		J_Ext = ['.dpj', '.DPJ', '.java', '.JAVA', '.jar', '.JAR'],
		Java = case lists:partition(fun(X) -> lists:member(X, J_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['implemented-in::java']
			   end,
		%%******************************************************************************
		%% Facet: works-with-format
		%% Description: Supports Format
		%%  Which data formats are supported by the package

		% Tag: works-with-format::json
		% Description: JSON
		%  JavaScript Object Notation

		Json_Ext = ['.json'],
		Json = case lists:partition(fun(X) -> lists:member(X, Json_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['works-with-format::json']
			   end,

		% Tag: works-with-format::xml
		% Description: XML
        XML_Ext = ['.xml'],
		XML = case lists:partition(fun(X) -> lists:member(X, XML_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['works-with-format::xml']
			   end,
		% Tag: works-with-format::xml:xslt
		% Description: XSL Transformations (XSLT)
        XSL_Ext = ['.xsl'],
		XSL = case lists:partition(fun(X) -> lists:member(X, XSL_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['works-with-format::xml:xslt']
			   end,
		% Tag: works-with-format::zip
		% Description: Zip Archives
        Zip_Ext = ['.zip'],
		Zip = case lists:partition(fun(X) -> lists:member(X, Zip_Ext) end, Exts) of
				 {[], _} -> [] ;
				 {_, _}  -> ['works-with-format::zip']
			   end,
        % Result
		C ++ Cpp ++ E ++ Java ++ Json ++ XML ++ XSL ++ Zip.

%%-------------------------------------------------------------------------
%% @doc Hooks on first Results
%% @end
%%-------------------------------------------------------------------------
hooks(T, I, {_PE, PC}, {DE, _DC}) ->  
		   	% Hook : detect client/server when both found
		   	NC = ?EXIST('network::client', T),
		   	NS = ?EXIST('network::server', T),
		   	% If both network::client and network::server, 
		   	% try to discriminate otherwise remove both instead giving wrong infos		
			% If there is 'connect' in parent calls and in deps exports, it is a client
			Client  = (lists:keymember(connect, 2, PC) and lists:keymember(connect, 2, DE)),
			% If there is 'listen'  in parent calls and in deps exports, it is a server
			Server  = (lists:keymember(listen, 2, PC) and lists:keymember(listen, 2, DE)),

	   		{description, Desc} = lists:keyfind(description, 1, I),
			D= case Desc of 
				 undefined ->  [] ;	
				 _ -> Tokens = string:tokens(Desc," "),
		    		  Toks = lists:flatmap(fun(X) -> [string:to_lower(X)] end, Tokens),
		    		  case lists:member("http", Toks) of
							true -> ['protocol::http'] ;
							false -> []
		       			 end
			   end,
		   	H= case (NC and NS) of	
				true -> 
						case (Client and Server) of
							true -> T ;
							false -> case Client of
										true  -> % remove Server
												 T -- ['network::server'] ;
										false -> % remove Client
												 T -- ['network::client'] 
									 end
						end;
				false -> T
		      end,
		   H ++ D.


