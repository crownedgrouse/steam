%%-------------------------------------------------------------------------
%% @doc Special transcoding for erlang.mk semantic
%% @end
%%-------------------------------------------------------------------------
tags('scope::suite', 'erlang-mk') -> 'erlang::otp:suite' ;
tags('implemented-in::c', 'erlang-mk') -> 'erlang::driver:c' ;
tags('implemented-in::c++', 'erlang-mk') -> 'erlang::driver:c++' ;
tags('role::shared-lib', 'erlang-mk') -> 'erlang::library' ;

tags('interface::daemon', 'erlang-mk') -> 
	% depending geas type
	{type, Type} = lists:keyfind(type, 1, get(steam_infos)),
	case Type of
		'otp' -> 'erlang::otp:application' ;
		'app' -> 'erlang::application' ;
		_     -> []
	end;

tags('interface::commandline', 'erlang-mk') -> 
	% depending geas type
	{type, Type} = lists:keyfind(type, 1, get(steam_infos)),
	case Type of
		'esc' -> 'erlang::otp:escript' ;
		_     -> []
	end;

tags('role::plugin', 'erlang-mk') -> 
	% Check what kind of plugin(s) 
	% plugins.mk present
	Emk = [],
	{name, Name} = lists:keyfind(name, 1, get(steam_infos)),
	Exp = get(geas_exports),
	% rebar 2 : function/2 = module name
	R2 = case lists:keyfind(Name, 2, Exp) of
			  {Name, Name, 2} -> ['plugin::rebar:2'] ;
			  _ -> []
		 end,
    % rebar 3 : behaviour(provider) = init/1, do/1, format_error/1
	R3 = case lists:member({Name, init, 1}, Exp) and 
			  lists:member({Name, do, 1}, Exp) and 
			  lists:member({Name, format_error, 1}, Exp) of
			  true  -> ['plugin::rebar:3'] ;
			  false -> []
	     end,
	[Emk ++ R2 ++ R3];

% Default
tags(Atom, 'erlang-mk')
     when is_atom(Atom) -> Atom;

tags(Path, Mode) 
	 when is_list(Path),
		  is_atom(Mode) -> % Call usual tagging
						   case tags(Path) of
								{error, R}     -> {error, R} ;
								{ok, Debtags0} -> Debtags = case (filelib:is_regular(filename:join(Path, "erlang.mk")) and 
			  											          filelib:is_regular(filename:join(Path, "plugins.mk"))) of
			  															true  -> ['plugin::erlang-mk'] ++ lists:delete('role::plugin', Debtags0);
			  															false -> Debtags0 
		    									 			end, 
												Transco = lists:flatmap(fun(X) -> [tags(X, Mode)] end, Debtags),
												Rels = lists:flatmap(fun(X) -> [list_to_atom("erlang::version:" ++ X)] end, geas:w2l(geas:compat(Path, global))),
												{ok, lists:usort(lists:flatten(Transco ++ Rels))}
						   end.

