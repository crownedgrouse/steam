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

-include("steam_db.hrl").

-export([facets/0, tags/0, tags/1]).

%%-------------------------------------------------------------------------
%% @doc Return list of handled tags
%% @end
%%-------------------------------------------------------------------------

tags() -> lists:usort(?Tags).

%%-------------------------------------------------------------------------
%% @doc Return lists of handled facets
%% @end
%%-------------------------------------------------------------------------

facets() -> lists:usort(?Facets).

%%-------------------------------------------------------------------------
%% @doc Return tags found in a Erlang application root path
%% @end
%%-------------------------------------------------------------------------

tags(_Path) -> ok.

