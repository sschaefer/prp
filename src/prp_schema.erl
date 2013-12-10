-module(prp_schema).

-export([init_tables/0,
	 fill_with_dummies/0]).

-include("prp_datatypes.hrl").

%% Schema
init_tables() ->
    mnesia:create_table(paper, [{attributes, record_info(fields, paper)}]).

%% Fixtures
fill_with_dummies() ->
    Fill = mnesia:transaction(fun() ->
	       mnesia:write({paper, 1, "1"}),
	       mnesia:write({paper, 2, "2"}),
	       mnesia:write({paper, 3, "3"}) end),
   
    case Fill of
	{atomic, ok} ->
	     ok
    end.

				     

	 
