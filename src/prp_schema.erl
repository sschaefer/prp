-module(prp_schema).

-export([init_tables/0,
	 fill_with_dummies/0]).
-export([paper_exists/1,
	 read_paper/1]).

-include("prp_datatypes.hrl").

%% Schema
init_tables() ->
    mnesia:create_table(paper, [{attributes, record_info(fields, paper)}]).

%% Storage API
paper_exists(Id) when is_list(Id) ->
    paper_exists(list_to_integer(Id));

paper_exists(Id) when is_binary(Id) ->
    paper_exists(binary_to_list(Id));

paper_exists(Id) ->
    Read = fun() -> mnesia:read(paper, Id) end,		   
    case mnesia:transaction(Read) of
	{atomic, []} ->
	    io:format("~p, Id: ~p Exists: ~p~n", [?LINE, Id, false]),
	    false;
	{atomic, [{paper, _, _}]} ->
	    io:format("~p, Id: ~p Exists: ~p~n", [?LINE, Id, true]),
	    true
    end.

read_paper(Id) when is_list(Id) ->
    read_paper(list_to_integer(Id));
read_paper(Id) when is_binary(Id) ->
    read_paper(binary_to_list(Id));
read_paper(Id) ->
    Read = fun() -> mnesia:read(paper, Id) end,
    case mnesia:transaction(Read) of
	{atomic, []} ->
	    {error, not_exists};
	{atomic, [Paper]} ->
	    {ok, Paper}
    end.

%% Fixtures
fill_with_dummies() ->
    Fill = mnesia:transaction(fun() ->
	       mnesia:write({paper, 1, <<"1">>}),
	       mnesia:write({paper, 2, <<"2">>}),
	       mnesia:write({paper, 3, <<"3">>}) end),
   
    case Fill of
	{atomic, ok} ->
	     ok
    end.

				     

	 
