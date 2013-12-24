-module(prp_schema).

-export([init_tables/0,
	 fill_with_dummies/0]).
-export([paper_exists/1,
	 create_paper/2,
	 update_paper/2,
	 read_paper/1,
	 delete_paper/1,
	 new_id/0
	]).

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

create_paper(Id, Title) when is_list(Id) ->
    create_paper(list_to_integer(Id), Title);
create_paper(Id, Title) when is_binary(Id) ->
    create_paper(binary_to_list(Id), Title);
create_paper(Id, Title) ->
    io:format("~p Paper created: ~p, ~p~n", [?LINE, Id, Title]),
    check_high_id(Id),
    Write = fun() -> 
	mnesia:write({paper, Id, Title}) end,
    case mnesia:transaction(Write) of
	{atomic, ok} -> ok
    end.

check_high_id(Id) when is_list(Id) ->
    check_high_id(list_to_integer(Id));
check_high_id(Id) when is_binary(Id) ->
    check_high_id(binary_to_list(Id));
check_high_id(Id) ->
    CheckId = fun() -> 
	R = mnesia:read({paper, "high id"}),
	case R of
	    [] -> mnesia:write({paper, "high id", Id});
	    [{paper, "high id", OldHighId}] when OldHighId < Id -> mnesia:write({paper, "high id", Id});
	    _ -> ok
	end
    end,
    case mnesia:transaction(CheckId) of
	{atomic, ok} -> ok;
	Result ->
	    io:format("check_high_id failed, tansaction result ~p~n", [Result])
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

update_paper(Id, Title) ->
    io:format("~p Updating by overwrite: ~p, ~p~n", [?LINE, Id, Title]),
    create_paper(Id, Title).
    
delete_paper(Id) when is_list(Id) ->
    delete_paper(list_to_integer(Id));
delete_paper(Id) when is_binary(Id) ->
    delete_paper(binary_to_list(Id));
delete_paper(Id) ->
    Delete = fun() -> mnesia:delete({paper, Id}) end,
    case mnesia:transaction(Delete) of
	{atomic, ok} ->
	    ok
    end.

new_id() ->
    NewHighId = fun() ->
	case mnesia:read(paper, "high id") of
	    [] ->
		mnesia:write({paper, "high id", 0}),
		0;
	    [{paper, "high id", OldHighId}] ->
		ok = mnesia:write({paper, "high id", OldHighId + 1}),
		OldHighId + 1
	end
    end,
    case mnesia:transaction(NewHighId) of
	{atomic, Id} -> Id
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

				     

	 
