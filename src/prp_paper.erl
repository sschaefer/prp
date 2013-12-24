-module(prp_paper).

-export([init/3]).
-export([resource_exists/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([provide_html/2,
	 provide_plain/2,
	 provide_json/2]).
-export([content_types_accepted/2]).
-export([put_accept_json/2,
	 post_accept_json/2]).
-export([delete_resource/2]).

init(_Transport, _Req, []) ->
    prp_schema:init_tables(),
    prp_schema:fill_with_dummies(),
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>, <<"PUT">>, <<"DELETE">>
     ], Req, State}.
     
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, provide_html},
      {<<"text/plain">>, provide_plain},
      {<<"application/json">>, provide_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    case cowboy_req:method(Req) of
	{<<"PUT">>, Req1} ->
	    {[{<<"application/json">>, put_accept_json}], Req1, State};
	{<<"POST">>, Req1} ->
	    {[{<<"application/json">>, post_accept_json}], Req1, State}
    end.

resource_exists(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {prp_schema:paper_exists(Id), Req1, State}.

provide_html(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Title}} = prp_schema:read_paper(Id),
    Body = <<"<html><body>", Title/binary, "</body></html>">>,
    {Body, Req1, State}.

provide_plain(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Body}} = prp_schema:read_paper(Id),
    {Body, Req1, State}.

provide_json(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Title}} = prp_schema:read_paper(Id),
    Body = jsx:encode([{<<"id">>, Id}, {<<"title">>, Title}]),
    {Body, Req1, State}.

put_accept_json(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    case cowboy_req:body(Req1) of
	{ok, <<"title=", Title/binary>>, Req2} ->
	    case prp_schema:paper_exists(Id) of
		true ->
		    ok = prp_schema:update_paper(Id, Title),
		    {true, Req2, State};
		false ->
		    ok = prp_schema:create_paper(Id, Title),
		    Body = jsx:encode([{<<"id">>, Id}, {<<"title">>, Title}]),
		    Req3 = cowboy_req:set_resp_body(Body, Req2),
		    {{true, <<"/paper/", Id/binary>>}, Req3, State}
	    end;
	{_, _, Req2} ->
	    {ok, Req3} = cowboy_req:reply(400, Req2),
	    {halt, Req3, State}
    end.

post_accept_json(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    UseId = case Id of
		undefined ->
		    list_to_binary(integer_to_list(prp_schema:new_id()));
		_ ->
		    case prp_schema:paper_exists(Id) of
			true ->
			    list_to_binary(integer_to_list(prp_schema:new_id()));
			false ->
			    Id
			end
	    end,
    case cowboy_req:body(Req1) of
	{ok, <<"title=", Title/binary>>, Req2} ->
	    ok = prp_schema:create_paper(UseId, Title),
	    Body = jsx:encode([{<<"id">>, UseId}, {<<"title">>, Title}]),
	    Req3 = cowboy_req:set_resp_body(Body, Req2),
	    Req4 = cowboy_req:set_resp_header(<<"location">>, <<"/paper/", UseId/binary>>, Req3),
	    {ok, Req5} = cowboy_req:reply(201, Req4),
	    {halt, Req5, State};
	{_, _, Req2} ->
	    {ok, Req3} = cowboy_req:reply(400, Req2),
	    {halt, Req3, State}
    end.

delete_resource(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    ok = prp_schema:delete_paper(Id),
    {true, Req1, State}.
