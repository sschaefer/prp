-module(prp_paper).

-export([init/3]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([use_html/2,
	 use_plain/2,
	 use_json/2]).

init(_Transport, _Req, []) ->
    prp_schema:init_tables(),
    prp_schema:fill_with_dummies(),
    {upgrade, protocol, cowboy_rest}.

%% Cheat: assume paper content is the same as id.
%% Real code must look up paper content via prp_schema.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, use_html},
      {<<"text/plain">>, use_plain},
      {<<"application/json">>, use_json}
     ], Req, State}.

resource_exists(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {prp_schema:paper_exists(Id), Req1, State}.

use_html(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Content}} = prp_schema:read_paper(Id),
    Body = <<"<html><body>", Content/binary, "</body></html>">>,
    {Body, Req1, State}.

use_plain(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Body}} = prp_schema:read_paper(Id),
    {Body, Req1, State}.

use_json(Req, State) ->
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, {paper, _Id, Content}} = prp_schema:read_paper(Id),
    Body = jsx:encode([{<<"rest">>, Content}]),
    {Body, Req1, State}.
