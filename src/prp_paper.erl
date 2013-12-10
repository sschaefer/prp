-module(prp_paper).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
    prp_schema:init_tables(),
    prp_schema:fill_with_dummies(),
    {ok, Req, undefined_state}.

%% Cheat: assume paper content is the same as id.
%% Real code must look up paper content via prp_schema.
handle(Req, State) ->
    {Content, Req1} = cowboy_req:binding(id, Req),
    {ok, Req2} = cowboy_req:reply(200, [
	{<<"content-type">>, <<"text/html">>}
    ], to_html(Content), Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

to_html(Text) ->
    <<"<html><body>",Text/binary,"</body></html>">>.

    
