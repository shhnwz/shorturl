%%%-------------------------------------------------------------------
%%% @author mohd
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2019 8:14 PM
%%%-------------------------------------------------------------------
-module(req_handler).
-author("mohd").

%% API
-export([init/2]).

init(Req0 = #{method := <<"POST">>}, Opts) ->
  Method = cowboy_req:method(Req0),
  {ok,Body,ReqMap} = cowboy_req:read_body(Req0),
  io:fwrite("Method = ~p~n",[Method]),
  io:fwrite("Body = ~p~n",[Body]),
  Token = shorturl_app:generate_token(Body),
  io:fwrite("Token = ~p~n",[Token]),
  Req = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
  }, Token, Req0),
  {ok, Req, Opts};

init(Req0 = #{method := <<"GET">>}, Opts) ->
  Method = cowboy_req:method(Req0),
  TokenStr = case cowboy_req:has_body(Req0) of
               true ->
                 {ok,Body,ReqMap} = cowboy_req:read_body(Req0),
                 Body;
               false ->
                 {ok,Body,ReqMap} = cowboy_req:read_body(Req0),
                 maps:get(qs,ReqMap, undefined)
             end,
  io:fwrite("Method = ~p~n",[Method]),
  io:fwrite("TokenStr = ~p~n",[TokenStr]),
  io:fwrite("ReqMap = ~p~n",[ReqMap]),
  URL = shorturl_app:retrieve_url(TokenStr),
  io:fwrite("URL = ~p~n", [URL]),
   Req = cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain">>
  }, URL, Req0),
  {ok, Req, Opts}.


