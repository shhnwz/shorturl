%%%-------------------------------------------------------------------
%% @doc shorturl public API
%% @end
%%%-------------------------------------------------------------------

-module(shorturl_app).

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").
%% Application callbacks
-export([start/2, stop/1, generate_token/1, retrieve_url/1]).

-define(BUCKET_TYPE, <<"crdt_counter_type">>).
-define(BUCKET_NAME,<<"sequence_bucket">>).
-define(TOKEN_BUCKET_TYPE, <<"default">>).
-define(TOKEN_BUCKET_NAME,<<"token_bucket">>).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                     {'_',
                                      [{"/shorturl/generate_token", req_handler, []},
                                       {"/shorturl/redirect", req_handler, []}
                                   ]}]),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               [{port, 8080}],
                               #{env => #{dispatch => Dispatch}}
  ),
    shorturl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  cowboy_app:stop(my_http_listener),
    ok.
%%====================================================================
%% Internal functions
%%====================================================================

%%============================================
%% This function will take token in base64 as
%% input and returns Value mapped against it
%% as output.
%%==========================================
retrieve_url(Base64Tok) when is_list(Base64Tok) or is_binary(Base64Tok)->
  TokStr = case is_binary(Base64Tok) of
             true -> binary_to_list(Base64Tok);
             false -> Base64Tok
           end,
  Tok = base64:decode(TokStr),
  %% Riak client connect
  {ok,RiakPid} = riakc_pb_socket:start(localhost,8087,[keepalive]),
  %% Fetching Key Value from non_crdt_bucket
  BinUrl = case riakc_pb_socket:get(
    RiakPid,
    {?TOKEN_BUCKET_TYPE,?TOKEN_BUCKET_NAME},
    Tok
  ) of
    {ok, RObj} -> Value = riakc_obj:get_value(RObj),
      Value;
    {error,_} -> notfound
  end,
  %% Riak client stop
  riakc_pb_socket:stop(RiakPid),
  %% return longurlstring
  case BinUrl of
    notfound -> notfound;
    _Otherwise ->
      binary_to_list(BinUrl)
  end;

retrieve_url(_Default) -> badtoken.

generate_token([]) -> emptyinput;

generate_token(URL) when is_binary(URL) ->
 generate_token(binary_to_list(URL));

generate_token(UrlString) when is_list(UrlString) or is_binary(UrlString)->
  BinUrlString = case is_binary(UrlString) of
                   true -> UrlString;
                   false ->
                     list_to_binary(UrlString)
                 end,
  MD5EncodedBinaryString =  erlang:md5(BinUrlString),
  io:fwrite("MD5 = ~p~n", [MD5EncodedBinaryString]),
  <<PrefixBits:48/bitstring, Rem/binary>> = MD5EncodedBinaryString,
  Sequence_Number = binary:encode_unsigned(generate_seq_number(PrefixBits)),
  io:fwrite("Seq = ~p~n", [Sequence_Number]),
  Token = <<PrefixBits/binary, Sequence_Number/binary>>,
  io:fwrite("Token = ~p~n",[Token]),
  {ok,RiakPid} = riakc_pb_socket:start(localhost,8087,[keepalive]),
  RO = riakc_obj:new(
    {?TOKEN_BUCKET_TYPE,?TOKEN_BUCKET_NAME},
    Token,
    BinUrlString
  ),
  riakc_pb_socket:put(RiakPid,RO),
  riakc_pb_socket:stop(RiakPid),
  base64:encode(Token);

generate_token(_Default) -> badinput.

generate_seq_number(Key) ->
  {ok,RiakPid} = riakc_pb_socket:start(localhost,8087,[keepalive]),
  CounterValue = case riakc_pb_socket:fetch_type(
    RiakPid,
    {?BUCKET_TYPE,?BUCKET_NAME},
    Key
  ) of
    {ok, RCounterObj} -> io:fwrite("Counter Present~n"),
      UpdatedCounter = riakc_counter:increment(1,RCounterObj),
      riakc_pb_socket:update_type(
        RiakPid,
        {?BUCKET_TYPE,?BUCKET_NAME},
        Key,
        riakc_counter:to_op(UpdatedCounter)
      ),
      riakc_counter:value(UpdatedCounter);

    {error,{notfound,counter}} -> io:fwrite("notfound~n"),
      RCounterObj = riakc_counter:new(),
      RCounterObj1 = riakc_counter:increment(1,RCounterObj),
      riakc_pb_socket:update_type(
        RiakPid,
        {?BUCKET_TYPE,?BUCKET_NAME},
        Key,
        riakc_counter:to_op(RCounterObj1)
      ),
      riakc_counter:value(RCounterObj1)
  end,
  riakc_pb_socket:stop(RiakPid),
  CounterValue.


%%%%%%%%%%%Unit Tests%%%%%%%%%%%%%%%%

generate_token_bad_input_test() ->
  ReturnValue = generate_token(iambadurl),
  ?assertEqual(badinput,ReturnValue).

generate_token_empty_input_test() ->
  Token1 = generate_token(<<"">>),
  Token2 = generate_token(""),
  ?assertEqual(emptyinput,Token1),
  ?assertEqual(emptyinput,Token2).

generate_token_list_input_test() ->
  ReturnValue = generate_token("abcd"),
  ?assertNotEqual(badinput, ReturnValue).

generate_token_binary_input_test() ->
  ReturnValue = generate_token(<<"abcd">>),
  ?assertNotEqual(badinput, ReturnValue).

generate_token_duplicate_input_test() ->
  Token1 = generate_token("mytokentest"),
  Token2 = generate_token("mytokentest"),
  ?assertNotEqual(Token1,Token2).

generate_token_255_input_test() ->
  Str255_1 = <<"yorK5s5xBKn9WcGvCLBzuLScHyPExoOuPFbpttBYh2Z9HBdzc3Yibkn70Vqxwxfqf7uQZ52vtig0s8OVx6j8fo9jVMQaY00Q1KepskrKCfuSZwkSZCQkQamcgI31KemRE331ZM5v62ez4DIWULqhTB8jR01Jd0WWYh2IWteqg1dyuuFujFAUZDLzfnt3nolI9X6d3kcdUw031BK4elSQ817UJ4Zt7hquNgEHiKVR7zkcZEr6cdpF8bZehEiOysG">>,
  Str255_2 = <<"LFnS3mbHpHJYSaw29soGtLtziiBoRzN7jSpD4yj6kkFznJjYUb3grLRHAqvf4u7atxsHQOlIe7bRn35WT93ngPp3jlgboVsqkI2TU4PAQlz1id4SfTtxlcoWdfI3nA0l9GjNEDv92D5xQLtI4tucZFKkiorQ7GE1GFrxvvSFHx7uySKtUk1Dwt6BNAzgs9ayfsX0rnCUO4i6GibFfmrhQ1vQXh9ZsYVOKlmqPPaGEkWNtTvScXueBebC3iqCcGT">>,
  Str255_3 = "31VzF5rRlQQqoL3EZoZEOMwcwMipE9PbGKRYphuBWDgGiNRQE6ebVrEVBZa0f72pgmzvs8ImBMh8mWozedovHcdU3zB01XsaaQF8gLfFN5tLGsdP4t6zYxpu5beEQ8f7d0u9cxIrjm5uOFcttSKiKzyo2jKMhV11oiNdKZKoMe9HvacRLo0SyjcpC1Z2Y7SFToYi5E9VqYyiuQaL11KWPnFdkW1d30VVFKWjHMVbPcZKcQJP9zctmMst4SK1sFE",
  Str255_4 = "f9oZj5dxgVhGqDtXefzuMFYBqQVP58aj3Nz68kkiEEMGRR84wnMVJJYrDwaQ9cS7PTxiv6bcVhd264fN13nHzfjBDn9cpOFKkP6ZXkvXFmNjENFFedLTRkoVQg12Xx9DS8LwWTXNhgHj7oMyIJWhxgpPj0vKYrW4kZ0yMmD2NxNIbXe4jt1mpBzWklvzpD7vsShyafdIZWaquhYBmqrXm6e3CohhFPCrDVc3uf2u6MbWWW9lE30DLI1R2ImNB9H",
  Token1  = generate_token(Str255_1),
  Token2 = generate_token(Str255_2),
  Token3 = generate_token(Str255_3),
  Token4 = generate_token(Str255_4),
  ?assertEqual(Str255_1, list_to_binary(retrieve_url(Token1))),
  ?assertEqual(Str255_2, list_to_binary(retrieve_url(Token2))),
  ?assertEqual(Str255_3, retrieve_url(Token3)),
  ?assertEqual(Str255_4, retrieve_url(Token4)).

generate_token_2083_input_test() ->
  StrMaxURL = <<"UJwMHqyf9HgNczK40UhCt5adXGHGcKr6SNS9u4ByD9q2=hGVCZc9uwTZiBJJL3MY1UniQlEzovbZzC1GNYpNrW?N8BHyBCXSiDAoKMxyhANPOMCqWu=f8fBo6GgxqiiTabJ31532B?PdhH5S10zSVFBFPV46dmlREAvu4E2GCjRMrUQYhIGJOkcEjt4l&6gUAgCww5gL=pWf:tH@:bDtMr3l4Yf4mRj3J;qeYs87Q1AoaROvUW3F&MY/&T4kNCwv?9qz0PDM@pcvFVLHG5hz5rl85lW:frQUob4daP:4PAxAlXn3KxQOQZdrZS82&1b14RpCYZKqrN=hQcwWtC/dJS9MGam?22KVLMv=qJdHoTWmmg6qC51hAW8ywP6=fy&zUZ30n/gNIifiCn1dpsAoaXawVKOwOqS?@L=&X?F2A&EqxthWeGESulvJnrxj7Zg=acKT6vBf2&Se@Y0bPMzuPfthC0pf2UnXfw?U2HMiWW0ytlZXs8nIAZTe55of08QNJH@TgVtZPcbg56ymKYUuOPwcjZ0;MUdCQMtX3N/sn9WPtpUT05cg=/tGUw3LSIFm4fC:U3v6t0Y0nyc=UL4QlCUO6APoEy5QcGixaXz97By:&/iPbagCA@H8O37RbHrvGBTDHmm7?:EluvVcX&Rs1XtdPi@Cw?g@eBHA=&rU75toIi7hl:5DcoeDMU1gZML?Vi5/Dim=nOlZGhxsdZ24qLSX8ARCSti@?qECZ4aU;kA=lllkOlE9f=IsKv:7 sEait4g=W8r3mr@lQ6HOhgUDv4an@Vw5KzMqREJ85qOTE;3ClLkm:XXZBHGUCl=oyfQf1SEgFWGRLE80wz0W0NwlUkBs Kq:z3xE/YX54p4I85qkCxqTLDgiR2lI55u13JZc914Rz5At=re68nvI7 Z;::l2YMAvCgeoY cuSH=d?fadLbWj@8@2lvSqqn@XOuS4auE3FPLl6G/OFobAHCmXB90@oRY0F5WqUO3LWD:QoOyHDs6SGgimLkqN/y?Hqq=5dI4kRP:1xXoopnRH65@dw1nD&a2hM8KcOHecVVYEPP0JKj=9&LuUDroi7iOT=DK/4hT9@3IHwXzZkm2j7tRACJ6HNLQTNFsvTr:elLuNODX7ghqBlHiX5hBV@6J:YlgqXgDi&w&Zyp;80DXHAGRrJklyqU7eflP4g&@gzZLE4IJ4:POd&ue5Hc&vp1AH7@MyMK?hrQBcJ?51VCHz8YYAQUWRTrPgIJH1A:Ll09/CxBg0vUQ/ZEE@Rz6Td104xY8y5Fx8szGkxjX68X5xAIg3WMd3I:g7AunmcatBfsy7lU6mds=ou?ki9iEll pcDUvM1WCkNz63cwmZlGRL:qmV3nw1j2SyIgTDYQih37t0DsEO7UFPudmKcDFGnqTj3HkIeTaPEhADmb2jbi3&r2MbdI=Grf?7hJRibbDhUwJO?c@IaiVB:aP8MUldth7UiVll29B:bh2=Fs2&3jD&FlvDAm7YyiB?UNrCIU1v6QQWK?5g/CjmV47gY2naEd16nKK 9I5SVqfbD:skJ8n5yeFBnBfdGe=RM3jlvg?b44Mx8GfmiA7ofL6kK;PYVW@PFe3EV==bPNEb:6GLqDsu;cZ4ZMFCjK?8saUMoMTKIEeUHyRY&Xs:n58GNU8wrUOu&UQF7xLhJsG?KTe@jcPmVn@br9irMxgG9dy6nq3k&/8@b8vj2A0EZs@S6p4&?kwdFaAu/w;t/@BnU8ICVdy0Dt@5b/&y7FHbz@Xo8/8 SGzmh:kCdne/KRFvQM4MoB:t&atb9qmEt3fMRM1xjivSU9g:gukN:W1bPL7q3vXoz=44dg1aafk4XrBQz3 WRINHU4vBFsfYR7O:Gtxv4eFIA=?oF=0&soBrh8Lp;ubHSuRx32RZNYnj1f;1Oy:BIg7N&so1l/fpfWp7t kf8w D@VD/kUKWPd/TmOGI/aoDgxTFUBzdwfZ5Z@pu14nk=gXlX8ms7haqyDVSokpu1&J?VYc5diK7qI :=M&OnVzd2mfKlUMVWkh9GYXZDfNV=MxRqim2AEkTLhtaUi15VnjO3nVBJl8jq">>,
  ReturnValue = generate_token(StrMaxURL),
  ?assertNotEqual(badinput,ReturnValue),
  ?assertEqual(StrMaxURL, list_to_binary(retrieve_url(ReturnValue))).


retrieve_value_bad_input_test() ->
  ReturnValue = retrieve_url(badtokendata),
  ?assertEqual(badtoken,ReturnValue).

retrieve_value_success_test() ->
  Url = "www.shorturl.com",
  Token = generate_token(Url),
  ?assertEqual(Url,retrieve_url(Token)).



