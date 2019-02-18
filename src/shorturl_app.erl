%%%-------------------------------------------------------------------
%% @doc shorturl public API
%% @end
%%%-------------------------------------------------------------------

-module(shorturl_app).

-behaviour(application).

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
  binary_to_list(BinUrl);

retrieve_url(_Default) -> badtoken.

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

generate_token(_Default) -> badinputstring.

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


