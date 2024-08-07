%%%-------------------------------------------------------------------
%% @doc Header Generator for AWS Signature Version 4 (Unsigned Payload)
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_headers).
-export([generate/4, generate/6, generate/7]).
-define(PAYLOAD_HASH, <<"UNSIGNED-PAYLOAD">>).

%%====================================================================
%% @doc Header Generator
%%====================================================================
generate(HttpVerb, CanonicalUri, Scope, ExtraHeaders) ->
  Region = application:get_env(erlaws3, default_region, <<>>),
  Bucket = application:get_env(erlaws3, default_bucket, <<>>),
  generate(erlaws3_utils:bucket_url(Bucket), HttpVerb, CanonicalUri, <<>>, Region, Scope, ExtraHeaders).

generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope) ->
  generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope, []).

generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope, ExtraHeaders) ->
  Timestamp = erlaws3_utils:get_timestamp(),
  [Date, _] = binary:split(Timestamp, <<"T">>),
  AccessKey = application:get_env(erlaws3, access_key, <<>>),
  Credential = <<AccessKey/binary, "/", Date/binary, "/", AwsRegion/binary, "/", Scope/binary, "/aws4_request">>,
  Headers = lists:sort([{<<"host">>, Host}, {<<"x-amz-date">>, Timestamp} | ExtraHeaders]),
  SignedHeaders = lists:foldl(fun({Key, _}, Acc) ->
    if Acc == <<>> ->
      Key;
    true ->
      <<Acc/binary, ";", Key/binary>>
    end
  end, <<>>, Headers),
  Signature = generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Date, Timestamp),
  Authorization = <<"AWS4-HMAC-SHA256 ",
    "Credential=", Credential/binary, ", ",
    "SignedHeaders=", SignedHeaders/binary, ", ",
    "Signature=", Signature/binary>>,
  [
    {<<"Authorization">>, Authorization},
    {<<"x-amz-content-sha256">>, ?PAYLOAD_HASH},
    {<<"x-amz-date">>, Timestamp}
  | ExtraHeaders].

%%====================================================================
%% @doc Signature
%%====================================================================
generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Date, Timestamp) ->
  CanonicalRequest = generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers),
  StringToSign = generate_string_to_sign(Date, Timestamp, AwsRegion, Scope, CanonicalRequest),
  SigningKey = generate_signing_key(Date, AwsRegion, Scope),
  erlaws3_utils:sha256_to_hex(crypto:mac(hmac, sha256, SigningKey, StringToSign)).

%%====================================================================
%% @doc Canonical Request
%%====================================================================
generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers) ->
  Headers1 = lists:sort(Headers),
  <<_:1/binary, CanonicalHeaders/binary>> =
    lists:foldl(fun({N, V}, Acc) ->
      V2 = if is_integer(V) ->
        integer_to_binary(V);
      true ->
        string:trim(V)
      end,
      <<Acc/binary, "\n",
      (string:lowercase(N))/binary, ":", V2/binary>>
    end, <<>>, Headers1),
  <<_:1/binary, SignedHeaders/binary>> =
    lists:foldl(fun({N, _V}, Acc) ->
      <<Acc/binary, ";", (string:lowercase(N))/binary>>
    end, <<>>, Headers1),
  CanonicalRequest = <<HttpVerb/binary, "\n",
                      CanonicalUri/binary, "\n",
                      CanonicalQueryString/binary, "\n",
                      CanonicalHeaders/binary, "\n\n",
                      SignedHeaders/binary, "\n",
                      (?PAYLOAD_HASH)/binary>>,

  erlaws3_utils:sha256_to_hex(crypto:hash(sha256, CanonicalRequest)).

%%====================================================================
%% @doc String To Sign
%%====================================================================
generate_string_to_sign(Date, Timestamp, AwsRegion, Scope, CanonicalRequest) ->
  <<"AWS4-HMAC-SHA256\n",
    Timestamp/binary, "\n",
    Date/binary, "/", AwsRegion/binary, "/", Scope/binary, "/aws4_request\n",
    CanonicalRequest/binary>>.

%%====================================================================
%% @doc Signing Key
%%====================================================================
generate_signing_key(Date, AwsRegion, Scope) ->
  SecretKey = application:get_env(erlaws3, secret_key, <<>>),
  DateKey = crypto:mac(hmac, sha256, <<"AWS4", SecretKey/binary>>, Date),
  RegionKey = crypto:mac(hmac, sha256, DateKey, AwsRegion),
  ScopeKey = crypto:mac(hmac, sha256, RegionKey, Scope),
  crypto:mac(hmac, sha256, ScopeKey, <<"aws4_request">>).